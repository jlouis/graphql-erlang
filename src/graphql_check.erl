%%% @doc Type checking of GraphQL query documents
%%%
%%% The type checker carries out three tasks:
%%%
%%% Make sure that types check. That is, the user supplies a well
%%% typed query document.
%%%
%%% Make sure that types are properly inferred. Some times, the types
%%% the user supply needs an inference pass in order to figure out
%%% what the user supplied. The type-checker also infers the possible
%%% types and makes sure the query document is well typed.
%%%
%%% Handle coercion for the constant fragment of a query document.
%%% Whenever a coercible constant value is encountered in a query
%%% document, or a coercible parameter occurs in a parameter, the type
%%% checker runs "input coercion" which is part canonicalization, part
%%% input validation on input data. The coerced value is expanded into
%%% the query document or parameter string, so the execution engine
%%% always works with coerced data. This choice is somewhat peculiar,
%%% but it serves an optimization purpose since we only have to carry
%%% out a coercion once for a query with constant values.
%%%
%%% Polarity:
%%%
%%% This type checker mentions polarity of types. There are 3 kinds of
%%% polarity: positive, negative and non-polar. The flows of these are
%%% that Client -> Server is positive and Server -> Client is
%%% negative. Non-polar types flows both ways. Since the server engine
%%% doesn't trust the client, type checking follows some polarity
%%% rules. If we check a positive polarity context, we don't trust the
%%% client and we use the schema data to verify that everything is
%%% covered by the client in a valid way. If we check in negative
%%% polarity context, we are the server and can trust things are
%%% correct. So we fold over the query document when considering if
%%% types are correct. Non-polar values fall naturally in both
%%% contexts.
%%%
%%% Algorithm:
%%%
%%% We use a bidirectional type checker. In general we handle two kinds of
%%% typing constructs: `G |- e ==> t' (inference) and `G |- e <== t, e' (checking)
%%% The first of these gets G,e as inputs and derives a t. The second form
%%% gets G, e, and t as inputs and derives e' which is an e annotated with
%%% more information.
%%%
%%% By having these two forms, the type checking algorithm can switch between
%%% elaboration and lookup of data and checking that the types are correct.
%%% The type checker can thus handle a query in one checking pass over the
%%% structure rather than having to rely on two.
%%% @end
-module(graphql_check).

-include_lib("graphql/include/graphql.hrl").
-include("graphql_internal.hrl").
-include("graphql_schema.hrl").

-export([check/2, check_params/4]).
-export([funenv/2]).

-record(ctx,
        {
         endpoint_ctx :: endpoint_context(),
         path = [] :: [any()],
         vars = #{} :: #{ binary() => #vardef{} },
         frags = #{} :: #{ binary() =>  #frag{} },

         %% Current subcontext we are checking under. We are either
         %% running in the "query context" of an input query to the system
         %% or in the "variable context" of variables given as a JSON structure.
         %%
         %% The latter has different handling rules w.r.t, enumerated types
         sub_context = query :: query | variable
        }).
-type ctx() :: #ctx{}.
-type polarity() :: '+' | '-' | '*'.

-type ty() :: schema_type() | schema_object().
-type ty_name() :: binary().

%% This is a bidirectional type checker. It proceeds by running three
%% kinds of functions: synth(Gamma, E) -> {ok, T} | {error, Reason}
%% which synthesizes a given type out of its constituent parts.
%% check(Gamma, E, T) -> ok | {error, Reason} which checks that a
%% given term E has type T and sub(S, T) which forms a relation S <: T
%% of subsumption between types.

%% -- INFERENCE ------------------------------------------------------------
%%
%%

%% Elaborate a type and also determine its polarity. This is used for
%% input and output types
-spec infer_type(ctx(), ty_name() | ty()) -> {ok, {polarity(), ty()}}.
infer_type(Ctx = #ctx{endpoint_ctx = Ep}, Tau) ->
    case infer_type_(Ep, Tau) of
        {error, Reason} ->
            err(Ctx, Reason);
        {Polarity, TauPrime} ->
            {ok, {Polarity, TauPrime}}
    end.

-spec infer_type_(endpoint_context(), ty_name() | ty()) -> {polarity(), ty()} | {error, Reason :: term()}.
infer_type_(Ep, {non_null, Ty}) ->
    case infer_type_(Ep, Ty) of
        {error, Reason} -> {error, Reason};
        {Polarity, V} -> {Polarity, {non_null, V}}
    end;
infer_type_(Ep, {list, Ty}) ->
    case infer_type_(Ep, Ty) of
        {error, Reason} -> {error, Reason};
        {Polarity, V} -> {Polarity, {list, V}}
    end;
infer_type_(Ep, {scalar, Name}) ->
    #scalar_type{} = Ty = graphql_schema:get(Ep, Name),
    {_polarity, Ty} = infer_type_(Ep, Ty);
%% NonPolar
infer_type_(_Ep, #scalar_type{} = Ty) -> {'*', Ty};
infer_type_(_Ep, {enum, _} = E) -> {'*', E};
infer_type_(_Ep, #enum_type{} = Ty) -> {'*', Ty};
%% Positive
infer_type_(_Ep, #input_object_type{} = Ty) -> {'+', Ty};
%% Negative
infer_type_(_Ep, #object_type{} = Ty) -> {'-', Ty};
infer_type_(_Ep, #interface_type{} = Ty) -> {'-', Ty};
infer_type_(_Ep, #union_type{} = Ty) -> {'-', Ty};
%% Lookup
infer_type_(Ep, {name, _, N}) -> infer_type_(Ep, N);
infer_type_(Ep, N) when is_binary(N) ->
    case graphql_schema:lookup(Ep, N) of
        not_found -> {error, {not_found, N}};
        %% Non-polar types
        #enum_type{} = Enum -> {'*', Enum};
        #scalar_type{} = Scalar -> {'*', Scalar};

        %% Positive types
        #input_object_type{} = IOType -> {'+', IOType};

        %% Negative types
        #object_type{} = OT -> {'-', OT};
        #interface_type{} = IFace -> {'-', IFace};
        #union_type{} = Union -> {'-', Union}
    end.

%% Infer a type and assert it is valid in input context
infer_input_type(Ctx, Ty) ->
    case infer_type(Ctx, Ty) of
        {ok, {'*', Tau}} -> {ok, Tau};
        {ok, {'+', Tau}} -> {ok, Tau};
        {ok, {'-', _}} -> err(Ctx, {invalid_input_type, Ty})
    end.

%% Infer a type and assert it is valid in output context
infer_output_type(Ctx, Ty) ->
    case infer_type(Ctx, Ty) of
        {ok, {'*', Tau}} -> {ok, Tau};
        {ok, {'-', Tau}} -> {ok, Tau};
        {ok, {'+', _}} -> err(Ctx, {invalid_output_type, Ty})
    end.



%% Main inference judgement
%%
%% Given a context and some graphql expression, we derive
%% a valid type for that expression. This is mostly handled by
%% a lookup into the environment.
infer(Ctx = #ctx{ endpoint_ctx = Ep}, #directive { id = ID }) ->
    Name = graphql_ast:name(ID),
    case graphql_schema:lookup(Ep, Name) of
        #directive_type{} = Tau -> {ok, Tau};
        not_found -> err(Ctx, {unknown_directive, Name})
    end;
infer(Ctx = #ctx{endpoint_ctx = Ep}, #op { ty = Ty } = Op) ->
    CtxP = add_path(Ctx, Op),
    case graphql_schema:lookup(Ep, 'ROOT') of
        not_found ->
            err(Ctx, no_root_schema);
        Schema ->
            Root = graphql_schema:resolve_root_type(Ty, Schema),
            case graphql_schema:lookup(Ep, Root) of
                not_found ->
                    err(CtxP, {type_not_found, Root});
                #object_type{} = Tau ->
                    {ok, Tau}
            end
    end;
infer(Ctx, #frag { ty = Ty } = F) ->
    infer_output_type(add_path(Ctx, F), Ty);
infer(#ctx { frags = FragEnv } = Ctx, #frag_spread { id = ID }) ->
    Name = graphql_ast:name(ID),
    case maps:get(Name, FragEnv, not_found) of
        not_found ->
            CtxP = add_path(Ctx, Name),
            err(CtxP, unknown_fragment);
        #frag{} = Frag ->
            {ok, Frag}
    end;
infer(#ctx { vars = Vars } = Ctx, {var, ID}) ->
    Var = graphql_ast:name(ID),
    case maps:get(Var, Vars, not_found) of
        not_found ->
            err(Ctx, {unbound_variable, Var});
        #vardef {} = VDef ->
            {ok, VDef}
    end;
infer(Ctx, X) ->
    exit({not_implemented, Ctx, X}).

infer_field(Ctx, #field { id = ID } = F, FieldTypes) ->
    CtxP = add_path(Ctx, F),
    Name = graphql_ast:name(ID),
    case maps:get(Name, FieldTypes, not_found) of
        not_found when Name == <<"__typename">> ->
            {ok, {introspection, typename}};
        not_found ->
            err(CtxP, unknown_field);
        #schema_field{} = Ty ->
            {ok, Ty}
    end.


%% -- TYPE CHECKING --------------------------------------------------------
%%
%%

%% Check arguments. Follows the general scheme of checking for uniqueness and
%% then check each argument.
check_args(Ctx, Args, Ty) ->
    %% Check uniqueness
    NamedArgs = [{graphql_ast:name(K), V} || {K, V} <- Args],
    case graphql_ast:uniq(NamedArgs) of
        ok ->
            ArgTys = maps:to_list(Ty),
            check_args_(Ctx, NamedArgs, ArgTys, []);
        {not_unique, X} ->
            err(Ctx, {not_unique, X})
    end.

%% Meat of the argument checker:
%%
%% Since arguments have positive polarity, they are checked according
%% to the schema arguments. We don't trust the client here as it can
%% omit args and it can put in the wrong values for args as well
%%
%% The meat of the argument checker. Walk over each schema arg and
%% verify it type checks according to the type checking rules.
check_args_(_Ctx, [], [], Acc) ->
    {ok, Acc};
check_args_(Ctx, [_|_] = Args, [], _Acc) ->
    err(Ctx, {excess_args, Args});
check_args_(Ctx, Args, [{N, #schema_arg { ty = ArgTy,
                                          default = Default }}| Next], Acc) ->
    CtxP = add_path(Ctx, N),
    {ok, Sigma} = infer_input_type(Ctx, ArgTy),

    case lists:keytake(N, 1, Args) of
        {value, {_, null}, _} ->
            %% You are currently not allowed to input null values
            err(CtxP, {null_input, N});
        {value, {_, Val}, RemainingArgs} ->
            %% Found argument with value Val
            Res = case check_value(CtxP, Val, Sigma) of
                      {ok, #var{} = Var} ->
                          {N, #{ type => Sigma,
                                 value => Var#var { default = Default }}};
                      {ok, RVal} ->
                          {N, #{ type => Sigma,
                                 value => RVal}}
                  end,
            check_args_(Ctx, RemainingArgs, Next, [Res|Acc]);
        false ->
            case {Sigma, Default} of
                {{non_null, _}, undefined} ->
                    err(Ctx, missing_non_null_param);
                {{non_null, _}, null} ->
                    err(Ctx, missing_non_null_param);
                _ ->
                    {ok, Coerced} = coerce_default_param(CtxP, Default, Sigma),
                    Res = {N, #{ type => Sigma,
                                 value => Coerced }},
                    check_args_(Ctx, Args, Next, [Res|Acc])
            end
    end.

check_directive(Ctx, Context, #directive{ args = Args, id = ID} = D,
                #directive_type { args = SArgs, locations = Locations } = Ty) ->
    CtxP = add_path(Ctx, D),
    case lists:member(Context, Locations) of
        true ->
            {ok, CArgs} = check_args(CtxP, Args, SArgs),
            {ok, D#directive { args = CArgs, schema = Ty }};
        false ->
            Name = graphql_ast:name(ID),
            err(Ctx, {invalid_directive_location, Name, Context})
    end.

%% @todo this might be an inference
check_directives(Ctx, OpType, Dirs) ->
     NamedDirectives = [{graphql_ast:name(ID), D}
                        || #directive { id = ID } = D <- Dirs],
     case graphql_ast:uniq(NamedDirectives) of
         ok ->
             {ok, [begin
                       {ok, Ty} = infer(Ctx, D),
                       {ok, CDir} = check_directive(Ctx, OpType, D, Ty),
                       CDir
                   end || D <- Dirs]};
         {not_unique, X} ->
             err(Ctx, {directives_not_unique, X})
     end.

%% Check values against a type:
%%
%% Judge a type and a value. Used to verify a type judgement of the
%% form 'G |- v <= T,e'' for a value 'v' and a type 'T'. Analysis has shown that
%% it is most efficient to make the case analysis follow 'v' over 'T'.
check_value(Ctx, Val, Ty) when is_binary(Ty) ->
    {ok, Sigma} = infer_input_type(Ctx, Ty),
    check_value(Ctx, Val, Sigma);
check_value(Ctx, {name, _, N}, Sigma) ->
    check_value(Ctx, N, Sigma);
check_value(Ctx, {var, ID}, Sigma) ->
    CtxP = add_path(Ctx, {var, ID}),
    {ok, #vardef { ty = Tau}} = infer(Ctx, {var, ID}),
    ok = sub_input(CtxP, Tau, Sigma),
    {ok, #var { id = ID, ty = Tau }};
check_value(Ctx, undefined, {non_null, _} = Sigma) ->
    err(Ctx, {type_mismatch,
              #{ document => undefined,
                 schema => Sigma }});
check_value(Ctx, null, {non_null, _} = Sigma) ->
    err(Ctx, {type_mismatch,
              #{ document => null,
                 schema => Sigma }});
check_value(Ctx, Val, {non_null, Sigma}) ->
    check_value(Ctx, Val, Sigma);
check_value(_Ctx, undefined, _Sigma) ->
    %% Values not given are currently defaulted to the value null
    %% @todo: Lift this curse
    {ok, null};
check_value(_Ctx, null, _Sigma) ->
    %% Null values are accepted in every other context
    {ok, null};
check_value(Ctx, Vals, {list, Sigma}) when is_list(Vals) ->
    {ok, [begin
              %% TODO: Fold and keep an iterator
              {ok, R} = check_value(Ctx, V, Sigma),
              R
          end || V <- Vals]};
check_value(Ctx, Val, {list, Sigma}) ->
    %% The Jun2018 specification says that a singleton value
    %% should be treated as if it were wrapped in a singleton type
    %% if we are in list-context
    check_value(Ctx, [Val], {list, Sigma});
check_value(Ctx = #ctx{endpoint_ctx = Ep}, {enum, N}, #enum_type { id = ID } = Sigma) ->
    case graphql_schema:validate_enum(Ep, ID, N) of
        not_found ->
            err(Ctx, {unknown_enum, N});
        ok ->
            coerce(Ctx, N, Sigma);
        {other_enums, Others} ->
            err(Ctx, {type_mismatch,
                       #{ document => Others,
                          schema => Sigma }})
    end;
check_value(Ctx, Obj, #input_object_type{} = Tau) when is_map(Obj) ->
    %% When an object comes in through JSON for example, then the input object
    %% will be a map which is already unique in its fields. To handle this, turn
    %% the object into the same form as the one we use on query documents and pass
    %% it on. Note that the code will create a map later on once the input has been
    %% uniqueness-checked.
    check_value(Ctx, {input_object, maps:to_list(Obj)}, Tau);
check_value(Ctx, {input_object, _} = InputObj, Sigma) ->
    case Sigma of
        #input_object_type{} ->
            check_input_obj(Ctx, InputObj, Sigma);
        _OtherType ->
            err(Ctx, {type_mismatch,
                      #{ document => InputObj,
                         schema => Sigma }})
    end;
check_value(Ctx, Val, #scalar_type{} = Sigma) ->
    coerce(Ctx, Val, Sigma);
check_value(#ctx { sub_context = query } = Ctx, String, #enum_type{}) when is_binary(String) ->
    %% The spec (Jun2018, section 3.9 - Input Coercion) says that this
    %% is not allowed, unless given as a parameter. In this case, it
    %% is not given as a parameter, but is expanded in as a string in
    %% a query document. Reject.
    err(Ctx, enum_string_literal);
check_value(#ctx { sub_context = variable } = Ctx, String, #enum_type{} = Tau) when is_binary(String) ->
    %% In the case of a sub context for variables, we are allowed to handle the case
    check_value(Ctx, {enum, String}, Tau);
check_value(Ctx, Val, #enum_type{} = Sigma) ->
    coerce(Ctx, Val, Sigma);
check_value(Ctx, Val, Sigma) ->
    err(Ctx, {type_mismatch,
              #{ document => Val,
                 schmema => Sigma }}).

check_input_obj(Ctx, {input_object, Obj},
                #input_object_type{ fields = Fields }) ->
    AssocList = [{coerce_name(K), V} || {K, V} <- Obj],
    case graphql_ast:uniq(AssocList) of
        {not_unique, Key} ->
            err(Ctx, {input_object_not_unique, Key});
        ok ->
            {ok,
             check_input_obj_(Ctx, maps:from_list(AssocList),
                              maps:to_list(Fields), #{})}
    end.

%% Input objects are in positive polarity, so the schema's fields are used
%% to verify that every field is present, and that there are no excess fields
%% As we process fields in the object, we remove them so we can check that
%% there are no more fields in the end.
check_input_obj_(Ctx, Obj, [], Acc) ->
    case maps:size(Obj) of
        0 -> Acc;
        K when K > 0 -> err(Ctx, {excess_fields_in_object, Obj})
    end;
check_input_obj_(Ctx, Obj, [{Name, #schema_arg { ty = Ty,
                                                 default = Default }} | Next],
                 Acc) ->
    CtxP = add_path(Ctx, Name),
    {ok, Result} =
        case maps:get(Name, Obj, not_found) of
            not_found ->
                case check_not_found(CtxP, Ty, Default) of
                    undefined ->
                        coerce_default_param(CtxP, null, Ty);
                    default ->
                        coerce_default_param(CtxP, Default, Ty)
                end;
            V ->
                {ok, Tau} = infer_input_type(CtxP, Ty),
                case check_value(CtxP, V, Tau) of
                    {ok, #var{} = Var} ->
                        {ok, Coerced} = coerce_default_param(CtxP, Default, Ty),
                        {ok, Var#var { default = Coerced }};
                    {ok, Res} ->
                        {ok, Res}
                end
        end,
    check_input_obj_(Ctx,
                     maps:remove(Name, Obj),
                     Next,
                     Acc#{ Name => Result }).

check_sset(Ctx, [], Ty) ->
    case Ty of
        #object_type{} -> err(Ctx, fieldless_object);
        #interface_type{} -> err(Ctx, fieldless_interface);
        _ -> {ok, []}
    end;
check_sset(Ctx, [_|_], #scalar_type{}) ->
    err(Ctx, selection_on_scalar);
check_sset(Ctx, [_|_], #enum_type{}) ->
    err(Ctx, selection_on_enum);
check_sset(Ctx, SSet, Ty) ->
    check_sset_(Ctx, SSet, Ty).

check_sset_(_Ctx, [], _Ty) ->
    {ok, []};
check_sset_(Ctx, [#frag { id = '...', ty = undefined } = Frag | Fs], Sigma) ->
    {ok, Rest} = check_sset_(Ctx, Fs, Sigma),
    {ok, CFrag} = check(Ctx, Frag, Sigma),
    {ok, [CFrag | Rest]};
check_sset_(Ctx, [#frag { id = '...' } = Frag | Fs], Sigma) ->
    {ok, Rest} = check_sset_(Ctx, Fs, Sigma),
    {ok, Tau} = infer(Ctx, Frag),
    {ok, CFrag} = check(Ctx, Frag, Tau),
    ok = sub_output(Ctx, Tau, Sigma),
    {ok, [CFrag | Rest]};
check_sset_(Ctx, [#frag_spread { directives = Dirs } = FragSpread | Fs], Sigma) ->
    {ok, Rest} = check_sset_(Ctx, Fs, Sigma),
    CtxP = add_path(Ctx, FragSpread),
    {ok, #frag { schema = Tau }} = infer(Ctx, FragSpread),
    ok = sub_output(CtxP, Tau, Sigma),
    {ok, CDirectives} = check_directives(CtxP, 'FRAGMENT_SPREAD', Dirs),
    {ok, [FragSpread#frag_spread { directives = CDirectives } | Rest]};
check_sset_(Ctx, [#field{} = F|Fs], {non_null, Ty}) ->
    check_sset_(Ctx, [F|Fs], Ty);
check_sset_(Ctx, [#field{} = F|Fs], {list, Ty}) ->
    check_sset_(Ctx, [F|Fs], Ty);
check_sset_(Ctx, [#field{ args = Args, directives = Dirs,
                          selection_set = SSet } = F | Fs], Sigma) ->
    {ok, Rest} = check_sset_(Ctx, Fs, Sigma),
    CtxP = add_path(Ctx, F),
    {ok, CDirectives} = check_directives(CtxP, 'FIELD', Dirs),
    {ok, FieldTypes} = fields(CtxP, Sigma),
    case infer_field(Ctx, F, FieldTypes) of
        {ok, {introspection, typename} = Ty} ->
            {ok, [F#field { schema = Ty,
                            directives = CDirectives }
                  |Rest]};
        {ok, #schema_field { ty = Ty, args = TArgs } = SF} ->
            {ok, Tau} = infer_output_type(Ctx, Ty),
            {ok, CSSet} = check_sset(CtxP, SSet, Tau),
            {ok, CArgs} = check_args(CtxP, Args, TArgs),
            {ok, [F#field {
                    args = CArgs,
                    schema = SF#schema_field { ty = Tau },
                    directives = CDirectives,
                    selection_set = CSSet }
                  | Rest]}
    end.

%% Main check relation:
%%
%% Given a Context of environments
%% An expression to check
%% A type to check it against
%%
%% We derive an expression in which we have annotated types into
%% the AST. This helps the later execution stage.
check(Ctx, #frag { ty = undefined } = Frag, Sigma) ->
    %% The specification has a rule in which if you omit the
    %% type of a fragment, it "picks up" the type of the context
    %% because this can be used in the case where you want to include
    %% or slip a block of information
    check(Ctx, Frag#frag { ty = Sigma }, Sigma);
check(Ctx, #frag { directives = Dirs,
                   selection_set = SSet } = F, Sigma) ->
    CtxP = add_path(Ctx, F),
    {ok, Tau} = infer(Ctx, F),
    ok = sub_output(CtxP, Tau, Sigma),
    {ok, CDirectives} = check_directives(CtxP, 'INLINE_FRAGMENT', Dirs),
    {ok, CSSet} = check_sset(CtxP, SSet, Tau),
    {ok, F#frag { schema = Tau,
                  directives = CDirectives,
                  selection_set = CSSet }};
check(Ctx, #op { vardefs = VDefs, directives = Dirs, selection_set = SSet } = Op,
           #object_type {} = Sigma) ->
    CtxP = add_path(Ctx, Op),
    DirectiveLocation = directive_location(Op),
    {ok, VarDefs} = var_defs(CtxP, VDefs),
    {ok, CDirectives} = check_directives(CtxP, DirectiveLocation, Dirs),
    {ok, CSSet} = check_sset(CtxP#ctx { vars = VarDefs }, SSet, Sigma),
    {ok, Op#op {
           schema = Sigma,
           directives = CDirectives,
           selection_set = CSSet,
           vardefs = VarDefs}}.

%% To check a document, establish a default context and
%% check the document.
check(Ep, #document{} = Doc) ->
    try check_(Ep, Doc) of Res -> Res
    catch throw:{error, Path, Msg} ->
            graphql_err:abort(Path, type_check, Msg)
    end.

check_(Ep, #document{ definitions = Defs } = Doc) ->
    Fragments = lists:filter(
                  fun(#frag{}) -> true; (_) -> false end,
                  Defs),
    FragEnv = fragenv(Ep, Fragments),
    Ctx = #ctx{endpoint_ctx = Ep, frags = FragEnv},
    COps = [begin
                {ok, Type} = infer(Ctx, Op),
                {ok, COp} = check(Ctx, Op, Type),
                COp
            end || Op <- Defs],
    {ok, #{
           ast => Doc#document { definitions = COps },
           fun_env => funenv(Ep, COps) }}.


%% GraphQL queries are really given in two stages. One stage is the
%% query document containing (static) queries which take parameters.
%% These queries can be seen as functions (stored procedures) you can
%% call.
%%
%% If called, we get a function name, and a set of parameters for that
%% function. So we have to go through the concrete parameters and type
%% check them against the function environment type schema. If the
%% input parameters can not be coerced into the parameters expected by
%% the function scheme, and error occurs.

%% This is the entry-point when checking parameters for an already parsed,
%% type checked and internalized query. It serves to verify that a requested
%% operation and its parameters matches the types in the operation referenced
check_params(Ep, FunEnv, OpName, Params) ->
    try
        case operation(FunEnv, OpName, Params) of
            undefined -> #{};
            not_found ->
                err(#ctx{endpoint_ctx = Ep},
                    {operation_not_found, OpName});
            VarEnv ->
                Ctx = #ctx { endpoint_ctx = Ep,
                             vars = VarEnv,
                             path = [OpName],
                             sub_context = variable },
                check_params_(Ctx, Params)
        end
    catch throw:{error, Path, Msg} ->
            graphql_err:abort(Path, type_check, Msg)
    end.


%% Parameter checking has positive polarity, so we fold over
%% the type var environment from the schema and verify that each
%% type is valid.
check_params_(#ctx { vars = VE } = Ctx, OrigParams) ->
    F = fun
            (Key, #vardef { ty = Tau, default = Default}, Parameters) ->
                CtxP = add_path(Ctx, Key),
                case maps:get(Key, Parameters, not_found) of
                    not_found ->
                        case check_not_found(CtxP, Tau, Default) of
                            undefined ->
                                Parameters;
                            default ->
                                {ok, Res} = coerce_default_param(CtxP, Default, Tau),
                                Parameters#{ Key => Res }
                        end;
                    Value ->
                        {ok, Res} = check_value(CtxP, Value, Tau),
                        Parameters#{ Key => Res }
                end
        end,
    maps:fold(F, OrigParams, VE).

%% Handle the case where the parameter isn't found in the system
%% In this case, we handle nullability through this matching rule set
check_not_found(Ctx, {non_null, _}, null) ->
    err(Ctx, missing_non_null_param);
check_not_found(Ctx, {non_null, _}, undefined) ->
    err(Ctx, missing_non_null_param);
check_not_found(_Ctx, _Tau, undefined) ->
    undefined;
check_not_found(_Ctx, _Tau, _Default) ->
    default.

%% -- SUBTYPE/SUBSUMPTION ------------------------------------------------------
%%
%%

%% Subsumption relation over input types:
%%
%% Decide if an input type is an valid subsumption of another type. We assume
%% that the first parameter is the 'Tau' type and the second parameter
%% is the 'Sigma' type.
%%
%% Some of the cases are reflexivity. Some of the cases are congruences.
%% And some are special handling explicitly.
%%

sub_input(Ctx, Tau, Sigma) ->
    case sub_input_(Tau, Sigma) of
        yes ->
            ok;
        no ->
            err(Ctx, {type_mismatch,
                      #{ document => Tau,
                         schema => Sigma }})
    end.

sub_input_(#scalar_type { id = ID }, #scalar_type { id = ID }) -> yes;
sub_input_(#enum_type { id = ID }, #enum_type { id = ID }) -> yes;
sub_input_(#input_object_type { id = ID }, #input_object_type { id = ID }) -> yes;
sub_input_({non_null, Tau}, {non_null, Sigma}) ->
    sub_input_(Tau, Sigma);
sub_input_({non_null, Tau}, Sigma) ->
    %% A more strict document type of non-null is always allowed since
    %% it can't be null in the schema then
    sub_input_(Tau, Sigma);
sub_input_(_Tau, {non_null, _Sigma}) ->
    %% If the schema requires a non-null type but the document doesn't
    %% supply that, it is an error
    no;
sub_input_({list, Tau}, {list, Sigma}) ->
    %% Lists are decided by means of a congruence
    sub_input_(Tau, Sigma);
sub_input_(Tau, {list, Sigma}) ->
    %% A singleton type is allowed to be embedded in a list according to the
    %% specification (Oct 2016)
    sub_input_(Tau, Sigma);
sub_input_(_Tau, _Sigma) ->
    %% Any other type combination are invalid
    no.

%% Subsumption relation over output (fragment) types
%% Decide is a fragment can be embedded in a given scope
%% We proceed by computing the valid set of the Scope and also the
%% Valid set of the fragment. The intersection type between these two,
%% Scope and Spread, must not be the empty set. Otherwise it is a failure.
%%
%% The implementation here works by case splitting the different possible
%% output types one at a time and then handling them systematically rather
%% than running an intersection computation. This trades off computation
%% for code size when you have a match that can be optimized in any way.
%%
%% First a series of congruence checks. We essentially ignore
%% The list and non-null modifiers and check if the given fragment
%% can be expanded in the given scope by recursing.
%%
%% Fragments doesn't care if they sit inside lists or if the scope
%% type is non-null:
sub_output(Ctx, Tau, {list, Sigma}) ->
    sub_output(Ctx, Tau, Sigma);
sub_output(Ctx, Tau, {non_null, Sigma}) ->
    sub_output(Ctx, Tau, Sigma);
sub_output(Ctx, {non_null, Tau}, Sigma) ->
    sub_output(Ctx, Tau, Sigma);
%% Reflexivity:
sub_output(_Ctx, #object_type { id = Ty },
                      #object_type { id = Ty }) ->
    %% Object spread in Object scope requires a perfect match
    ok;
sub_output(Ctx, #object_type { id = Tau },
                     #object_type { id = Sigma  }) ->
    %% If not a perfect match, this is an error:
    err(Ctx, {fragment_spread, Tau, Sigma});
%% An object subsumes a union scope if the object is member of
%% Said union type
sub_output(Ctx, #object_type { id = ID },
                     #union_type { id = UID,
                                   types = Sigmas }) ->
    case lists:member(ID, Sigmas) of
        true -> ok;
        false -> err(Ctx, {not_union_member, ID, UID})
    end;
%% Likewise an object is subsumed by an interface if the object
%% is member of said interface:
sub_output(Ctx, #object_type { id = ID,
                                    interfaces = IFaces },
                     #interface_type { id = IID }) ->
    case lists:member(IID, IFaces) of
        true -> ok;
        false -> err(Ctx, {not_interface_member, ID, IID})
    end;
%% Otherwise, this is an error:
sub_output(Ctx, #interface_type { id = IID },
                     #object_type { id = OID, interfaces = IFaces }) ->
    case lists:member(IID, IFaces) of
        true -> ok;
        false -> err(Ctx, {not_interface_embedder, IID, OID})
    end;
%% Interface Tau subsumes interface Sigma if they have concrete
%% objects in common. This means there is at least one valid expansion,
%% so this should be allowed.
sub_output(Ctx = #ctx{endpoint_ctx = Ep}, #interface_type { id = SpreadID },
                     #interface_type { id = ScopeID }) ->
    Taus = graphql_schema:lookup_interface_implementors(Ep, SpreadID),
    Sigmas = graphql_schema:lookup_interface_implementors(Ep, ScopeID),
    case ordsets:intersection(
           ordsets:from_list(Taus),
           ordsets:from_list(Sigmas)) of
        [_|_] ->
            ok;
        [] ->
            err(Ctx, {no_common_object, SpreadID, ScopeID})
    end;
%% Interfaces subsume unions, if the union has at least one member
%% who implements the interface.
sub_output(Ctx = #ctx{endpoint_ctx = Ep}, #interface_type { id = SpreadID },
                     #union_type{ id = ScopeID, types = ScopeMembers }) ->
    Taus = graphql_schema:lookup_interface_implementors(Ep, SpreadID),
    case ordsets:intersection(
           ordsets:from_list(Taus),
           ordsets:from_list(ScopeMembers)) of
        [_|_] ->
            ok;
        [] ->
            err(Ctx, {no_common_object, SpreadID, ScopeID})
    end;
%% Unions subsume objects if they are members
sub_output(Ctx, #union_type { id = UID, types = UMembers },
                     #object_type { id = OID }) ->
    case lists:member(OID, UMembers) of
        true -> ok;
        false -> err(Ctx, {not_union_embedder, UID, OID})
    end;
%% Unions subsume interfaces iff there is an intersection between
%% what members the union has and what the implementors of the interface
%% are.
sub_output(Ctx = #ctx{endpoint_ctx = Ep}, #union_type { id = SpreadID, types = SpreadMembers },
                     #interface_type { id = ScopeID }) ->
    Sigmas = graphql_schema:lookup_interface_implementors(Ep, ScopeID),
    case ordsets:intersection(
           ordsets:from_list(SpreadMembers),
           ordsets:from_list(Sigmas)) of
        [_|_] ->
            ok;
        [] ->
            err(Ctx, {no_common_object, SpreadID, ScopeID})
    end;
%% Unions subsume if there are common members
sub_output(Ctx, #union_type { id = SpreadID, types = SpreadMembers },
                     #union_type { id = ScopeID,  types = ScopeMembers }) ->
    case ordsets:intersection(
           ordsets:from_list(SpreadMembers),
           ordsets:from_list(ScopeMembers)) of
        [_|_] ->
            ok;
        [] ->
            err(Ctx, {no_common_object, SpreadID, ScopeID})
    end.

%% -- COERCION OF INPUTS ---------------------------------------------------
%%
%%
coerce_name(B) when is_binary(B) -> B;
coerce_name(Name) -> graphql_ast:name(Name).

%% This is a function which must go as soon as we have proper
%% type checking on the default values in the schema type checker.
%% There is absolutely no reason to do something like this then since
%% it can never fail like this.
coerce_default_param(#ctx { }, undefined, _Ty) ->
    {ok, undefined};
coerce_default_param(#ctx { path = Path } = Ctx, Default, Ty) ->
    try check_value(Ctx, Default, Ty) of
        Result -> Result
    catch
        Class:Err ->
            error_logger:error_report(
              [{path, graphql_err:path(lists:reverse(Path))},
               {default_value, Default},
               {type, graphql_err:format_ty(Ty)},
               {default_coercer_error, Class, Err}]),
            err(Ctx, non_coercible_default)
    end.

coerce(Ctx, Val, #enum_type { id = ID, resolve_module = ResolveMod }) ->
    case ResolveMod of
        undefined ->
            {ok, Val};
        Mod ->
            resolve_input(Ctx, ID, Val, Mod)
    end;
coerce(Ctx, Val, #scalar_type { id = ID, resolve_module = Mod }) ->
    true = Mod /= undefined,
    resolve_input(Ctx, ID, Val, Mod).

resolve_input(Ctx, ID, Val, Mod) ->
    try Mod:input(ID, Val) of
        {ok, NewVal} -> {ok, NewVal};
        {error, Reason} ->
            err(Ctx, {input_coercion, ID, Val, Reason})
    catch
        Cl:Err ->
            err_report({input_coercer, ID, Val}, Cl, Err),
            err(Ctx, {input_coerce_abort, {Cl, Err}})
    end.

%% -- INTERNAL FUNCTIONS ------------------------------------------------------

%% Handle a list of vardefs by elaboration of their types
var_defs(Ctx, Input) ->
    VDefs =
        [case infer_input_type(Ctx, V#vardef.ty) of
             {ok, Tau} -> V#vardef { ty = Tau }
         end || V <- Input],
    NamedVars = [{graphql_ast:name(K), V}
                 || #vardef { id = K } = V <- VDefs],
    case graphql_ast:uniq(NamedVars) of
        ok ->
            {ok, varenv(VDefs)};
        {not_unique, Var} ->
            err(add_path(Ctx, Var), {param_not_unique, Var})
    end.

%% Extract fields from a type, where the type has fields
fields(_Ctx, #object_type { fields = Fields }) -> {ok, Fields};
fields(_Ctx, #interface_type { fields = Fields }) -> {ok, Fields};
fields(_Ctx, #union_type {}) -> {ok, #{}}.

%% Build a varenv
varenv(VarList) ->
    maps:from_list(
      [{graphql_ast:name(Var), Def} || #vardef { id = Var } = Def <- VarList]).

%% Build a funenv
funenv(Ep, Ops) ->
    F = fun
        (#frag{}, FE) -> FE;
        (#op { id = ID, vardefs = VDefs }, FE) ->
            Name = graphql_ast:name(ID),
            Ctx = #ctx{endpoint_ctx = Ep},
            {ok, VarEnv} = var_defs(Ctx, maps:values(VDefs)),
            FE#{ Name => VarEnv }
    end,
    lists:foldl(F, #{}, Ops).

annotate_frag(Ep, #frag { ty = Ty } = Frag) ->
    Ctx = #ctx{endpoint_ctx = Ep},
    {ok, Tau} = infer_output_type(Ctx, Ty),
    Frag#frag { schema = Tau }.

%% Build a fragenv
fragenv(Ep, Frags) ->
    maps:from_list(
      [{graphql_ast:name(ID), annotate_frag(Ep, Frg)} || #frag { id = ID } = Frg <- Frags]).

%% Figure out what kind of directive location we are in
directive_location(#op { ty = Ty }) ->
    case Ty of
        undefined -> 'QUERY';
        {query, _} -> 'QUERY';
        {mutation, _} -> 'MUTATION';
        {subscription, _} -> 'SUBSCRIPTION'
    end.

%% Determine the operation whih the call wants to run
operation(FunEnv, <<>>, Params) ->
    %% Supplying an empty string is the same as not supplying anything at all
    %% This should solve problems where we have empty requests
    operation(FunEnv, undefined, Params);
operation(FunEnv, undefined, Params) ->
    case maps:to_list(FunEnv) of
        [] when Params == #{} ->
            undefined;
        [] when Params /= #{} ->
            err([], unnamed_operation_params);
        [{_, VarEnv}] ->
            VarEnv;
        _ ->
            %% The error here should happen in the execute phase
            undefined
    end;
operation(FunEnv, OpName, _Params) ->
    maps:get(OpName, FunEnv, not_found).

%% Tell the error logger that something is off
err_report(Term, Cl, Err) ->
  error_logger:error_report(
    [
     Term,
     {error, Cl, Err}
    ]).

%% Add a path component to the context
-spec add_path(ctx(), Component :: term()) -> ctx().
add_path(#ctx { path = P } = Ctx, C) ->
    Ctx#ctx { path = [C|P] }.

%% Report an error relative to a context
-spec err(ctx(), Term :: term()) -> no_return().
err(#ctx{ path = Path }, Msg) ->
    throw({error, Path, Msg}).
