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
%%% typing constructs: G |- e => t (inference) and G |- e <= t,e' (checking)
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

-export([check/1]).
-export([funenv/1]).

-record(ctx,
        {
         path = [] :: any(),
         vars = #{} :: #{ binary() => term() },
         frags = #{} :: #{ binary() =>  #frag{} }
        }).
-type expr() :: any().
-type ty() :: any().

%% This is a bidirectional type checker. It proceeds by running three
%% kinds of functions: synth(Gamma, E) -> {ok, T} | {error, Reason}
%% which synthesizes a given type out of its constituent parts.
%% check(Gamma, E, T) -> ok | {error, Reason} which checks that a
%% given term E has type T and sub(S, T) which forms a relation S <: T
%% of subsumption between types.

%% Elaborate a type and also determine its polarity. This is used for
%% input and output types
infer_type({non_null, Ty}) ->
    case infer_type(Ty) of
        {error, Reason} -> {error, Reason};
        {Polarity, V} -> {Polarity, {non_null, V}}
    end;
infer_type({list, Ty}) ->
    case infer_type(Ty) of
        {error, Reason} -> {error, Reason};
        {Polarity, V} -> {Polarity, {list, V}}
    end;
infer_type({scalar, Name}) ->
    #scalar_type{} = Ty = graphql_schema:get(Name),
    {_polarity, Ty} = infer_type(Ty);
infer_type(#scalar_type{} = Ty) -> {'*', Ty};
infer_type({enum, _} = E) -> {'*', E};
infer_type(#enum_type{} = Ty) -> {'*', Ty};
infer_type({name, _, N}) -> infer_type(N);
infer_type(N) when is_binary(N) ->
    case graphql_schema:lookup(N) of
        not_found -> {error, not_found};
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

%% Main inference judgement
%%
%% Given a context and some graphql expression, we derive
%% a valid type for that expression. This is mostly handled by
%% a lookup into the environment.
-spec infer(Context :: ctx(), Exp :: expr()) -> {ok, ty()}.
infer(Ctx, #directive { id = ID }) ->
    case graphql_ast:name(ID) of
        <<"include">> -> {ok, graphql_directives:include()};
        <<"skip">> -> {ok, graphql_directives:skip()};
        Name -> err(Ctx, {unknown_directive, Name})
    end;
infer(Ctx, #op { ty = Ty } = Op) ->
    CtxP = add_path(Ctx, Op),
    case graphql_schema:lookup('ROOT') of
        not_found ->
            err(Ctx, no_root_schema);
        Schema ->
            Root = graphql_schema:resolve_root_type(Ty, Schema),
            case graphql_schema:lookup(Root) of
                not_found ->
                    err(CtxP, {type_not_found, Root});
                #object_type{} = Ty ->
                    {ok, Ty}
            end
    end;
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
infer(_Ctx, _) ->
    {error, not_implemented}.

%% The infer/3 is a variant of infer where we have a context of some sort
%% from which to run the derivation of the type (list of argument types,
%% or the fields of a selection set for instance).

infer_arg(Ctx, K, ArgTypes) ->
    Name = graphql_ast:name(K),
    case maps:get(Name, ArgTypes, not_found) of
        not_found ->
            err(Ctx, {unknown_argument, Name});
        #schema_arg{ ty = Ty } ->
            input_type(Ty)
    end.

-spec infer(Context :: ctx(), Exp :: expr(), TyMap :: ty()) -> {ok, ty()}.
infer(Ctx, #field { id = ID } = F, FieldTypes) ->
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

%% To check a document, establish a default context and
%% check the document.
check(#document{ definitions = Defs } = Doc) ->
    {Fragments, Ops} = lists:partition(
                         fun(#frag{}) -> true; (_) -> false end,
                         Defs),
    FragEnv = fragenv(Fragments),
    CtxP = add_path(#ctx{}, document),
    Ctx = CtxP#ctx { frags = FragEnv },
    {ok, COps} = [begin
                      {ok, Type} = infer(Ctx, Op),
                      {ok, COp} = check(Ctx, Op, Type),
                      COp
                  end || Op <- Ops],
    {ok, Doc#document { definitions = COps }}.

%% The check/2 relation checks a expression under an environment
%% it is used whenever we don't have a type but want to have a type
%%
%% @todo: Get rid of this since it is probably better embedded in
%% the other flow.
-spec check_directives(Context :: ctx(), Exp :: expr()) -> {ok, expr()}.
check_directives(Ctx, {directives, OpType, Dirs}) ->
     NamedDirectives = [{graphql_ast:name(ID), D}
                        || #directive { id = ID } = D <- Dirs],
     case graphql_ast:uniq(NamedDirectives) of
         ok ->
             {ok, [begin
                       {ok, Ty} = infer(Ctx, D),
                       {ok, CDir} = check(Ctx, {OpType, D}, Ty),
                       CDir
                   end || D <- Dirs]};
         {not_unique, X} ->
             err(Ctx, {directives_not_unique, X})
     end.

%% Main check relations:
%%
%% Given a Context of environments
%% An expression to check
%% A type to check it against
%%
%% We derive an expression in which we have annotated types into
%% the AST. This helps the later execution stage.

%% Check arguments. Follows the general scheme
-spec check_args(Context :: ctx(), Exp :: expr(), Ty :: ty()) -> {ok, expr()}.
check_args(Ctx, Args, Ty) ->
    %% Check uniqueness
    NamedArgs = [{graphql_ast:name(K), V} || {K, V} <- Args],
    case graphql_ast:uniq(NamedArgs) of
        ok ->
            ArgTys = maps:to_list(Ty),
            check_args_(Ctx, NamedArgs, ArgTys, []);
        {not_unique, X} ->
            err(Path, {not_unique, X})
    end.

%% Meat of the argument checker:
%%
%% Since arguments have positive polarity, they are checked according
%% to the schema arguments.
%% The meat of the argument checker. Walk over each schema arg and
%% verify it type checks according to the type checking rules.
check_args_(_Ctx, [], [], Acc) ->
    {ok, Acc};
check_args_(Ctx, [_|_] = Args, [], _Acc) ->
    err(Ctx, {excess_args, Args});
check_args_(Ctx, Args, [{N, #schema_arg { ty = SigmaTy }} = SArg | Next], Acc) ->
    CtxP = add_path(Ctx, N),
    {ok, {_, #{ type := Tau, value := Val}} = A, NextArgs} =
        take_arg(CtxP, SArg, Args),
    {ok, {_Polarity, Sigma}} = infer_type(SigmaTy),
    Res = case check_value(CtxP, Val, Sigma) of
              {ok, Val} -> A;
              {ok, RVal} -> {Name, {Tau, RVal}}
          end,
    check_args_(Ctx, NextArgs, Next, [Res|Acc]).

%% Check values against a type:
%%
%% Judge a type and a value. Used to verify a type judgement of the
%% form 'G |- v <= T,e'' for a value 'v' and a type 'T'. Analysis has shown that
%% it is most efficient to make the case analysis follow 'v' over 'T'.

check_value(Ctx, {name, _, N} Sigma) ->
    check_value(Ctx, N, Sigma);
check_value(Ctx, null, {non_null, Sigma} = STy) ->
    err(Ctx, {type_mismatch,
              #{ document => Value,
                 schema => STy }});
check_value(Ctx, Val, {non_null, Sigma} = STy) ->
    check_value(Ctx, Value, Sigma);
check_value(Ctx, null, Sigma) ->
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
check_value(Ctx, {enum, N} #enum_type { id = ID } = Sigma) ->
    case graphql_schema:validate_enum(ID, N) of
        not_found ->
            err(Ctx, {unknown_enum, N});
        ok ->
            coerce(Ctx, N, Sigma);
        {other_enums, Others} ->
            err(Path, {type_mismatch,
                       #{ document => Others,
                          schema => SType }})
    end;
check_value(Ctx, {input_object, _} = InputObj, Sigma) ->
    case Sigma of
        #input_object_type{} = IOType ->
            check_input_object(Ctx, Sigma, InputObj);
        _OtherType ->
            err(Ctx, {type_mismatch,
                      #{ document => InputObj,
                         schema => Sigma }})
    end;
check_value(Ctx, Value, #scalar_type{} = Sigma) ->
    coerce(Ctx, Value, Sigma);
check_value(Ctx, String, #enum_type{}) when is_binary(String) ->
    %% The spec (Jun2018, section 3.9 - Input Coercion) says that this
    %% is not allowed, unless given as a parameter. In this case, it
    %% is not given as a parameter, but is expanded in as a string in
    %% a query document. Reject.
    err(Ctx, enum_string_literal);
check_value(Ctx, Value, #enum_type{} = Sigma) ->
    coerce(Ctx, Value, Sigma);
check_value(Ctx, Value, Sigma) ->
    err(Ctx, {type_mismatch,
              #{ document => Value,
                 schmema => Sigma }}).

check_input_obj(Ctx, {input_object, Obj},
                #input_object_type{ fields = Fields } = Tau) ->
    AssocList = [{coerce_name(K), V} || {K, V} <- Obj],
    case graphql_ast:uniq(AssocList) of
        {not_unique, Key} ->
            err(Ctx, {input_object_not_unique, Key});
        ok ->
            {ok, 
             check_input_obj_(Ctx, maps:from_list(AssocList),
                              maps:to_list(Fields))}
    end;

%% Input objects are in positive polarity, so the schema's fields are used
%% to verify that every field is present, and that there are no excess fields
%% As we process fields in the object, we remove them so we can check that
%% there are no more fields in the end.
check_input_obj_(Ctx, Obj, []) ->
    case maps:size(Obj) of
        0 -> [];
        K when K > 0 -> err(Ctx, {excess_fields_in_object, Obj})
    end;
%% @todo: Clearly this has to change because Ty isn't known at this
check_input_obj_(Ctx, Obj, [{Name, #schema_arg { ty = Ty,
                                                 default = Default }} | Next]) -> 
    Result = case maps:get(Name, Obj, not_found) of
                 not_found ->
                     case Ty of
                         {non_null, _} when Default == null ->
                             err(add_path(Ctx, Name), missing_non_null_param);
                         _ ->
                             coerce_default_param(Ctx, Default, Ty)
                     end;
                 V ->
                     check_param(add_path(Ctx, Name), V, Ty)
             end,
    [Result | check_input_obj_(Ctx, maps:remove(Name, Obj), Next)].

%% This is a function which must go as soon as we have proper
%% type checking on the default values in the schema type checker.
%% There is absolutely no reason to do something like this then since
%% it can never fail like this.
coerce_default_param(Path, VarEnv, Ty, Default) ->
    try check_param(Path, VarEnv, Ty, Default) of
        Result -> Result
    catch
        Class:Err ->
            error_logger:error_report(
              [{path, graphql_err:path(lists:reverse(Path))},
               {default_value, Default},
               {type, graphql_err:format_ty(Ty)},
               {default_coercer_error, Class, Err}]),
            err(Path, non_coercible_default)
    end.

-spec check_sset(Ctx :: ctx(),
                 Exprs :: [any()],
                 Ty :: ty()) ->
                        {ok, Result :: [term()]}.
check_sset(Ctx, [], Ty) ->
    case Ty of
        #object_type{} -> err(Ctx, fieldless_object);
        #interface_type{} -> err(Ctx, fieldless_interface);
        _ -> {ok, []}
    end;
check_sset(Ctx, [_|_], #scalar_type{}) ->
    err(Ctx, selection_on_scalar);
check(Ctx, [_|_], #enum_type{}) ->
    err(Ctx, selection_on_enum);
check_sset(Ctx, SSet, Ty) ->
    check_sset_(Ctx, SSet, Ty).

check_sset_(_Ctx, [], _Ty) ->
    {ok, []};
check_sset_(Ctx, [#frag { id = '...' } = Frag | Fs], Sigma) ->
    {ok, Rest} = check_sset_(Ctx, Fs, Sigma),
    {ok, FragTy} = infer(Ctx, Frag),
    {ok, CFrag} = check(Ctx, Frag, FragTy),
    {ok, [CFrag | Rest]};
check_sset_(Ctx, [#frag_spread { directives = Dirs } = FragSpread | Fs], Sigma) ->
    CtxP = add_path(Ctx, FragSpread),
    {ok, Rest} = check_sset_(Ctx, Fs, Sigma),
    {ok, #frag { schema = Tau }} = infer(Ctx, FragSpread),
    ok = sub_frag(CtxP, Tau, Sigma),

    {ok, CDirectives} = check_directives(CtxP, {directive, frag_spread, Dirs}),
    %% @todo: Consider just expanding #frag{} here
    {ok, [FragSpread#frag_spread { directives = CDirectives }
          | Rest]};
check_sset_(Ctx, [#field{} = F|Fs], {non_null, Ty}) ->
    check_sset_(Ctx, [F|Fs], Ty);
check_sset_(Ctx, [#field{} = F|Fs], {list, Ty}) ->
    check_sset_(Ctx, [F|Fs], Ty);
check_sset_(Ctx, [#field{}|_], not_found) ->
    exit({broken_invariant, Ctx});
check_sset_(Ctx, [#field{ args = Args, directives = Dirs,
                          selection_set = SSet } = F | Fs], Sigma) ->
    CtxP = add_path(Ctx, F),
    {ok, FieldTypes} = fields(CtxP, Sigma),
    {ok, Rest} = check_sset_(Ctx, Fs, Sigma),
    {ok, CDirectives} = check_directives(CtxP, {directive, field, Dirs}),
    case infer(Ctx, F, FieldTypes) of
        {ok, {introspection, typename} = Ty} ->
            {ok, [F#field { schema = Ty,
                            directives = CDirectives }
                  |Rest]};
        {ok, #schema_field { ty = Ty, args = TArgs } = SF} ->
            {ok, Type} = output_type(Ty),
            {ok, CSSet} = check_sset(CtxP, SSet, Type),
            {ok, CArgs} = check_args(CtxP, Args, TArgs),
            {ok, [F#field {
                    args = CArgs,
                    schema = SF#schema_field { ty = Type },
                    directives = CDirectives,
                    selection_set = CSSet }
                  | Rest]}
    end.

-spec check(Context :: ctx(), Exp :: expr(), Ty :: ty()) -> {ok, expr()}.
check(Ctx, {var, ID}, Sigma) ->
    {ok, Tau} = infer(Ctx, {var, ID}),
    case sub(Tau, Sigma) of
        yes -> {ok, {var, ID, Tau}};
        no ->
            err(Ctx, {type_mismatch,
                      #{ document => {var, ID, Tau},
                         schema => Sigma }})
    end;
check(Ctx, {Context,
            #directive{ args = Args, id = ID} = D},
      #directive_type { args = SArgs, locations = Locations } = Ty) ->
    CtxP = add_path(Ctx, D),
    case lists:member(Context, Locations) of
        true ->
            {ok, CArgs} = check_args(CtxP, Args, SArgs),
            {ok, D#directive { args = CArgs, schema = Ty }};
        false ->
            Name = graphql_ast:name(ID),
            err(Ctx, {invalid_directive_location, Name, Context})
    end;
check(Ctx, #frag { directives = Dirs,
                   selection_set = SSet } = F, Sigma) ->
    CtxP = add_path(Ctx, F),
    {ok, Fields} = fields(CtxP, Sigma),
    {ok, Tau} = infer(Ctx, F),
    ok = sub_frag(CtxP, Tau, Sigma),
    {ok, CDirectives} = check_directives(CtxP, {directive, fragment, Dirs}),
    {ok, CSSet} = check_sset(CtxP, SSet, Fields),
    {ok, F#frag { schema = Tau,
                  directives = CDirectives,
                  selection_set = CSSet }};
check(Ctx, #op { vardefs = VDefs, directives = Dirs, selection_set = SSet } = Op,
           #object_type { fields = Fields }) ->
    CtxP = add_path(Ctx, Op),
    {ok, Ty} = infer(Ctx, Op),
    OperationType = operation_context(Op),
    {ok, CDirectives} = check_directives(CtxP, {directive, OperationType, Dirs}),
    {ok, CSSet} = check_sset(CtxP, SSet, Fields),
    {ok, VarDefs} = var_defs(CtxP, {var_def, VDefs}),
    {ok, Op#op {
           schema = Ty,
           directives = CDirectives,
           selection_set = CSSet,
           vardefs = VarDefs}}.

%% Subsumption relation over types:
%%
%% Decide if a type is an valid subsumption of another type. We assume
%% that the first parameter is the 'Tau' type and the second parameter
%% is the 'Sigma' type.
%%
%% Some of the cases are reflexivity. Some of the cases are congruences.
%% And some are special handling explicitly.
%%
sub(#scalar_type { id = ID }, #scalar_type { id = ID }) -> yes;
sub(#enum_type { id = ID }, #enum_type { id = ID }) -> yes;
sub(#input_object_type { id = ID }, #input_object_type { id = ID }) -> yes;
sub({non_null, DTy}, {non_null, STy}) ->
    sub(DTy, STy);
sub({non_null, DTy}, STy) ->
    %% A more strict document type of non-null is always allowed since
    %% it can't be null in the schema then
    sub(DTy, STy);
sub(_DTy, {non_null, _STy}) ->
    %% If the schema requires a non-null type but the document doesn't
    %% supply that, it is an error
    no;
sub({list, DTy}, {list, STy}) ->
    %% Lists are decided by means of a congruence
    sub(DTy, STy);
sub(DTy, {list, STy}) ->
    %% A singleton type is allowed to be embedded in a list according to the
    %% specification (Oct 2016)
    sub(DTy, STy);
sub(_DTy, _STy) ->
    %% Any other type combination are invalid
    no.

%% Subsumption relation over fragment types
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
sub_frag(Ctx, Tau, {list, Sigma}) ->
    sub_frag(Ctx, Tau, Sigma);
sub_frag(Ctx, Tau, {non_null, Sigma}) ->
    sub_frag(Ctx, Tau, Sigma);
sub_frag(Ctx, {non_null, Tau}, Sigma) ->
    sub_frag(Ctx, Tau, Sigma);
%% Reflexivity:
sub_frag(_Ctx, #object_type { id = Ty },
                      #object_type { id = Ty }) ->
    %% Object spread in Object scope requires a perfect match
    ok;
sub_frag(Ctx, #object_type { id = Tau },
                     #object_type { id = Sigma  }) ->
    %% If not a perfect match, this is an error:
    err(Ctx, {fragment_spread, Tau, Sigma});
%% An object subsumes a union scope if the object is member of
%% Said union type
sub_frag(Ctx, #object_type { id = ID },
                     #union_type { id = UID,
                                   types = Sigmas }) ->
    case lists:member(ID, Sigmas) of
        true -> ok;
        false -> err(Ctx, {not_union_member, ID, UID})
    end;
%% Likewise an object is subsumed by an interface if the object
%% is member of said interface:
sub_frag(Ctx, #object_type { id = ID,
                                    interfaces = IFaces },
                     #interface_type { id = IID }) ->
    case lists:member(IID, IFaces) of
        true -> ok;
        false -> err(Ctx, {not_interface_member, ID, IID})
    end;
%% Otherwise, this is an error:
sub_frag(Ctx, #interface_type { id = IID },
                     #object_type { id = OID, interfaces = IFaces }) ->
    case lists:member(IID, IFaces) of
        true -> ok;
        false -> err(Ctx, {not_interface_embedder, IID, OID})
    end;
%% Interface Tau subsumes interface Sigma if they have concrete
%% objects in common. This means there is at least one valid expansion,
%% so this should be allowed.
sub_frag(Ctx, #interface_type { id = SpreadID },
                     #interface_type { id = ScopeID }) ->
    Taus = graphql_schema:lookup_interface_implementors(SpreadID),
    Sigmas = graphql_schema:lookup_interface_implementors(ScopeID),
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
sub_frag(Ctx, #interface_type { id = SpreadID },
                     #union_type{ id = ScopeID, types = ScopeMembers }) ->
    Taus = graphql_schema:lookup_interface_implementors(SpreadID),
    case ordsets:intersection(
           ordsets:from_list(Taus),
           ordsets:from_list(ScopeMembers)) of
        [_|_] ->
            ok;
        [] ->
            err(Ctx, {no_common_object, SpreadID, ScopeID})
    end;
%% Unions subsume objects if they are members
sub_frag(Ctx, #union_type { id = UID, types = UMembers },
                     #object_type { id = OID }) ->
    case lists:member(OID, UMembers) of
        true -> ok;
        false -> err(Ctx, {not_union_embedder, UID, OID})
    end;
%% Unions subsume interfaces iff there is an intersection between
%% what members the union has and what the implementors of the interface
%% are.
sub_frag(Ctx, #union_type { id = SpreadID, types = SpreadMembers },
                     #interface_type { id = ScopeID }) ->
    Sigmas = graphql_schema:lookup_interface_implementors(ScopeID),
    case ordsets:intersection(
           ordsets:from_list(SpreadMembers),
           ordsets:from_list(Sigmas)) of
        [_|_] ->
            ok;
        [] ->
            err(Ctx, {no_common_object, SpreadID, ScopeID})
    end;
%% Unions subsume if there are common members
sub_frag(Ctx, #union_type { id = SpreadID, types = SpreadMembers },
                     #union_type { id = ScopeID,  types = ScopeMembers }) ->
    case ordsets:intersection(
           ordsets:from_list(SpreadMembers),
           ordsets:from_list(ScopeMembers)) of
        [_|_] ->
            ok;
        [] ->
            err(Ctx, {no_common_object, SpreadID, ScopeID})
    end.

coerce_name(B) when is_binary(B) -> B;
coerce_name(Name) -> graphql_ast:name(Name).

coerce(Ctx, Value, #enum_type { resolve_module = ResolveMod }) ->
    case ResolveMod of
        undefined ->
            {ok, Value};
        Mod ->
            resolve_input(Ctx, ID, Value, Mod)
    end;
coerce(Ctx, Value, #scalar_type { resolve_module = Mod }) ->
    true = Mod /= undefined,
    resolve_input(Ctx, ID, Value, Mod).

resolve_input(Ctx, ID, Value, Mod) ->
    try Mod:input(ID, Value) of
        {ok, NewVal} -> {ok, NewVal};
        {error, Reason} ->
            err(Ctx, {input_coercion, ID, Value, Reason})
    catch
        Cl:Err ->
            err_report({input_coercer, ID, Value}, Cl, Err),
            err(Ctx, {input_coerce_abort, {Cl, Err}})
    end.

%% -- INTERNAL FUNCTIONS ------------------------------------------------------------

%% Assert a type is an input type
input_type(Ty) ->
    case infer_type(Ty) of
        {error, Reason} -> {error, Reason};
        {'*', V} -> {ok, V};
        {'+', V} -> {ok, V};
        {'-', _} -> {error, {invalid_input_type, Ty}}
    end.

%% Assert a type is an output type
output_type(Ty) ->
    case infer_type(Ty) of
        {error, Reason} -> {error, Reason};
        {'*', V} -> {ok, V};
        {'-', V} -> {ok, V};
        {'+', _} -> {error, {invalid_output_type, Ty}}
    end.

%% Handle a list of vardefs by elaboration of their types
var_defs(Ctx, VDefs) ->
    VDefs =
        [case input_type(V#vardef.ty) of
             {ok, Ty} -> V#vardef { ty = Ty };
             {error, not_found} -> err(Ctx, {type_not_found, graphql_ast:id(V)});
             {error, {invalid_input_type, T}} -> err(Ctx, {not_input_type, T})
         end || V <- VDefs],
    NamedVars = [{graphql_ast:name(K), V}
                 || #vardef { id = K } = V <- VDefs],
    case graphql_ast:uniq(NamedVars) of
        ok ->
            {ok, varenv(VDefs)};
        {not_unique, Var} ->
            err(add_path(Ctx, Var), {param_not_unique, Var})
    end.

%% Extract fields from a type
fields(_Ctx, #object_type { fields = Fields }) -> {ok, Fields};
fields(_Ctx, #interface_type { fields = Fields }) -> {ok, Fields};
fields(_Ctx, #union_type {}) -> {ok, #{}}.

varenv(VDefs) ->
    L = [{graphql_ast:name(Var), Def} || #vardef { id = Var } = Def <- VDefs],
    maps:from_list(L).

funenv(Ops) ->
    F = fun
        (#frag{}, FE) -> FE;
        (#op { id = ID, vardefs = VDefs }, FE) ->
            Name = graphql_ast:name(ID),
            VarEnv = varenv(VDefs),
            FE#{ Name => VarEnv }
    end,
    lists:foldl(F, #{}, Ops).

fragenv(Frags) ->
    maps:from_list(
      [{graphql_ast:name(ID), Frg} || #frag { id = ID } = Frg <- Frags]).

operation_context(#op { ty = Ty }) ->
    case Ty of
        undefined -> query;
        {query, _} -> query;
        {mutation, _} -> mutation;
        {subscription, _} -> subscription
    end.

add_path(#ctx { path = P } = Ctx, C) ->
    Ctx#ctx { path = [C|P] }.

take_arg(Ctx, {Key, #schema_arg { ty = Tau,
                                  default = Default }}, Args) ->
    case lists:keytake(Key, 1, Args) of
        {value, Arg, NextArgs} ->
            %% Argument found, use it
            {ok, Arg, NextArgs}
        false ->
            %% Argument was not given. Resolve default value if any
            case {Tau, Default} of
                {{non_null, _}, null} ->
                    err(Ctx, missing_non_null_param);
                _ ->
                    {ok, {Key, #{ type => Tau, value => Default}}, Args}
            end;
    end.

%% Tell the error logger that something is off
err_report(Term, Cl, Err) ->
  error_logger:error_report(
    [
     Term,
     {error, Cl, Err}
    ]).

err(#ctx{ path = Path }, Msg) ->
    throw({error, Path, Msg}).
