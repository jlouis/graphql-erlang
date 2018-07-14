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

-export([infer/1,
         infer_params/3]).

-record(ctx,
        {
         path = [],
         vars :: #{ binary() => term() }
        }).
-type ctx() :: #ctx{}.
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

infer(Doc) ->
    try infer(#ctx{}, Doc) of
        {ok, AST} ->
            {ok, AST};
        {error, Reason} ->
            {error, Reason}
    catch
        throw:{error, Path, Msg} ->
            graphql_err:abort(Path, type_check, Msg)
    end.

infer_params(_, _, _) ->
    {error, not_implemented}.


-spec infer(Context :: ctx(), Exp :: expr()) -> {ok, ty()}.
infer(Ctx, #directive { id = ID }) ->
    case graphql_ast:name(ID) of
        <<"include">> -> {ok, graphql_directives:include()};
        <<"skip">> -> {ok, graphql_directives:skip()};
        _Name -> err(Ctx, {unknown_directive, D})
    end;
infer(Ctx, #op {} = Op) ->
    CtxP = add_path(Ctx, Op),
    RootSchema = root(Ctx, Op),
    case graphql_schema:lookup(RootSchema) of
        not_found ->
            err(CtxP, {type_not_found, RootSchema});
        #object_type{} = Ty ->
            {ok, Ty}
    end;
infer(#ctx { vars = Vars }, {var, ID}) ->
    Var = graphql_ast:name(ID),
    case maps:get(Var, Vars, not_found) of
        not_found ->
            err(Ctx, {unbound_variable, Var});
        #vardef {} = VDef ->
            {ok, VDef}
    end;
infer(Gamma, #document{} = Doc) ->
    {error, not_implemented}.

-spec infer(Context :: ctx(),
            Exp :: expr(),
            TyMap :: ty()) ->
                   {ok, ty()}.
infer(Ctx, {arg, K}, ArgTypes) ->
    Name = graphql_ast:name(K),
    case maps:get(Name, ArgTypes, not_found) of
        not_found ->
            err(Ctx, {unknown_argument, Name});
        #schema_arg{ ty = Ty } ->
            input_type(Ty)
    end;
infer(Ctx, #field { id = ID } = F, FieldTypes) ->
    CtxP = add_path(Ctx, F),
    graphql_ast:name(ID),
    case maps:get(Name, FieldTypes, not_found) of
        not_found when Name == <<"__typename">> ->
            {ok, {introspection, typename}};
        not_found ->
            err(CtxP, unknown_field);
        #schema_field{} = Ty ->
            {ok, Ty}
    end.

check(#document{} = Doc) ->
    check(#ctx{}, Doc).

-spec check(Context :: ctx(), Exp :: expr()) -> {ok, expr()}.
check(Ctx, {directives, OpType, Dirs}) ->
    NamedDirectives = [{graphql_ast:name(ID), D}
                       || #directive { id = ID } = D <- Dirs],
    case graphql_ast:uniq(NamedDirectives) of
        ok ->
            {ok, [check(Ctx, {OpType, D}) || D <- Dirs]};
        {not_unique, X} ->
            err(Ctx, {directives_not_unique, X})
    end;
check(Ctx, {OpType, #directive {} = D}) ->
    {ok, Ty} = infer(Ctx, D),
    check(Ctx, {OpType, D}, Ty);
check(Ctx, #frag{} = F) ->
    {ok, Type} = infer(Ctx, F),
    check(Ctx, F, Type);
check(Ctx, #op{} = O) ->
    {ok, Type} = infer(Ctx, O),
    check(Ctx, O, Type);
check(Ctx, #document{ definitions = Defs } = Doc) ->
    CtxP = add_path(Ctx, document),
    {ok, Doc#document {
           definitions = [check(CtxP, Op) || Op <- Operations] }}.

-spec check(Context :: ctx(), Exp :: expr(), Ty :: ty()) -> {ok, expr()}.
check(Ctx, {var, ID}, Sigma) ->
    {ok, Tau} = infer(Ctx, {var, ID}),
    case sub(Tau, Sigma) of
        yes -> {ok, {var, ID, Tau}};
        no ->
            err(Ctx, {type_mismatch,
                      #{ document => {var, Var, Tau},
                         schema => Sigma }})
    end;
check(Ctx, {args, []}, _Ty) ->
    {ok, []};
check(Ctx, {args, [{K, V}|Args]}, Ty) ->
    {ok, Rest} = check(Ctx, {args, Args}, Ty),
    {ok, CTy} = infer(Ctx, {arg, K}, Ty),
    {ok, [{K, #{ type => CTy,
                 value => V }}|Rest]};
check(Ctx, {Context,
            #directive{}},
      #directive_type { args = SArgs, locations = Locations }) ->
    CtxP = add_path(Ctx, D),
    case lists:member(Context, Locations) of
        true ->
            {ok, D#directive { args = check(CtxP, {args, Args}, SArgs),
                               schema = Ty }};
        false ->
            err(Ctx, {invalid_directive_location, graphql_ast:name(ID), Context})
    end;
check(Ctx, [], Ty) ->
    case Ty of
        #object_type{} -> err(Ctx, fieldless_object);
        #interface_type{} -> err(Ctx, fieldless_interface);
        _ ->
            {ok, []}
    end;
check(Ctx, [#field{} = F|Fs], {non_null, Ty}) -> check(Ctx, [F|Fs], Ty);
check(Ctx, [#field{} = F|Fs], {list, Ty}) -> check(Ctx, [F|Fs], Ty);
check(Ctx, [#field{} = F|Fs], not_found) -> exit(broken_invariant);
check(Ctx, [#field{} = F|Fs], #scalar_type{}) -> err(Ctx, selection_on_scalar);
check(Ctx, [#field{} = F|Fs], #enum_type{}) -> err(Ctx, selection_on_enum);
check(Ctx, [#field{} = F|Fs], #object_type { fields = Fields }) -> check(Ctx, [F|Fs], Fields);
check(Ctx, [#field{} = F|Fs], #interface_type { fields = Fields }) -> check(Ctx, [F|Fs], Fields);
check(Ctx, [#field{} = F|Fs], #union_type {}) -> check(Ctx, [F|Fs], #{});
check(Ctx, [#field{ args = Args, directives = Dirs } = F | Fs], FieldTypes)
  when is_map(FieldTypes) ->
    CtxP = add_path(Ctx, F),
    {ok, Rest} = check(Ctx, Fs, FieldTypes),
    {ok, CDirectives} = check(CtxP, {directive, field, Dirs}),
    case infer(Ctx, F, FieldTypes) of
        {ok, {introspection, typename} = Ty} ->
            {ok, [F#field { schema = Ty,
                            directives = CDirectives }
                  |Rest]};
        {ok, #schema_field { ty = Ty, args = TArgs } = SF} ->
            {ok, Type} = output_type(Ty),
            {ok, CSSet} = check(CtxP, SSet, Type),
            {ok, CArgs} = check(CtxP, {args, Args}, TArgs),
            {ok, [F#field {
                    args = CArgs,
                    schema = SF#schema_field { ty = Type },
                    directives = CDirectives,
                    selection_set = CSSet }
                  | Rest]}
    end;
check(Ctx, #op { vardefs = VDefs, directives = Dirs, selection_set = SSet } = Op,
           #object_type { fields = Fields }) ->
    CtxP = add_path(Ctx, Op),
    OperationType = operation_context(Op),
    {ok, CDirectives} = check(CtxP, {directive, OperationType, Dirs}),
    {ok, CSSet} = check(CtxP, SSet, Fields),
    {ok, VarDefs} = var_defs(CtxP, {var_def, VDefs}),
    {ok, Op#op {
           schema = Ty,
           directives = CDirectives,
           selection_set = CSSet,
           var_defs = VarDefs}}.
    
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

%% -- INTERNAL FUNCTIONS ------------------------------------------------------------

%% Assert a type is an input type
input_type(Ty) ->
    case type(Ty) of
        {error, Reason} -> {error, Reason};
        {'*', V} -> {ok, V};
        {'+', V} -> {ok, V};
        {'-', _} -> {error, {invalid_input_type, Ty}}
    end.

%% Assert a type is an output type
output_type(Ty) ->
    case type(Ty) of
        {error, Reason} -> {error, Reason};
        {'*', V} -> {ok, V};
        {'-', V} -> {ok, V};
        {'+', _} -> {error, {invalid_output_type, Ty}}
    end.

%% Handle a list of vardefs by elaboration of their types
var_defs(Path, VDefs) ->
    [case input_type(V#vardef.ty) of
         {ok, Ty} -> V#vardef { ty = Ty };
         {error, not_found} -> err(Path, {type_not_found, graphql_ast:id(V)});
         {error, {invalid_input_type, T}} -> err(Path, {not_input_type, T})
     end || V <- VDefs].


operation_context(#op { ty = Ty }) ->
    case Ty of
        undefined -> query;
        {query, _} -> query;
        {mutation, _} -> mutation;
        {subscription, _} -> subscription
    end.

add_path(#ctx { path = P } = Ctx, C) ->
    Ctx#ctx { path = [C|P] }.

err(#context{ path = Path }, Msg) ->
    throw({error, Path, Msg}).
