%%% @doc Type checking of GraphQL query documents
%%%
%%% @end
-module(graphql_type_check).

-include("graphql_internal.hrl").
-include("graphql_schema.hrl").

-export([x/1, x_params/3]).


%% -- TOP LEVEL TYPE CHECK CODE -------------------------------

%% Type checking proceeds by the ordinary way of writing a type
%% checker. First we split the input query document definitions, which
%% we call "Clauses", into its fragments and the remaining operations.
%% Next, we create an environment of fragments, so we can refer to
%% their types as we type check other things.
%%
%% Type checking then proceeds one Clause at a time.
-spec x(graphql:ast()) -> {ok, #{ atom() => any()}}.
x(Doc) ->
    x(#{}, Doc).

x(Ctx, {document, Clauses}) ->
   tc(Ctx, [document], Clauses).

tc(Ctx, Path, Clauses) ->
   {Fragments, _Rest} = fragments(Clauses),
   FragCtx = Ctx#{ fragenv => mk_fragenv(Fragments) },
   NewClauses = tc_clauses(FragCtx, Path, Clauses),
   {ok, #{
       fun_env => graphql_elaborate:mk_funenv(NewClauses),
       ast => {document, NewClauses}
   }}.

tc_clauses(Ctx, Path, Cs) ->
    [tc_clause(Ctx, Path, C) || C <- Cs].
    
tc_clause(Ctx, Path, #frag{} = Frag) -> tc_frag(Ctx, Path, Frag);
tc_clause(Ctx, Path, #op{} = Op) -> tc_op(Ctx, Path, Op).

%% -- TYPE CHECK OF PARAMETER ENVS ------------------

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

-spec x_params(any(), any(), any()) -> graphql:param_context().
x_params(_FunEnv, undefined, #{}) -> #{};
x_params(_FunEnv, undefined, _) -> graphql_err:abort([], params_on_unnamed);
x_params(FunEnv, OpName, Params) ->
    case maps:get(OpName, FunEnv, not_found) of
        not_found ->
           graphql_err:abort([], {operation_not_found, OpName});
        TyVarEnv ->
            tc_params([OpName], TyVarEnv, Params)
    end.

tc_params(Path, TyVarEnv, InitialParams) ->
    F = 
      fun(K, V0, PS) ->
        case tc_param(Path, K, V0, maps:get(K, PS, not_found)) of
            ok -> PS;
            {replace, V1} -> PS#{ K => V1 }
        end
      end,
    maps:fold(F, InitialParams, (TyVarEnv)).

tc_param(Path, K, #vardef { ty = {non_null, _} }, not_found) ->
    graphql_err:abort([K | Path], missing_non_null_param);
tc_param(_Path, _K, #vardef { default = Default }, not_found) ->
    {replace, Default};
tc_param(Path, K, #vardef { ty = Ty }, Val) ->
    check_param([K | Path], Ty, Val).
    
%% When checking params, the top level has been elaborated by the
%% elaborator, but the levels under it has not. So we have a case where
%% we need to look up underlying types and check them.

check_param(Path, {non_null, Ty}, V) -> check_param(Path, Ty, V);
check_param(Path, {scalar, Sc}, V) -> input_coerce_scalar(Path, Sc, V);
check_param(Path, #enum_type{} = ETy, {enum, V}) when is_binary(V) ->
    check_param(Path, ETy, V);
check_param(Path, #enum_type { id = Ty }, V) when is_binary(V) ->
    case graphql_schema:lookup_enum_type(V) of
        #enum_type { id = Ty, repr = Repr } ->
            {replace, enum_representation(Repr, V)};
        OtherTy ->
            graphql_err:abort(Path, {param_mismatch, {enum, Ty, OtherTy}})
    end;
check_param(Path, {list, T}, L) when is_list(L) ->
    %% Build a dummy structure to match the recursor. Unwrap this
    %% structure before replacing the list parameter.
    NewList = [
        case check_param(Path, T, X) of
            ok -> X;
            {replace, X2} -> X2
        end || X <- L],
    {replace, NewList};
check_param(Path, #input_object_type{} = IOType, Obj) when is_map(Obj) ->
    check_input_object(Path, IOType, Obj);
%% The following expands un-elaborated (nested) types
check_param(Path, Ty, V) when is_binary(Ty) ->
    case graphql_schema:lookup(Ty) of
        #scalar_type {} = ScalarTy -> input_coerce_scalar(Path, ScalarTy, V);
        #input_object_type {} = IOType -> check_input_object(Path, IOType, V);
        #enum_type {} = Enum -> check_param(Path, Enum, V);
        _ ->
            graphql_err:abort(Path, {param_mismatch, Ty, V})
    end;
%% Everything else are errors
check_param(Path, Ty, V) ->
    graphql_err:abort(Path, {param_mismatch, Ty, V}).

check_input_object(Path, #input_object_type{ fields = Fields }, Obj) ->
    Coerced = coerce_object(Obj),
    {replace, check_object_fields(Path, maps:to_list(Fields), Coerced, #{})}.



check_object_fields(Path, [], Obj, Result) ->
    case maps:size(Obj) of
        0 -> Result;
        K when K > 0 -> graphql_err:abort(Path, {excess_fields_in_object, Obj})
    end;
check_object_fields(Path, [{Name, #schema_arg { ty = Ty, default = Def }} | Next], Obj, Result) ->
    Val = case maps:get(Name, Obj, not_found) of
        not_found ->
            case Ty of
                {non_null, _} ->
                    graphql_err:abort([Name | Path], missing_non_null_param);
                _ ->
                    Def
            end;
        V ->
            case check_param([Name | Path], Ty, V) of
                ok -> V;
                {replace, V2} -> V2
            end
    end,
    check_object_fields(Path, Next, maps:remove(Name, Obj), Result#{ Name => Val }).

input_coerce_scalar(_Path, id, V) when is_binary(V) -> ok;
input_coerce_scalar(_Path, string, V) when is_binary(V) -> ok;
input_coerce_scalar(_Path, int, V) when is_integer(V) -> ok;
input_coerce_scalar(_Path, float, V) when is_float(V) -> ok;
input_coerce_scalar(_Path, float, I) when is_integer(I) -> {replace, float(I)};
input_coerce_scalar(_Path, bool, true) -> ok;
input_coerce_scalar(_Path, bool, false) -> ok;
input_coerce_scalar(Path, #scalar_type {} = SType, Val) ->
    input_coercer(Path, SType, Val);
input_coerce_scalar(Path, Ty, _V) ->
    graphql_err:abort(Path, {type_mismatch, #{ schema => {scalar, Ty}}}).

input_coercer(Path, #scalar_type { id = ID, input_coerce = IC}, Val) ->
    try IC(Val) of
        {ok, NewVal} -> {replace, NewVal};
        {error, Reason} -> graphql_err:abort(Path, {input_coercion, ID, Val, Reason})
    catch
        Cl:Err ->
            graphql_err:abort(Path, {input_coerce_abort, {Cl, Err}})
    end.

%% -- FRAGMENTS --------------------------------

fragments(Clauses) ->
    lists:partition(fun (#frag{}) -> true; (_) -> false end, Clauses).

mk_fragenv(Frags) ->
    F = fun(#frag { id = ID } = Frag) -> {graphql_ast:name(ID), Frag} end,
    maps:from_list([F(Frg) || Frg <- Frags]).

tc_frag(Ctx, Path, #frag {selection_set = SSet} = Frag) ->
    Frag#frag { selection_set = tc_sset(Ctx, Path, SSet) }.

%% -- OPERATIONS -------------------------------

tc_op(Ctx, Path, #op { vardefs = VDefs, selection_set = SSet} = Op) ->
    VarEnv = graphql_elaborate:mk_varenv([Op | Path], VDefs),
    Op#op { selection_set = tc_sset(Ctx#{ varenv => VarEnv }, [id(Op) | Path], SSet) }.

%% -- SELECTION SETS ------------------------------------
tc_sset(Ctx, Path, SSet) ->
    [tc_field(Ctx, Path, S) || S <- SSet].

tc_field(#{ fragenv := FE }, Path, #frag_spread { id = ID } = FSpread) ->
    Name = graphql_ast:name(ID),
    case maps:get(Name, FE, not_found) of
        not_found ->
            graphql_err:abort(Path, {unknown_fragment, Name});
        _FragTy ->
            %% You can always include a fragspread, as long as it exists
            %% It may be slightly illegal in a given context but this just
            %% means the system will ignore the fragment on execution
            FSpread
    end;
tc_field(Ctx, Path, #frag { id = '...', selection_set = SSet} = InlineFrag) ->
    InlineFrag#frag {
        selection_set = tc_sset(Ctx, [InlineFrag | Path], SSet)
    };
tc_field(_Ctx, Path, #field { selection_set = [_|_],
                              schema_obj = scalar }) ->
    graphql_err:abort(Path, selection_on_scalar);
tc_field(_Ctx, _Path, #field { schema = {introspection, typename},
                               schema_obj = scalar } = F) ->
    F;
tc_field(Ctx, Path, #field { args = Args,
                             schema = #schema_field { args = SArgs },
                             schema_obj = scalar } = F) ->
    F#field { args = tc_args(Ctx, [F | Path], Args, SArgs)};
tc_field(Ctx, Path, #field { args = Args,
                             selection_set = SSet,
                             schema = #schema_field { args = SArgs }} = F) ->
    F#field { args = tc_args(Ctx, [F | Path], Args, SArgs),
              selection_set = tc_sset(Ctx, [F | Path], SSet) }.
            
%% -- ARGS -------------------------------------
tc_args(Ctx, Path, Args, Schema) ->
    NArgs = names(Args),
    case uniq(lists:sort(NArgs)) of
        ok ->
          SchemaArgs = maps:to_list(Schema),
          tc_args_(Ctx, Path, NArgs, SchemaArgs, []);
        {error, Reason} -> graphql_err:abort(Path, Reason)
    end.

names(Args) -> [{graphql_ast:name(K), V} || {K, V} <- Args].
     
tc_args_(_Ctx, _Path, [], [], Acc) -> Acc;
tc_args_(_Ctx, Path, [_|_] = Args, [], _Acc) ->
    graphql_err:abort(Path, {excess_args, Args});
tc_args_(Ctx, Path, Args, [{Name, #schema_arg { ty = STy }} = SArg | Next], Acc) ->
    case find_arg(Args, SArg) of
        {error, Reason} ->
            graphql_err:abort([Name | Path], Reason);
        {ok, {_, #{ type := Ty, value := Val}} = A, NextArgs} ->
            ValueType = value_type(Ctx, Path, graphql_ast:unwrap_type(Ty), Val),
            SchemaType = schema_type(STy),
            case refl(Path, ValueType, SchemaType) of
                ok ->
                    tc_args_(Ctx, Path, NextArgs, Next, [A | Acc]);
                {replace, RVal} ->
                    tc_args_(Ctx, Path, NextArgs, Next, [{Name, {Ty, RVal}} | Acc]);
                {error, Expected} ->
                    graphql_err:abort(Path, {type_mismatch, #{ id => Name, schema => Expected }});
                {error, Got, Expected} ->
                     graphql_err:abort(Path, {type_mismatch,
                          #{ id => Name, document => Got, schema => Expected }})
            end
    end.

%% Search a list of arguments for the next argument. Handle non-null values
%% correctly as we are conducting the search
find_arg(Args, {Key, #schema_arg { ty = {non_null, _}}}) ->
    case lists:keytake(Key, 1, Args) of
        false ->
            {error, missing_non_null_param};
        {value, Arg, NextArgs} ->
            {ok, Arg, NextArgs}
    end;
find_arg(Args, {Key, #schema_arg { ty = Ty, default = Default }}) ->
    case lists:keytake(Key, 1, Args) of
        false ->
            {ok, {Key, #{ type => Ty, value => Default }}, Args};
        {value, Arg, NextArgs} ->
            {ok, Arg, NextArgs}
    end.

uniq([]) -> ok;
uniq([_]) -> ok;
uniq([{X, _}, {X, _} | _]) -> {error, {unique, X}};
uniq([_ | Next]) -> uniq(Next).

-spec schema_type(binary() | schema_type()) -> schema_type().
schema_type({scalar, X}) when is_atom(X) -> {scalar, X};
schema_type({non_null, T}) -> {non_null, schema_type(T)};
schema_type([Tag]) -> {list, schema_type(Tag)};
schema_type(#enum_type{} = Ty) -> Ty;
schema_type(#input_object_type{} = Ty) -> Ty;
schema_type(#scalar_type{} = Ty) -> Ty;
%% Elaborate types which are not elaborated.
%% Strictly, this ought to be unnecessary given enough
%% elaboration and optimization.
schema_type(Tag) ->
    case graphql_schema:lookup(Tag) of
        #scalar_type{} = SType -> SType;
        #enum_type{} = Enum -> Enum;
        #object_type{} = OType -> OType;
        #input_object_type{} = IOType -> IOType;
        #interface_type{} = IFType -> IFType;
        not_found ->
            exit({schema_not_found, Tag})
    end.

value_type(#{ varenv := VE }, Path, _, {var, ID}) ->
    Name = graphql_ast:name(ID),
    case maps:get(Name, VE, not_found) of
        not_found ->
            graphql_err:abort(Path, {unbound_variable, Name});
        #vardef { ty = Ty } -> Ty
    end;
value_type(_Ctx, _Path, _, null) -> null;
value_type(_Ctx, _Path, #enum_type{} = Ty, {enum, _N}) ->
    Ty;
value_type(_Ctx, Path, _, {enum, N}) ->
    case graphql_schema:lookup_enum_type(N) of
        not_found -> graphql_err:abort(Path, {unknown_enum, N});
        #enum_type {} = Enum -> Enum
    end;
value_type(_Ctx, _Path, {scalar, Tag}, V) ->
    case valid_scalar_value(V) of
        true -> {scalar, Tag, V};
        false -> value_type(_Ctx, _Path, undefined, V)
    end;
value_type(_Ctx, _Path, _, {name, N, _}) -> N;
value_type(_Ctx, _Path, _, S) when is_binary(S) -> {scalar, string, S};
value_type(_Ctx, _Path, _, I) when is_integer(I) -> {scalar, int, I};
value_type(_Ctx, _Path, _, F) when is_float(F) -> {scalar, float, F};
value_type(_Ctx, _Path, _, true) -> {scalar, bool, true};
value_type(_Ctx, _Path, _, false) -> {scalar, bool, false};
value_type(_Ctx, _Path, _, Obj) when is_map(Obj) -> coerce_object(Obj);
value_type(Ctx, Path, {list, Ty}, {list, Ts}) when is_list(Ts) ->
    {list, [value_type(Ctx, Path, Ty, T) || T <- Ts]}.

%% EQ Match:
refl(_Path, X, X) -> ok;
%% Compound:
refl(_Path, null, {non_null, _}) -> {error, non_null};
refl(Path, {list, As}, {list, T}) ->
    case lists:all(fun(X) -> refl(Path, X, T) == ok end, As) of
        true -> ok;
        false -> {error, {list, T}}
    end;
refl(_Path, null, _T) -> ok;
refl(Path, {non_null, A}, T) -> refl(Path, A, T);
refl(Path, A, {non_null, T}) -> refl(Path, A, T);
%% Ground:
refl(_Path, {scalar, Tag, V}, {scalar, Tag}) -> {replace, V};
refl(Path, {scalar, Tag, V}, #scalar_type { id = ID } = SType) ->
    case Tag of
        string -> input_coercer(Path, SType, V);
        int -> input_coercer(Path, SType, V);
        float -> input_coercer(Path, SType, V);
        ID -> input_coercer(Path, SType, V)
    end;
refl(_Path, #input_object_type { id = ID }, {input_object, #input_object_type { id = ID }}) ->
    ok;
refl(Path, Obj, #input_object_type{} = Ty) when is_map(Obj) ->
    check_input_object(Path, Ty, Obj);
%% Nullable types
%% Failure:
refl(_Path, A, T) -> {error, A, T}.

coerce_object(Obj) when is_map(Obj) ->
    coerce_object_(Obj).
    
coerce_object_(Obj) when is_map(Obj) ->
    Elems = maps:to_list(Obj),
    maps:from_list([{coerce_name(K), coerce_object_(V)} || {K, V} <- Elems]);
coerce_object_(Other) -> Other.

coerce_name(B) when is_binary(B) -> B;
coerce_name(Name) -> graphql_ast:name(Name).

%% -- AST MANIPULATION -------------------------

%% True if input is a scalar value
valid_scalar_value(S) when is_binary(S) -> true;
valid_scalar_value(F) when is_float(F) -> true;
valid_scalar_value(I) when is_integer(I) -> true;
valid_scalar_value(true) -> true;
valid_scalar_value(false) -> true;
valid_scalar_value(_) -> false.

enum_representation(binary, V) -> V;
enum_representation(atom, V) -> binary_to_atom(V, utf8);
enum_representation(tagged, V) -> {enum, V}.

id(#op { id = ID }) -> ID.
