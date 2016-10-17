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
       fun_env => mk_funenv(NewClauses),
       ast => {document, NewClauses}
   }}.

tc_clauses(Ctx, Path, Cs) ->
    [tc_clause(Ctx, Path, C) || C <- Cs].
    
tc_clause(Ctx, Path, #frag{} = Frag) -> tc_frag(Ctx, Path, Frag);
tc_clause(Ctx, Path, #op{} = Op) -> tc_op(Ctx, Path, Op).

%% -- MK OF FUNENV ------------------------------

%% The function environment encodes a mapping from the name of a query
%% or mutation into the vars/params it accepts and their corresponding
%% type scheme. This allows us to look up a function call via the
%% variable environment later when we execute a given function in the
%% GraphQL Schema.

mk_funenv(Ops) -> mk_funenv(Ops, #{}).

mk_funenv([], FunEnv) -> FunEnv;
mk_funenv([#frag{} | Next], FunEnv) -> mk_funenv(Next, FunEnv);
mk_funenv([#op { id = OpName, vardefs = VDefs } | Next], FunEnv) ->
    VarEnv = mk_varenv([], VDefs),
    mk_funenv(Next, FunEnv#{ graphql_ast:name(OpName) => VarEnv }).

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
    
tc_param(Path, K, {{non_null, _}, _Default}, not_found) ->
    graphql_err:abort([K | Path], missing_non_null_param);
tc_param(_Path, _K, {_Ty, Default}, not_found) ->
    {replace, Default};
tc_param(Path, K, {Ty, _}, Val) ->
    check_param([K | Path], Ty, Val).
    
%% When checking params, the top level has been elaborated by the
%% elaborator, but the levels under it has not. So we have a case where
%% we need to look up underlying types and check them.

check_param(Path, {non_null, Ty}, V) -> check_param(Path, Ty, V);
check_param(Path, {scalar, Sc}, V) -> input_coerce_scalar(Path, Sc, V);
check_param(Path, {enum, Ty}, {enum, V}) when is_binary(V) ->
    check_param(Path, {enum, Ty}, V);
check_param(Path, {enum, Ty}, V) when is_binary(V) ->
    case graphql_schema:lookup_enum_type(V) of
        not_found -> graphql_err:abort(Path, {unknown_enum_value, V});
        #enum_type { id = Ty, repr = Repr } ->
            {replace, enum_representation(Repr, V)};
        _OtherTy -> graphql_err:abort(Path, {param_mismatch, {enum, Ty}})
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
check_param(Path, {input_object, Ty}, Obj) ->
    check_input_object(Path, Ty, Obj);
%% The following expands un-elaborated (nested) types
check_param(Path, Ty, V) when is_binary(Ty) ->
    case graphql_schema:lookup(Ty) of
        #scalar_type {} = ScalarTy -> input_coerce_scalar(Path, ScalarTy, V);
        #input_object_type {} = IOType -> check_input_object(Path, IOType, V);
        #enum_type {} -> check_param(Path, {enum, Ty}, V);
        _ ->
            graphql_err:abort(Path, {param_mismatch, Ty, V})
    end;
%% Everything else are errors
check_param(Path, Ty, V) ->
    graphql_err:abort(Path, {param_mismatch, Ty, V}).

check_input_object(Path, #input_object_type{ fields = Fields }, Obj) ->
    {replace, check_object_fields(Path, maps:to_list(Fields), Obj, #{})}.

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
                
input_coerce_scalar(Path, Ty, V) ->
    case coerce_scalar(Ty, V) of
        ok -> ok;
        {replace, V2} -> {replace, V2};
        {error, Reason} -> graphql_err:abort(Path, Reason)
    end.
    
coerce_scalar(id, V) when is_binary(V) -> ok;
coerce_scalar(string, V) when is_binary(V) -> ok;
coerce_scalar(int, V) when is_integer(V) -> ok;
coerce_scalar(float, V) when is_float(V) -> ok;
coerce_scalar(bool, true) -> ok;
coerce_scalar(bool, false) -> ok;
coerce_scalar(Ty, _)
  when
      Ty == id;
      Ty == string;
      Ty == int;
      Ty == float;
      Ty == bool ->
    {error, {type_mismatch, #{ schema => {scalar, Ty}}}};
coerce_scalar(#scalar_type { input_coerce = IC }, Val) ->
    try IC(Val) of
        {ok, NewVal} -> {replace, NewVal};
        {error, Reason} -> {error, Reason}
    catch
        Cl:Err ->
            {error, {input_coerce_abort, {Cl, Err}}}
    end.

%% -- FRAGMENTS --------------------------------

fragments(Clauses) ->
    lists:partition(
      fun
         (#frag{}) -> true;
         (_) -> false
      end,
      Clauses).

mk_fragenv(Frags) ->
    F = fun(#frag { id = ID, ty = Ty }) -> {graphql_ast:name(ID), Ty} end,
    maps:from_list(
       [F(Frg) || Frg <- Frags]).

tc_frag(Ctx, Path, #frag {selection_set = SSet} = Frag) ->
    Frag#frag { selection_set = tc_sset(Ctx, Path, SSet) }.

%% -- OPERATIONS -------------------------------

tc_op(Ctx, Path, #op { vardefs = VDefs, selection_set = SSet} = Op) ->
    VarEnv = mk_varenv([Op | Path], VDefs),
    Op#op { selection_set = tc_sset(Ctx#{ varenv => VarEnv }, [Op | Path], SSet) }.

%% -- SELECTION SETS ------------------------------------
tc_sset(Ctx, Path, SSet) -> [tc_field(Ctx, Path, S) || S <- SSet].

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
tc_field(Ctx, Path, 
	#field { args = Args, selection_set = SSet, schema = Schema, schema_obj = Obj } = F) ->
    case {Schema, Obj} of
        {_, scalar} when SSet /= [] ->
            graphql_err:abort(Path, selection_on_scalar);
        {{introspection, typename}, scalar} ->
            F;
        {#schema_field { args = SArgs }, scalar } ->
           F#field { args = tc_args(Ctx, [F | Path], Args, SArgs)};
        {#schema_field { args = SArgs }, Obj} ->
           F#field {
               args = tc_args(Ctx, [F | Path], Args, SArgs),
               selection_set = tc_sset(Ctx, [F | Path], SSet) }
    end.
            
%% -- ARGS -------------------------------------
tc_args(Ctx, Path, Args, Schema) ->
    case uniq(lists:sort(Args)) of
        ok -> tc_args_(Ctx, Path, Args, Schema);
        {error, Reason} -> graphql_err:abort(Path, Reason)
    end.

tc_args_(_Ctx, _Path, [], _Schema) -> [];
tc_args_(Ctx, Path, [{ID, {Ty, Val}} = A | Next], Schema) ->
    Name = graphql_ast:name(ID),
    case ty_check(Path, ty_of(Ctx, Path, graphql_ast:unwrap_type(Ty), Val), schema_type(Ty)) of
        ok ->
            [A | tc_args_(Ctx, Path, Next, Schema)];
        {replace, RVal } ->
            [{ID, {Ty, RVal}} | tc_args_(Ctx, Path, Next, Schema)];
        {error, Expected} ->
            graphql_err:abort(Path, {type_mismatch, #{ id => Name, schema => Expected }});
        {error, Got, Expected} ->
            graphql_err:abort(Path, {type_mismatch,
                 #{ id => Name, document => Got, schema => Expected }})
    end.

uniq([]) -> ok;
uniq([_]) -> ok;
uniq([{X, _}, {X, _} | _]) -> {error, {unique, X}};
uniq([_ | Next]) -> uniq(Next).

schema_type({scalar, X}) when is_atom(X) -> {scalar, X};
schema_type({non_null, T}) -> {non_null, schema_type(T)};
schema_type([Tag]) -> {list, schema_type(Tag)};
%% Elaborate types which are not elaborated.
%% Strictly, this ought to be unnecessary given enough
%% elaboration and optimization.
schema_type(Tag) ->
    case graphql_schema:lookup(Tag) of
        #scalar_type{} = SType -> SType;
        #enum_type{} -> {enum, Tag};
        #object_type{} -> {object, Tag};
        #input_object_type{} = IOType -> {input_object, IOType};
        #interface_type{} -> {interface, Tag};
        not_found ->
            exit({schema_not_found, Tag})
    end.

ty_of(#{ varenv := VE }, Path, _, {var, ID}) ->
    Name = graphql_ast:name(ID),
    case maps:get(Name, VE, not_found) of
        not_found ->
            graphql_err:abort(Path, {unbound_variable, Name});
        {Ty, _Default} -> Ty
    end;
ty_of(_Ctx, Path, _, {enum, N}) ->
    case graphql_schema:lookup_enum_type(N) of
        not_found -> graphql_err:abort(Path, {unknown_enum, N});
        #enum_type { id = EnumTy } -> {enum, EnumTy}
    end;
ty_of(_Ctx, _Path, {scalar, Tag}, V) ->
    case valid_scalar_value(V) of
        true -> {scalar, Tag, V};
        false -> ty_of(_Ctx, _Path, undefined, V)
    end;
ty_of(_Ctx, _Path, _, {name, N, _}) -> N;
ty_of(_Ctx, _Path, _, S) when is_binary(S) -> {scalar, string, S};
ty_of(_Ctx, _Path, _, I) when is_integer(I) -> {scalar, int, I};
ty_of(_Ctx, _Path, _, F) when is_float(F) -> {scalar, float, F};
ty_of(_Ctx, _Path, _, true) -> {scalar, bool, true};
ty_of(_Ctx, _Path, _, false) -> {scalar, bool, false};
ty_of(_Ctx, _Path, _, Obj) when is_map(Obj) -> coerce_object(Obj);
ty_of(Ctx, Path, {list, Ty}, {list, Ts}) when is_list(Ts) ->
    {list, [ty_of(Ctx, Path, Ty, T) || T <- Ts]}.

%% EQ Match:
ty_check(_Path, X, X) -> ok;
%% Compound:
ty_check(Path, {non_null, A}, T) -> ty_check(Path, A, T);
ty_check(Path, A, {non_null, T}) -> ty_check(Path, A, T);
ty_check(Path, {list, As}, {list, T}) ->
    case lists:all(fun(X) -> ty_check(Path, X, T) == ok end, As) of
        true -> ok;
        false -> {error, {list, T}}
    end;
%% Ground:
ty_check(_Path, {scalar, Tag, V}, {scalar, Tag}) -> {replace, V};
ty_check(Path, {scalar, Tag, V}, #scalar_type { id = Tag, input_coerce = IC }) ->
    case IC(V) of
        {ok, V} -> ok;
        {ok, NV} -> {replace, NV};
        {error, Reason} ->
            graphql_err:abort(Path, {input_coercion, Tag, V, Reason})
    end;
ty_check(Path, Obj, {input_object, #input_object_type{} = Ty}) when is_map(Obj) ->
    check_input_object(Path, Ty, Obj);
%% Failure:
ty_check(_Path, A, T) -> {error, A, T}.

coerce_object(Obj) when is_map(Obj) ->
    coerce_object_(Obj).
    
coerce_object_(Obj) when is_map(Obj) ->
    Elems = maps:to_list(Obj),
    maps:from_list([{graphql_ast:name(K), coerce_object_(V)} || {K, V} <- Elems]);
coerce_object_(Other) -> Other.

%% -- VARENV -------------------------------------
mk_varenv(Path, VDefs) ->
    maps:from_list(
       [{graphql_ast:name(Var), {varenv_ty_coerce(Path, Ty), Def}}
           || #vardef { id = Var, ty = Ty, default = Def } <- VDefs]).

varenv_ty_coerce(_Path, {scalar, X}) -> {scalar, X};
varenv_ty_coerce(Path, {list, T}) -> {list, varenv_ty_coerce(Path, T)};
varenv_ty_coerce(Path, {non_null, T}) -> {non_null, varenv_ty_coerce(Path, T)};
varenv_ty_coerce(Path, T) ->
    N = graphql_ast:name(T),
    case graphql_schema:lookup(N) of
        not_found -> graphql_err:abort(Path, {unknown_type, N});
        #enum_type{} -> {enum, N};
        #scalar_type{} -> {scalar, N};
        #input_object_type{} = IOType -> {input_object, IOType};
        #interface_type{} -> {interface, N}
    end.

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

