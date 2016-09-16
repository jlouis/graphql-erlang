-module(gql_tc).

-include("gql.hrl").
-include("gql_schema.hrl").

-export([x/1, x_params/3]).

-spec x(gql:ast()) -> {ok, #{ atom() => any()}}.
x(Doc) -> x(#{}, Doc).

x(Ctx, {document, Clauses}) ->
   tc(Ctx, [document], Clauses).

tc(Ctx, Path, Clauses) ->
   {Fragments, _Rest} = fragments(Clauses),
   FragCtx = Ctx#{ fragenv => tc_mk_fragenv(Fragments) },
   NewClauses = tc_clauses(FragCtx, Path, Clauses),
   {ok, #{
       fun_env => tc_funenv(NewClauses),
       ast => {document, NewClauses}
   }}.

tc_clauses(Ctx, Path, Cs) ->
    [tc_clause(Ctx, Path, C) || C <- Cs].
    
tc_clause(Ctx, Path, #frag{} = Frag) -> tc_frag(Ctx, Path, Frag);
tc_clause(Ctx, Path, #op{} = Op) -> tc_op(Ctx, Path, Op).

%% -- MK OF FUNENV ------------------------------

tc_funenv(Ops) -> tc_funenv(Ops, #{}).
tc_funenv([], FunEnv) -> FunEnv;
tc_funenv([#frag{} | Next], FunEnv) -> tc_funenv(Next, FunEnv);
tc_funenv([#op { id = OpName, vardefs = VDefs } | Next], FunEnv) ->
    VarEnv = tc_mk_varenv([], VDefs),
    tc_funenv(Next, FunEnv#{ name(OpName) => VarEnv }).

%% -- TYPE CHECK OF PARAMETER ENVS ------------------
-spec x_params(any(), any(), any()) -> gql:param_context().
x_params(_FunEnv, undefined, #{}) -> #{};
x_params(_FunEnv, undefined, _) ->
    gql_err:abort([], params_on_unnamed);
x_params(FunEnv, OpName, Vars) ->
    case maps:get(OpName, FunEnv, not_found) of
        not_found ->
           gql_err:abort([], {operation_not_found, OpName});
        VarEnv -> tc_params([OpName], maps:to_list(VarEnv), Vars)
    end.

tc_params(Path, Args, Params) ->
    F = 
      fun({K, _} = P, St) ->
        case tc_param(Path, P, maps:get(K, St, not_found)) of
            ok -> St;
            {replace, V} -> St#{ K => V }
        end
      end,
    lists:foldl(F, Params, Args).
    
tc_param(Path, {K, {Ty, Default}}, not_found) ->
    case non_null(Ty) of
        true -> gql_err:abort([K | Path], missing_non_null_param);
        false -> {replace, Default}
    end;
tc_param(Path, {K, {Ty, _}}, Val) ->
    check_param([K | Path], Ty, Val).
    
%% When checking params, the top level has been elaborated by the
%% elaborator, but the levels under it has not. So we have a case where
%% we need to look up underlying types and check them.
check_param(Path, {non_null, Ty}, V) -> check_param(Path, Ty, V);
check_param(Path, {scalar, Sc}, V) -> input_coerce_scalar(Path, Sc, V);
check_param(Path, {enum, Ty}, V) when is_binary(V) ->
    case gql_schema:lookup_enum_type(V) of
       not_found -> gql_err:abort(Path, {unknown_enum_value, V});
       Ty -> {replace, {enum, V}};
        _OtherTy -> gql_err:abort(Path, {param_mismatch, {enum, Ty}})
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
check_param(Path, [T], {list, L}) when is_list(L) ->
    %% Build a dummy structure to match the recursor. Unwrap this
    %% structure before replacing the list parameter.
    NewList = [
        case check_param(Path, T, X) of
            ok -> X;
            {replace, X2} -> X2
        end || X <- L],
    {replace, NewList};
check_param(Path, {input_object, T}, Obj) ->
    check_input_object(Path, T, Obj);
%% The following expands un-elaborated (nested) types
check_param(Path, Ty, V) when is_binary(Ty) ->
    case gql_schema:lookup(Ty) of
        #scalar_type {} -> input_coerce_scalar(Path, Ty, V);
        #input_object_type {} -> check_input_object(Path, Ty, V);
        #enum_type {} -> check_param(Path, {enum, Ty}, V);
        _ ->
            gql_err:abort(Path, {param_mismatch, Ty, V})
    end;
%% Everything else are errors
check_param(Path, Ty, V) ->
    gql_err:abort(Path, {param_mismatch, Ty, V}).

check_input_object(Path, T, Obj) ->
    case gql_schema:lookup(T) of
        not_found ->
            gql_err:abort(Path, {unknown_type, T});
        #input_object_type{ fields = Fields } ->
            {replace, check_object_fields(Path, maps:to_list(Fields), coerce_object(Obj), #{})};
        #interface_type{ id = ID } ->
            gql_err:abort(Path, {invalid_input_type, ID});
        #object_type{ id = ID } ->
            gql_err:abort(Path, {invalid_input_type, ID});
        #union_type { id = ID } ->
            gql_err:abort(Path, {invalid_input_type, ID})
    end.

coerce_object(#{} = Obj) -> Obj;
coerce_object({object, Obj}) -> input_object_type_scheme(Obj).

check_object_fields(Path, [], Obj, Result) ->
    case maps:size(Obj) of
        0 -> Result;
        K when K > 0 -> gql_err:abort(Path, {excess_fields_in_object, Obj})
    end;
check_object_fields(Path, [{Name, #schema_arg { ty = Ty, default = Def }} | Next], Obj, Result) ->
    Val = case maps:get(Name, Obj, not_found) of
        not_found ->
            case non_null(Ty) of
                true ->
                    gql_err:abort([Name | Path], missing_non_null_param);
                false ->
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
        {error, Reason} -> gql_err:abort(Path, Reason)
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
coerce_scalar(Ty, Val) ->
    case gql_schema:lookup(Ty) of
        not_found ->
            {error, scalar_type_not_found};
        #scalar_type { input_coerce = IC } ->
            try IC(Val) of
                {ok, NewVal} -> {replace, NewVal};
                {error, Reason} -> {error, Reason}
            catch
                Cl:Err ->
                    {error, {input_coerce_abort, {Cl, Err}}}
            end;
        _ ->
            {error, {not_scalar_type, Ty}}
    end.

%% -- FRAGMENTS --------------------------------

fragments(Clauses) ->
    lists:partition(
      fun
         (#frag{}) -> true;
         (_) -> false
      end,
      Clauses).

tc_mk_fragenv(Frags) ->
    F = fun(#frag { id = ID, ty = Ty }) -> {name(ID), Ty} end,
    maps:from_list(
       [F(Frg) || Frg <- Frags]).

tc_frag(Ctx, Path, #frag {selection_set = SSet} = Frag) ->
    Frag#frag { selection_set = tc_sset(Ctx, Path, SSet) }.

%% -- OPERATIONS -------------------------------

tc_op(Ctx, Path, #op { vardefs = VDefs, selection_set = SSet} = Op) ->
    VarEnv = tc_mk_varenv([Op | Path], VDefs),
    Op#op { selection_set = tc_sset(Ctx#{ varenv => VarEnv }, [Op | Path], SSet) }.

%% -- SELECTION SETS ------------------------------------
tc_sset(Ctx, Path, SSet) -> [tc_field(Ctx, Path, S) || S <- SSet].

tc_field(#{ fragenv := FE }, Path, #frag_spread { id = ID } = FSpread) ->
    Name = name(ID),
    case maps:get(Name, FE, not_found) of
        not_found ->
            gql_err:abort(Path, {unknown_fragment, Name});
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
        {{introspection, typename}, scalar} ->
            ok = scalar_check([F | Path], SSet),
            F;
        {#schema_field { args = SArgs }, scalar } ->
           ok = scalar_check([F | Path], SSet),
           F#field { args = tc_args(Ctx, [F | Path], Args, SArgs)};
        {#schema_field { args = SArgs }, Obj} ->
           F#field {
               args = tc_args(Ctx, [F | Path], Args, SArgs),
               selection_set = tc_sset(Ctx, [F | Path], SSet) }
    end.

scalar_check(_Path, []) -> ok;
scalar_check(Path, _) ->
    gql_err:abort(Path, selection_on_scalar).
            
%% -- ARGS -------------------------------------
tc_args(Ctx, Path, Args, Schema) ->
    case uniq(lists:sort(Args)) of
        ok -> tc_args_(Ctx, Path, Args, Schema);
        {error, Reason} -> gql_err:abort(Path, Reason)
    end.

tc_args_(_Ctx, _Path, [], _Schema) -> [];
tc_args_(Ctx, Path, [{ID, {Ty, Val}} = A | Next], Schema) ->
    Name = name(ID),
    case ty_check(Path, ty_of(Ctx, Path, unwrap_type(Ty), Val), schema_type(Ty)) of
        ok ->
            [A | tc_args_(Ctx, Path, Next, Schema)];
        {replace, RVal } ->
            [{ID, {Ty, RVal}} | tc_args_(Ctx, Path, Next, Schema)];
        {error, Expected} ->
            gql_err:abort(Path, {type_mismatch, #{ id => Name, schema => Expected }});
        {error, Got, Expected} ->
            gql_err:abort(Path, {type_mismatch,
                 #{ id => Name, document => Got, schema => Expected }})
    end.

uniq([]) -> ok;
uniq([_]) -> ok;
uniq([{X, _}, {X, _} | _]) -> {error, {unique, X}};
uniq([_ | Next]) -> uniq(Next).

unwrap_type({non_null, Ty}) -> unwrap_type(Ty);
unwrap_type([Ty]) -> {list, unwrap_type(Ty)};
unwrap_type(X) -> X.

schema_type({scalar, X}) -> {scalar, X};
schema_type({non_null, T}) -> {non_null, schema_type(T)};
schema_type([Tag]) -> {list, schema_type(Tag)};
%% Elaborate types which are not elaborated.
%% Strictly, this ought to be unnecessary given enough
%% elaboration and optimization.
schema_type(Tag) ->
    case gql_schema:lookup(Tag) of
        #scalar_type{ input_coerce = IC } -> {scalar, Tag, IC};
        #enum_type{} -> {enum, Tag};
        #object_type{} -> {object, Tag};
        #input_object_type{} -> {input_object, Tag};
        #interface_type{} -> {interface, Tag};
        not_found ->
            exit({schema_not_found, Tag})
    end.

ty_of(#{ varenv := VE }, Path, _, {var, ID}) ->
    Name = name(ID),
    case maps:get(Name, VE, not_found) of
        not_found ->
            gql_err:abort(Path, {unbound_variable, Name});
        {Ty, _Default} -> Ty
    end;
ty_of(_Ctx, Path, _, {enum, E}) ->
    N = name(E),
    case gql_schema:lookup_enum_type(N) of
        not_found -> gql_err:abort(Path, {unknown_enum, E});
        EnumTy -> {enum, EnumTy}
    end;
ty_of(_Ctx, _Path, {scalar, Tag}, V) ->
    case scalar(V) of
        true -> {scalar, Tag, V};
        false -> ty_of(_Ctx, _Path, undefined, V)
    end;
ty_of(_Ctx, _Path, _, {name, N, _}) -> N;
ty_of(_Ctx, _Path, _, S) when is_binary(S) -> {scalar, string, S};
ty_of(_Ctx, _Path, _, I) when is_integer(I) -> {scalar, int, I};
ty_of(_Ctx, _Path, _, F) when is_float(F) -> {scalar, float, F};
ty_of(_Ctx, _Path, _, true) -> {scalar, bool, true};
ty_of(_Ctx, _Path, _, false) -> {scalar, bool, false};
ty_of(_Ctx, _Path, _, {object, Fields}) -> {object, Fields};
ty_of(Ctx, Path, {list, Ty}, {list, Ts}) when is_list(Ts) ->
    {list, [ty_of(Ctx, Path, Ty, T) || T <- Ts]}.

ty_check(_Path, {scalar, Tag, V}, {scalar, Tag}) ->
    ok = builtin(Tag),
    {replace, V};
ty_check(Path, {scalar, Tag, V}, {scalar, Tag, IC}) ->
    case IC(V) of
        {ok, V} -> ok;
        {ok, NV} -> {replace, NV};
        {error, Reason} ->
            gql_err:abort(Path, {input_coercion, Tag, V, Reason})
    end;
ty_check(_Path, X, X) -> ok; %% Perfect match always wins

ty_check(Path, {non_null, A}, T) -> ty_check(Path, A, T);
ty_check(Path, A, {non_null, T}) -> ty_check(Path, A, T);
ty_check(Path, {list, As}, {list, T}) ->
    case lists:all(fun(X) -> ty_check(Path, X, T) == ok end, As) of
        true -> ok;
        false -> {error, {list, T}}
    end;
ty_check(Path, {object, Obj}, {input_object, Ty}) ->
    check_input_object(Path, Ty, input_object_type_scheme(Obj));
ty_check(_Path, A, T) -> {error, A, T}.

input_object_type_scheme(Obj) when is_list(Obj) ->
    TypSchema = in_obj_ty_scheme(Obj),
    maps:from_list(TypSchema).

in_obj_ty_scheme([]) -> [];
in_obj_ty_scheme([ {Key, Val} | Fields ]) ->
    N = name(Key),
    [{N, Val} | in_obj_ty_scheme(Fields) ].

%% -- VARENV -------------------------------------
tc_mk_varenv(Path, VDefs) ->
    maps:from_list(
       [{name(Var), {varenv_ty_coerce(Path, Ty), Def}}
           || #vardef { id = Var, ty = Ty, default = Def } <- VDefs]).

varenv_ty_coerce(Path, {ty, Ty}) -> varenv_ty_coerce(Path, Ty);
varenv_ty_coerce(_Path, {scalar, X}) -> {scalar, X};
varenv_ty_coerce(Path, {list, T}) -> {list, varenv_ty_coerce(Path, T)};
varenv_ty_coerce(Path, {non_null, T}) -> {non_null, varenv_ty_coerce(Path, T)};
varenv_ty_coerce(Path, T) ->
    N = name(T),
    case gql_schema:lookup(N) of
        not_found -> gql_err:abort(Path, {unknown_type, N});
        #enum_type{} -> {enum, N};
        #scalar_type{} -> {scalar, N};
        #input_object_type{} -> {input_object, N};
        #interface_type{} -> {interface, N}
    end.

%% -- AST MANIPULATION -------------------------
name('ROOT') -> <<"ROOT">>;
name({name, N, _}) -> N;
name({ty, Name}) -> name(Name);
name({var, N}) -> name(N).

%% True if input is a scalar value
scalar(S) when is_binary(S) -> true;
scalar(F) when is_float(F) -> true;
scalar(I) when is_integer(I) -> true;
scalar(true) -> true;
scalar(false) -> true;
scalar(_) -> false.

%% True for non-null inputs
non_null({non_null, _Ty}) -> true;
non_null(_) -> false.

%% Builtin types
builtin(string) -> ok;
builtin(id) -> ok;
builtin(float) -> ok;
builtin(int) -> ok;
builtin(bool) -> ok.
