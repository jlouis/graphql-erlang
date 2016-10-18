-module(graphql_elaborate).

-include("graphql_schema.hrl").
-include("graphql_internal.hrl").

-export([x/1]).
-export([mk_varenv/2, mk_funenv/1]).

-spec x(graphql:ast()) -> graphql:ast().
x(Doc) -> document(Doc).

document({document, Ops}) ->
    {document, operations([document], Ops)}.

operations(Path, Operations) ->
    [operation_(Path, Op) || Op <- Operations].

operation_(Path, #frag{} = F) -> frag(Path, F);
operation_(Path, #op{} = O) -> op(Path, O).

%% -- VARIABLE ENVIRONMENTS -----------------------

%% -- VARENV -------------------------------------
mk_varenv(Path, VDefs) ->
    maps:from_list([varenv_coerce(Path, Def) || Def <- VDefs]).

varenv_coerce(Path, #vardef { id = Var, ty = T } = VarDef) ->
    case varenv_ty(T) of
        {ok, Type} ->
            {graphql_ast:name(Var), VarDef#vardef { ty = Type }};
        {error, Reason} ->
            graphql_err:abort(Path, Reason)
    end.

varenv_ty({scalar, X}) -> {ok, {scalar, X}};
varenv_ty({list, T}) ->
    case varenv_ty(T) of
        {ok, Ty} -> {ok, {list, Ty}};
        {error, Reason} -> {error, Reason}
    end;
varenv_ty({non_null, T}) ->
    case varenv_ty(T) of
        {ok, Ty} -> {ok, {non_null, Ty}};
        {error, Reason} -> {error, Reason}
    end;
varenv_ty(T) ->
    N = graphql_ast:name(T),
    case graphql_schema:lookup(N) of
        not_found -> {error, {unknown_type, N}};
        #enum_type{} = Enum -> {ok, Enum};
        #scalar_type{} = Scalar -> {ok, Scalar};
        #input_object_type{} = IOType -> {ok, IOType}
    end.

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


%% -- FRAGMENTS -----------------------------------
frag(Path, #frag { ty = T } = F) ->
   Ty = graphql_ast:name(T), %% This will always be a name
   case graphql_schema:lookup(Ty) of
       not_found ->
           graphql_err:abort([F | Path], {type_not_found, Ty});
       #object_type{ fields = Fields } = Obj ->
           fields([F | Path], F#frag { schema = Obj }, Fields);
       #interface_type{ fields = Fields } = IFace ->
           fields([F | Path], F#frag { schema = IFace }, Fields);
       #union_type {} = Union ->
           %% A union can only be elaborated on an empty field set:
           fields([F | Path], F#frag { schema = Union }, #{})
   end.

%% -- OPERATIONS -----------------------------------

op(Path, #op { vardefs = VDefs } = Op) ->
    RootSchema = root(Path, Op),
    case graphql_schema:lookup(RootSchema) of
        not_found ->
            graphql_err:abort([Op | Path], {type_not_found, RootSchema});
        #object_type{ fields = Fields } = Obj ->
            fields([Op | Path], Op#op{ schema = Obj, vardefs = var_defs([Op | Path], VDefs) }, Fields)
    end.

vdef(#vardef { ty = Ty }) ->
    case graphql_ast:unwrap_type(Ty) of
        {scalar, X} when
            X == int;
            X == string;
            X == bool;
            X == id -> builtin;
        TyName when is_binary(TyName) ->
            case graphql_schema:lookup(TyName) of
                not_found -> not_found;
                Obj -> Obj
            end
    end.

var_defs(Path, VDefs) ->
    [case vdef(V) of
        not_found -> graphql_err:abort(Path, {type_not_found, V});
        Obj -> V#vardef { schema = Obj }
      end || V <- VDefs].

root(Path, #op { ty = T } = Op) ->
    case graphql_schema:lookup('ROOT') of
        not_found -> graphql_err:abort([Op | Path], no_root_schema);
        Schema -> graphql_schema:resolve_root_type(T, Schema)
    end.

%% -- SELECTION SETS -------------------------------

fields(Path, #frag { selection_set = SSet} = F, Fields) ->
    F#frag{ selection_set = sset(Path, SSet, Fields)};
fields(Path, #op{ selection_set = SSet} = O, Fields) ->
    O#op{ selection_set = sset(Path, SSet, Fields)}.

sset(Path, SSet, Fields) ->
    [field(Path, S, Fields) || S <- SSet].

field(_Path, #frag_spread {} = FragSpread, _Fields) ->
    FragSpread;
%% Inline fragments are elaborated the same way as fragments
field(Path, #frag { id = '...' } = Frag, _Fields) ->
    frag(Path, Frag);
field(Path, #field { id = ID, args = Args, selection_set = SSet } = F, Fields) ->
    Name = graphql_ast:name(ID),
    case maps:get(Name, Fields, not_found) of
        not_found when Name == <<"__typename">> ->
            F#field { schema = {introspection, typename}, schema_obj = scalar};
        not_found ->
            graphql_err:abort(Path, {unknown_field, Name});
        #schema_field{ ty = Ty, args = SArgs } = SchemaTy ->
            case graphql_ast:unwrap_type(Ty) of
                {scalar, _} ->
                    F#field {
                      args = field_args([F | Path], Args, SArgs),
                      schema = SchemaTy,
                      schema_obj = scalar };
                T when is_binary(T) ->
                    {Obj, SSet2} = field_lookup([F | Path], T, SSet),
                    F#field {
                      args = field_args([F | Path], Args, SArgs),
                      schema = SchemaTy,
                      schema_obj = Obj,
                      selection_set = SSet2 }
            end
     end.

field_args(Path, Args, SArgs) ->
    [field_arg(Path, K, V, SArgs) || {K,V} <- Args].

field_arg(Path, K, V, SArgs) ->
    N = graphql_ast:name(K),
    case maps:get(N, SArgs, not_found) of
        not_found ->
            graphql_err:abort(Path, {argument_not_found, N});
        #schema_arg{ ty = Ty } ->
            {K, {field_arg_type(Ty), V}}
    end.

field_arg_type({non_null, Ty}) -> {non_null, field_arg_type(Ty)};
field_arg_type({scalar, _} = Ty) -> Ty;
field_arg_type(Ty) when is_binary(Ty) ->
    case graphql_schema:lookup(Ty) of
        #scalar_type{} = ScalarTy -> {scalar, ScalarTy};
        #enum_type{} -> Ty;
        #input_object_type{} -> Ty
    end.

field_lookup(Path, Ty, SSet) ->
    case graphql_schema:lookup(Ty) of
        not_found ->
            graphql_err:abort(Path, {entity_not_found, Ty});
        #scalar_type{} when SSet /= [] ->
            graphql_err:abort(Path, fields_on_scalar);
        #scalar_type{} = Scalar ->
            %% Written like this for coherence, we could skip it since SSet == []
            {Scalar, sset(Path, SSet, #{})};
        #object_type{} when SSet == [] ->
            graphql_err:abort(Path, fieldless_object);
        #object_type{ fields = Fields } = Obj ->
            {Obj, sset(Path, SSet, Fields)};
        #interface_type{} when SSet == [] ->
            graphql_err:abort(Path, fieldless_interface);
        #interface_type{ fields = Fields } = IFace ->
            {IFace, sset(Path, SSet, Fields)};
        #union_type{} = Union ->
            %% Any field lookup on a union will fail as a result of this
            {Union, sset(Path, SSet, #{})};
        #enum_type{} = Enum when SSet == [] ->
            {Enum, []};
        #enum_type{} ->
            graphql_err:abort(Path, {selection_set_on_enum, Ty})
    end.
