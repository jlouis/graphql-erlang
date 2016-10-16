-module(graphql_elaborate).

-include("graphql_schema.hrl").
-include("graphql_internal.hrl").

-export([x/1]).

-spec x(graphql:ast()) -> graphql:ast().
x(Doc) -> document(Doc).

document({document, Ops}) ->
    {document, ops([document], Ops)}.

ops(_Path, []) -> [];
ops(Path, [#frag {} = F | OPs]) ->
    [frag([F | Path], F) | ops(Path, OPs)];
ops(Path, [#op{} = O | OPs]) ->
    [op([O | Path], O) | ops(Path, OPs)].

%% -- FRAGMENTS -----------------------------------
frag(Path, #frag { ty = T } = F) ->
   Ty = graphql_ast:name(T),
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
            fields(Path, Op#op{ schema = Obj, vardefs = vdefs([Op | Path], VDefs) }, Fields)
    end.

vdef_lookup(Path, N) ->
    case graphql_schema:lookup(N) of
        not_found -> graphql_err:abort(Path, {type_not_found, N});
        Obj -> Obj
    end.

vdefs(_Path, []) -> [];
vdefs(Path, [#vardef { id = ID, ty = Ty } = V | Vs]) ->
    Obj = case unwrap_type(Ty) of
        {scalar, X} when
            X == int;
            X == string;
            X == bool;
            X == id -> builtin;
        {scalar, TyName} ->
            vdef_lookup([ID | Path], TyName);
        TyName ->
            vdef_lookup([ID | Path], TyName)
    end,
    [V#vardef { schema = Obj } | vdefs(Path, Vs)].

unwrap_type({scalar, Ty}) -> {scalar, Ty};
unwrap_type({name, N, _}) -> N;
unwrap_type({non_null, Ty}) -> unwrap_type(Ty);
unwrap_type({list, Ty}) -> unwrap_type(Ty).

root(Path, #op { ty = T } = Op) ->
    case graphql_schema:lookup('ROOT') of
        #root_schema { query = Q, mutation = M, subscription = S } ->
            graphql_ast:resolve_root_type(T, Q, M, S);
        not_found ->
            graphql_err:abort([Op | Path], no_root_schema)
    end.

%% -- SELECTION SETS -------------------------------

fields(Path, #frag { selection_set = SSet} = F, Fields) ->
    F#frag{ selection_set = sset(Path, SSet, Fields)};
fields(Path, #op{ selection_set = SSet} = O, Fields) ->
    O#op{ selection_set = sset(Path, SSet, Fields)}.

sset(Path, SSet, Fields) ->
    [field(Path, S, Fields) || S <- SSet].
    
field(_Path, #frag_spread {} = FragSpread, _Fields) -> FragSpread;
%% Inline fragments are elaborated the same way as fragments
field(Path, #frag { id = '...' } = Frag, _Fields) -> frag(Path, Frag);
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
        #scalar_type{} -> {scalar, Ty};
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
