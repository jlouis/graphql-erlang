-module(graphql_elaborate).

-include("graphql_schema.hrl").
-include("graphql_internal.hrl").

-export([x/1]).

-spec x(gql:ast()) -> gql:ast().
x(Doc) -> x_document(Doc).

x_document({document, Ops}) ->
    {document, x_ops([document], Ops)}.

x_ops(_Path, []) -> [];
x_ops(Path, [#frag {} = F | OPs]) ->
    [x_frag([F | Path], F) | x_ops(Path, OPs)];
x_ops(Path, [#op{} = O | OPs]) ->
    [x_op([O | Path], O) | x_ops(Path, OPs)].

%% -- FRAGMENTS -----------------------------------
x_frag(Path, #frag { ty = T } = F) ->
   Ty = name(T),
   case graphql_schema:lookup(Ty) of
       not_found ->
           graphql_err:abort([F | Path], {type_not_found, Ty});
       #object_type{ fields = Fields } = Obj ->
           x_fields([F | Path], F#frag { schema = Obj }, Fields);
       #interface_type{ fields = Fields } = IFace ->
           x_fields([F | Path], F#frag { schema = IFace }, Fields);
       #union_type {} = Union ->
           %% A union can only be elaborated on an empty field set:
           x_fields([F | Path], F#frag { schema = Union }, #{})
   end.

%% -- OPERATIONS -----------------------------------

x_op(Path, #op { vardefs = VDefs } = Op) ->
    RootSchema = x_root(Path, Op),
    case graphql_schema:lookup(RootSchema) of
        not_found ->
            graphql_err:abort([Op | Path], {type_not_found, RootSchema});
        #object_type{ fields = Fields } = Obj ->
            x_fields(Path, Op#op{ schema = Obj, vardefs = vdefs([Op | Path], VDefs) }, Fields)
    end.

vdef_lookup(Path, N) ->
    case graphql_schema:lookup(N) of
        not_found -> graphql_err:abort(Path, {type_not_found, N});
        Obj -> Obj
    end.

vdefs(_Path, []) -> [];
vdefs(Path, [#vardef { id = ID, ty = {ty, Ty} } = V | Vs]) ->
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

x_root(Path, #op { ty = T } = Op) ->
    case graphql_schema:lookup('ROOT') of
        #root_schema { query = Q, mutation = M, subscription = S } ->
            graphql_ast:resolve_root_type(T, Q, M, S);
        not_found ->
            graphql_err:abort([Op | Path], no_root_schema)
    end.

%% -- SELECTION SETS -------------------------------

x_fields(Path, #frag { selection_set = SSet} = F, Fields) ->
    F#frag{ selection_set = x_sset(Path, SSet, Fields)};
x_fields(Path, #op{ selection_set = SSet} = O, Fields) ->
    O#op{ selection_set = x_sset(Path, SSet, Fields)}.

x_sset(Path, SSet, Fields) ->
    [x_field(Path, S, Fields) || S <- SSet].
    
x_field(_Path, #frag_spread {} = FragSpread, _Fields) -> FragSpread;
%% Inline fragments are elaborated the same way as fragments
x_field(Path, #frag { id = '...' } = Frag, _Fields) -> x_frag(Path, Frag);
x_field(Path, #field { id = ID, args = Args, selection_set = SSet } = F, Fields) ->
    Name = name(ID),
    case maps:get(Name, Fields, not_found) of
        not_found when Name == <<"__typename">> ->
            F#field { schema = {introspection, typename}, schema_obj = scalar};
        not_found ->
            graphql_err:abort(Path, {unknown_field, Name});
        #schema_field{ ty = Ty, args = SArgs } = SchemaTy ->
            case graphql_ast:unwrap_type(Ty) of
                {scalar, _} ->
                    F#field {
                          args = x_field_args([F | Path], Args, SArgs),
                          schema = SchemaTy,
                          schema_obj = scalar };
                T when is_binary(T) ->
                    {Obj, SSet2} = x_field_lookup([F | Path], T, SSet),
                    F#field {
                          args = x_field_args([F | Path], Args, SArgs),
                    	schema = SchemaTy,
                    	schema_obj = Obj,
                    	selection_set = SSet2
                    }
            end
     end.

x_field_args(Path, Args, SArgs) ->
    [x_field_arg(Path, K, V, SArgs) || {K,V} <- Args].

x_field_arg(Path, K, V, SArgs) ->
    N = name(K),
    case maps:get(N, SArgs, not_found) of
        not_found ->
            graphql_err:abort(Path, {argument_not_found, N});
        #schema_arg{ ty = Ty } ->
            {K, {Ty, V}}
    end.

x_field_lookup(Path, Ty, SSet) ->
    case graphql_schema:lookup(Ty) of
        not_found ->
            graphql_err:abort(Path, {entity_not_found, Ty});
        #scalar_type{} when SSet /= [] ->
            graphql_err:abort(Path, fields_on_scalar);
        #scalar_type{} = Scalar ->
            %% Written like this for coherence, we could skip it since SSet == []
            {Scalar, x_sset(Path, SSet, #{})};
        #object_type{} when SSet == [] ->
            graphql_err:abort(Path, fieldless_object);
        #object_type{ fields = Fields } = Obj ->
            {Obj, x_sset(Path, SSet, Fields)};
        #interface_type{} when SSet == [] ->
            graphql_err:abort(Path, fieldless_interface);
        #interface_type{ fields = Fields } = IFace ->
            {IFace, x_sset(Path, SSet, Fields)};
        #union_type{} = Union ->
            %% Any field lookup on a union will fail as a result of this
            {Union, x_sset(Path, SSet, #{})};
        #enum_type{} = Enum when SSet == [] ->
            {Enum, []};
        #enum_type{} ->
            graphql_err:abort(Path, {selection_set_on_enum, Ty})
    end.
    
%% -- AST MANIPULATION ---------------------------
name('ROOT') -> <<"ROOT">>;
name({name, N, _}) -> N;
name({ty, Name}) -> name(Name);
name({var, N}) -> name(N).
