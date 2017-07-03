-module(graphql_elaborate).

-include("graphql_schema.hrl").
-include("graphql_internal.hrl").

-export([x/1]).
-export([mk_varenv/1, mk_funenv/1]).

-spec x(graphql:ast()) -> graphql:ast().
x(Doc) -> document(Doc).

document({document, Ops}) ->
    {document, operations([document], Ops)}.

operations(Path, Operations) ->
    [operation_(Path, Op) || Op <- Operations].

operation_(Path, #frag{} = F) -> frag(Path, F, undefined);
operation_(Path, #op{} = O) -> op(Path, O).

%% -- VARIABLE ENVIRONMENTS -----------------------

%% -- VARENV -------------------------------------
mk_varenv(VDefs) ->
    maps:from_list([varenv_coerce(Def) || Def <- VDefs]).

varenv_coerce(#vardef { id = Var } = VarDef) ->
    {graphql_ast:name(Var), VarDef}.

%% -- MK OF FUNENV ------------------------------

%% The function environment encodes a mapping from the name of a query
%% or mutation into the vars/params it accepts and their corresponding
%% type scheme. This allows us to look up a function call via the
%% variable environment later when we execute a given function in the
%% GraphQL Schema.

mk_funenv(Ops) ->
    F = fun
        (#frag{}, FE) -> FE;
        (#op { id = ID, vardefs = VDefs }, FE) ->
            Name = graphql_ast:name(ID),
            VarEnv = mk_varenv(VDefs),
            FE#{ Name => VarEnv }
    end,
    lists:foldl(F, #{}, Ops).

%% -- FRAGMENTS -----------------------------------
frag(Path, #frag { id = {name, _, _}, directives = [_|_] = Dirs } = F, _) ->
    graphql_err:abort([F | Path], {directives_on_named_fragment, Dirs});
frag(Path, #frag { ty = undefined, directives = Dirs } = Frag, ObjectType) ->
    frag_(Path,
          Frag#frag {
            ty = undefined,
            schema = ObjectType,
            directives = directives([Frag | Path], Dirs)
           });
frag(Path, #frag { ty = T, directives = Dirs } = F, _ObjecType) ->
    Ty = graphql_ast:name(T),
    case graphql_schema:lookup(Ty) of
        not_found ->
            graphql_err:abort([F | Path], {type_not_found, Ty});
        Schema ->
            frag_(Path, F#frag { schema = Schema, directives = directives([F|Path], Dirs) })
    end.

frag_(Path, #frag { schema = #object_type{ fields = Fields }} = F) ->
    fields([F | Path], F, Fields);
frag_(Path, #frag { schema = #interface_type{ fields = Fields }} = F) ->
    fields([F | Path], F, Fields);
frag_(Path, #frag { schema = #union_type{}} = F) ->
    %% Unions are always on the empty field set
    fields([F | Path], F, #{}).


%% -- OPERATIONS -----------------------------------

op(Path, #op { vardefs = VDefs, directives = [] } = Op) ->
    RootSchema = root(Path, Op),
    case graphql_schema:lookup(RootSchema) of
        not_found ->
            graphql_err:abort([Op | Path], {type_not_found, graphql_ast:name(RootSchema)});
        #object_type{ fields = Fields } = Obj ->
            fields([Op | Path], Op#op{ schema = Obj, vardefs = var_defs([Op | Path], VDefs) }, Fields)
    end;
op(Path, #op { directives = Dirs } = Op) ->
    graphql_err:abort([Op | Path], {directives_on_op, Dirs}).


vdef(#vardef { ty = Ty }) ->
    try vdef_type(Ty) of
        V -> V
    catch
        throw:Err -> Err
    end.
    
vdef_type({non_null, Ty}) ->
    {non_null, vdef_type(Ty)};
vdef_type({list, Ty}) ->
    {list, vdef_type(Ty)};
vdef_type({scalar, S}) -> {scalar, S};
vdef_type({name, _, N}) ->
    vdef_type(N);
vdef_type(N) when is_binary(N) ->
    case graphql_schema:lookup(N) of
        not_found -> throw(not_found);
        #enum_type{} = Enum -> Enum;
        #scalar_type{} = Scalar -> Scalar;
        #input_object_type{} = IOType -> IOType;
        _Obj -> throw({invalid_input_type, N})
    end.

var_defs(Path, VDefs) ->
    [case vdef(V) of
        not_found -> graphql_err:abort(Path, {type_not_found, graphql_ast:id(V)});
        {invalid_input_type, T} -> graphql_err:abort(Path, {not_input_type, T});
        Ty -> V#vardef { ty = Ty }
      end || V <- VDefs].

root(Path, #op { ty = T } = Op) ->
    case graphql_schema:lookup('ROOT') of
        not_found -> graphql_err:abort([Op | Path], no_root_schema);
        Schema -> graphql_schema:resolve_root_type(T, Schema)
    end.

%% -- DIRECTIVES -----------------------------------
directives(Path, Ds) ->
    try
        [directive(Path, D) || D <- Ds]
    catch
        throw:{unknown, Unknown} ->
            graphql_err:abort(Path, {unknown_directive, Unknown})
    end.

directive(Path, #directive{ id = ID, args = Args } = D) ->
    
    Schema = #directive_type { args = SArgs } =
        case graphql_ast:name(ID) of
            <<"include">> -> directive_schema(include);
            <<"skip">> -> directive_schema(skip);
            _Name -> throw({unknown, D})
        end,
    D#directive { args = field_args([D | Path], Args, SArgs),
                  schema = Schema }.

directive_schema(include) ->
    #directive_type {
       id = <<"include">>,
       args = #{
         <<"if">> =>
             #schema_arg{
                ty = graphql_schema:get(<<"Bool">>),
                default = false,
                description = <<"Wether or not the item should be included">> }
        }};
directive_schema(skip) ->
    #directive_type {
       id = <<"skip">>,
       args = #{
         <<"if">> =>
             #schema_arg{
                ty = graphql_schema:get(<<"Bool">>),
                default = false,
                description = <<"Wether or not the item should be skipped">> }
        }}.

%% -- SELECTION SETS -------------------------------

fields(Path, #frag { schema = OType, selection_set = SSet} = F, Fields) ->
    F#frag{ selection_set = sset(Path, OType, SSet, Fields)};
fields(Path, #op{ schema = OType, selection_set = SSet} = O, Fields) ->
    O#op{ selection_set = sset(Path, OType, SSet, Fields)}.

sset(Path, OType, SSet, Fields) ->
    [field(Path, OType, S, Fields) || S <- SSet].

field(Path, _OType, #frag_spread { directives = Dirs } = FragSpread, _Fields) ->
    ElabDirs = directives([FragSpread | Path], Dirs),
    FragSpread#frag_spread { directives = ElabDirs };
%% Inline fragments are elaborated the same way as fragments
field(Path, OType, #frag { id = '...' } = Frag, _Fields) ->
    frag(Path, Frag, OType);
field(Path, _OType, #field { id = ID, args = Args, selection_set = SSet, directives = Dirs } = F, Fields) ->
    Name = graphql_ast:name(ID),
    ElabDirs = directives([F | Path], Dirs),
    case maps:get(Name, Fields, not_found) of
        not_found when Name == <<"__typename">> ->
            F#field { schema = {introspection, typename},
                      directives = ElabDirs };
        not_found ->
            graphql_err:abort(Path, {unknown_field, Name});
        #schema_field{ ty = Ty, args = SArgs } = SF ->
            Type = field_type(Ty),
            SSet2 = field_lookup([F|Path], Type, SSet),
            F#field {
                args = field_args([F | Path], Args, SArgs),
                schema = SF#schema_field{ ty = Type },
                directives = ElabDirs,
                selection_set = SSet2 }
     end.

field_type({non_null, Ty}) -> {non_null, field_type(Ty)};
field_type({list, Ty}) -> {list, field_type(Ty)};
field_type({scalar, S}) -> {scalar, S};
field_type(B) when is_binary(B) ->
    case graphql_schema:lookup(B) of
        %% Non-polar
        #enum_type{} = Enum -> Enum;
        #scalar_type{} = Scalar -> Scalar;

        %% Output/Neg-polar
        #object_type{} = OT -> OT;
        #interface_type{} = IFace -> IFace;
        #union_type{} = Union -> Union
    end.

field_args(Path, Args, SArgs) ->
    [field_arg(Path, K, V, SArgs) || {K,V} <- Args].

field_arg(Path, K, V, SArgs) ->
    N = graphql_ast:name(K),
    case maps:get(N, SArgs, not_found) of
        not_found ->
            graphql_err:abort(Path, {argument_not_found, N});
        #schema_arg{ ty = Ty } ->
            {K, #{ type => field_arg_type(Ty), value => V}}
    end.

field_arg_type({non_null, Ty}) -> {non_null, field_arg_type(Ty)};
field_arg_type({list, Ty}) -> {list, field_arg_type(Ty)};
field_arg_type({scalar, _} = Ty) -> Ty;
field_arg_type(#scalar_type{} = Ty) -> Ty;
%% EDIT--Start
field_arg_type({enum, _} = Ty) -> Ty;
field_arg_type(#enum_type{} = Ty) -> Ty;
%% EDIT--End
field_arg_type(Ty) when is_binary(Ty) ->
    case graphql_schema:lookup(Ty) of
        #scalar_type{} = ScalarTy -> ScalarTy;
        #enum_type{} = EnumTy -> EnumTy;
        #input_object_type{} = IOType -> IOType
    end.


field_lookup(Path, {non_null, Obj}, SSet) ->
    field_lookup(Path, Obj, SSet);
field_lookup(Path, {list, Obj}, SSet) ->
    field_lookup(Path, Obj, SSet);
field_lookup(Path, not_found, _SSet) ->
    graphql_err:abort(Path, entity_not_found);
field_lookup(_Path, {scalar, _}, []) ->
    [];
field_lookup(Path, {scalar, _}, [_|_]) ->
    graphql_err:abort(Path, selection_on_scalar);
field_lookup(Path, #scalar_type{}, [_|_]) ->
    graphql_err:abort(Path, selection_on_scalar);
field_lookup(Path, #scalar_type{}, SSet) ->
    sset(Path, undefined, SSet, #{});
field_lookup(Path, #object_type{}, []) ->
    graphql_err:abort(Path, fieldless_object);
field_lookup(Path, #object_type{ fields = Fields } = OType, SSet) ->
    sset(Path, OType, SSet, Fields);
field_lookup(Path, #interface_type{}, []) ->
    graphql_err:abort(Path, fieldless_interface);
field_lookup(Path, #interface_type{ fields = Fields } = OType, SSet) ->
    sset(Path, OType, SSet, Fields);
field_lookup(Path, #union_type{} = OType, SSet) ->
    sset(Path, OType, SSet, #{});
field_lookup(_Path, #enum_type{}, []) ->
    [];
field_lookup(Path, #enum_type{}, _SSet) ->
    graphql_err:abort(Path, selection_set_on_enum).
