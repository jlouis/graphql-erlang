-module(graphql_elaborate).

-include("graphql_schema.hrl").
-include("graphql_internal.hrl").

-export([x/1]).
-export([mk_varenv/1, mk_funenv/1]).
-export([err_msg/1]).

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

%% -- ROOT ----------------------------------------
root(Path, #op { ty = T } = Op) ->
    case graphql_schema:lookup('ROOT') of
        not_found -> err([Op | Path], no_root_schema);
        Schema -> graphql_schema:resolve_root_type(T, Schema)
    end.

%% -- FRAGMENTS -----------------------------------

%% Fragment elaboration splits into the two major cases. In case #1,
%% we have a situation where there is no type designator on the
%% fragment, but there is a schema type on the object already. In the
%% other case, we have a type, but no schema entry. For that variant,
%% we lookup the schema type, elaborate the fragment with the type and
%% recurse.
frag(Path, #frag { id = {name, _, _}, directives = [_|_] } = F) ->
    err([F | Path], directives_on_named_fragment);
frag(Path, #frag { ty = undefined, directives = Dirs } = Frag) ->
    frag_fields(Path,
          Frag#frag {
            directives = directives([Frag | Path], Dirs)
           });
frag(Path, #frag { ty = T, directives = Dirs } = Frag) ->
    Ty = graphql_ast:name(T),
    case graphql_schema:lookup(Ty) of
        not_found ->
            err([Frag | Path], {type_not_found, Ty});
        TypeSchema ->
            frag_fields(Path,
                  Frag#frag {
                    schema = TypeSchema,
                    directives = directives([Frag|Path], Dirs) })
    end.

%% Handle the fields in a fragment by looking at its object type
frag_fields(Path, #frag { schema = #object_type{ fields = Fields }} = F) ->
    fields([F | Path], F, Fields);
frag_fields(Path, #frag { schema = #interface_type{ fields = Fields }} = F) ->
    fields([F | Path], F, Fields);
frag_fields(Path, #frag { schema = #union_type{}} = F) ->
    %% Unions are always on the empty field set
    %% This should return quickly, but is here for consistency
    fields([F | Path], F, #{}).

%% -- OPERATIONS -----------------------------------

%% Operations are straightforward congruences: elaborate into the structure
op(Path, #op { vardefs = VDefs, directives = [] } = Op) ->
    RootSchema = root(Path, Op),
    case graphql_schema:lookup(RootSchema) of
        not_found ->
            err([Op | Path], {type_not_found, RootSchema});
        #object_type{ fields = Fields } = Obj ->
            fields([Op | Path], Op#op{ schema = Obj, vardefs = var_defs([Op | Path], VDefs) }, Fields)
    end;
op(Path, #op { id = {name, _, _}, directives = [_|_] } = Op) ->
    err([Op | Path], directives_on_op).

%% Determine the type of a vardef by looking it up in the schema
vdef_type({non_null, Ty}) ->
    case vdef_type(Ty) of
        {ok, V} -> {ok, {non_null, V}};
        {error, Reason} -> {error, Reason}
    end;
vdef_type({list, Ty}) ->
    case vdef_type(Ty) of
        {ok, V} -> {ok, {list, V}};
        {error, Reason} -> {error, Reason}
    end;
vdef_type({scalar, S}) -> {ok, {scalar, S}};
vdef_type({name, _, N}) -> vdef_type(N);
vdef_type(N) when is_binary(N) ->
    case graphql_schema:lookup(N) of
        not_found -> {error, not_found};
        #enum_type{} = Enum -> {ok, Enum};
        #scalar_type{} = Scalar -> {ok, Scalar};
        #input_object_type{} = IOType -> {ok, IOType};
        _Unknown -> {error, {invalid_input_type, N}}
    end.

%% Handle a list of vardefs by elaboration of their types
var_defs(Path, VDefs) ->
    [case vdef_type(V#vardef.ty) of
         {ok, Ty} -> V#vardef { ty = Ty };
         {error, not_found} -> err(Path, {type_not_found, graphql_ast:id(V)});
         {error, {invalid_input_type, T}} -> err(Path, {not_input_type, T})
     end || V <- VDefs].

%% -- DIRECTIVES -----------------------------------
directives(Path, Ds) ->
    try
        [directive(Path, D) || D <- Ds]
    catch
        throw:{unknown, Unknown} ->
            err(Path, {unknown_directive, Unknown})
    end.

directive(Path, #directive{ id = ID, args = Args } = D) ->
    Schema = #directive_type { args = SArgs } =
        case graphql_ast:name(ID) of
            <<"include">> ->
                graphql_builtins:directive_schema(include);
            <<"skip">> ->
                graphql_builtins:directive_schema(skip);
            _Name ->
                throw({unknown, D})
        end,
    D#directive { args = field_args([D | Path], Args, SArgs),
                  schema = Schema }.

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
    frag(Path, Frag#frag { schema = OType });
field(Path, _OType, #field { id = ID, args = Args, selection_set = SSet, directives = Dirs } = F, Fields) ->
    Name = graphql_ast:name(ID),
    ElabDirs = directives([F | Path], Dirs),
    case maps:get(Name, Fields, not_found) of
        not_found when Name == <<"__typename">> ->
            F#field { schema = {introspection, typename},
                      directives = ElabDirs };
        not_found ->
            err([F|Path], unknown_field);
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
            err(Path, {unknown_argument, N});
        #schema_arg{ ty = Ty } ->
            {K, #{ type => field_arg_type(Ty), value => V}}
    end.

field_arg_type({non_null, Ty}) -> {non_null, field_arg_type(Ty)};
field_arg_type({list, Ty}) -> {list, field_arg_type(Ty)};
field_arg_type({scalar, _} = Ty) -> Ty;
field_arg_type(#scalar_type{} = Ty) -> Ty;
field_arg_type({enum, _} = Ty) -> Ty;
field_arg_type(#enum_type{} = Ty) -> Ty;
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
    err(Path, unknown_field);
field_lookup(_Path, {scalar, _}, []) ->
    [];
field_lookup(Path, {scalar, _}, [_|_]) ->
    err(Path, selection_on_scalar);
field_lookup(Path, #scalar_type{}, [_|_]) ->
    err(Path, selection_on_scalar);
field_lookup(Path, #scalar_type{}, SSet) ->
    sset(Path, undefined, SSet, #{});
field_lookup(Path, #object_type{}, []) ->
    err(Path, fieldless_object);
field_lookup(Path, #object_type{ fields = Fields } = OType, SSet) ->
    sset(Path, OType, SSet, Fields);
field_lookup(Path, #interface_type{}, []) ->
    err(Path, fieldless_interface);
field_lookup(Path, #interface_type{ fields = Fields } = OType, SSet) ->
    sset(Path, OType, SSet, Fields);
field_lookup(Path, #union_type{} = OType, SSet) ->
    sset(Path, OType, SSet, #{});
field_lookup(_Path, #enum_type{}, []) ->
    [];
field_lookup(Path, #enum_type{}, _SSet) ->
    err(Path, selection_on_enum).

%% -- Error Handling
-spec err(term(), term()) -> no_return().
err(Path, Reason) ->
    graphql_err:abort(Path, elaborate, Reason).

err_msg({type_not_found, Ty}) ->
    ["Type not found in schema: ", Ty];
err_msg({not_input_type, Ty}) ->
    ["Type ", Ty, " is not an input type but is used in input-context"];
err_msg(directives_on_op) ->
    ["No support for directives on operation"];
err_msg(no_root_schema) ->
    ["No root schema found. One is required for correct operation"];
err_msg({unknown_field, F}) ->
    ["The query refers to a field, ", F, ", which is not present in the schema"];
err_msg(unknown_field) ->
    ["The query refers to a field which is not known"];
err_msg({unknown_argument, N}) ->
    ["The query refers to an argument, ", N, ", which is not present in the schema"];
err_msg({unknown_directive, Dir}) ->
    ["The query uses a directive, ", Dir, ", which is unknown to this GraphQL server"];
err_msg(selection_on_scalar) ->
    ["Cannot apply a selection set to a scalar field"];
err_msg(selection_on_enum) ->
    ["Cannot apply a selection set to an enum type"];
err_msg(fieldless_object) ->
    ["The path refers to an Object type, but no fields were specified"];
err_msg(fieldless_interface) ->
    ["The path refers to an Interface type, but no fields were specified"];
err_msg(directives_on_named_fragment) ->
    ["No support for directives on named fragments"].

