-module(graphql_elaborate).

-include("graphql_schema.hrl").
-include("graphql_internal.hrl").

-export([x/1]).
-export([type/1]).
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


%% -- TYPE ELABORATION -----------------------------------------------

%% Elaborate a type and also determine its polarity. This is used for
%% input and output types
type({non_null, Ty}) ->
    case type(Ty) of
        {error, Reason} -> {error, Reason};
        {Polarity, V} -> {Polarity, {non_null, V}}
    end;
type({list, Ty}) ->
    case type(Ty) of
        {error, Reason} -> {error, Reason};
        {Polarity, V} -> {Polarity, {list, V}}
    end;
type(#scalar_type{} = Ty) -> {'*', Ty};
type({enum, _} = E) -> {'*', E};
type(#enum_type{} = Ty) -> {'*', Ty};
type({name, _, N}) -> type(N);
type(N) when is_binary(N) ->
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
        #union_type{} = Union -> {'-', Union};
        
        _Unknown -> {error, {invalid_type, N}}
    end.

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
    frag_sset(Path,
          Frag#frag {
            directives = directives([Frag | Path], Dirs)
           });
frag(Path, #frag { ty = T, directives = Dirs } = Frag) ->
    Ty = graphql_ast:name(T),
    case graphql_schema:lookup(Ty) of
        not_found ->
            err([Frag | Path], {type_not_found, Ty});
        TypeSchema ->
            frag_sset(Path,
                  Frag#frag {
                    schema = TypeSchema,
                    directives = directives([Frag|Path], Dirs) })
    end.

%% Handle the fields in a fragment by looking at its object type
frag_sset(Path, #frag { schema = #object_type{ fields = Fields }} = F) ->
    sset([F | Path], F, Fields);
frag_sset(Path, #frag { schema = #interface_type{ fields = Fields }} = F) ->
    sset([F | Path], F, Fields);
frag_sset(Path, #frag { schema = #union_type{}} = F) ->
    %% Unions are always on the empty field set
    %% This should return quickly, but is here for consistency
    sset([F | Path], F, #{}).

%% -- OPERATIONS -----------------------------------

%% Operations are straightforward congruences: elaborate into the structure
op(Path, #op { vardefs = VDefs, directives = [] } = Op) ->
    RootSchema = root(Path, Op),
    case graphql_schema:lookup(RootSchema) of
        not_found ->
            err([Op | Path], {type_not_found, RootSchema});
        #object_type{ fields = Fields } = Obj ->
            sset([Op | Path], Op#op{ schema = Obj, vardefs = var_defs([Op | Path], VDefs) }, Fields)
    end;
op(Path, #op { id = {name, _, _}, directives = [_|_] } = Op) ->
    err([Op | Path], directives_on_op).


%% Handle a list of vardefs by elaboration of their types
var_defs(Path, VDefs) ->
    [case input_type(V#vardef.ty) of
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

%% A selection set is handled by recursing into the fragment or operation,
%% then process each field inside the selection set of that operation.
sset(Path, #frag { schema = OType, selection_set = SSet} = F, Fields) ->
    F#frag{ selection_set = [field(Path, OType, S, Fields) || S <- SSet] };
sset(Path, #op{ schema = OType, selection_set = SSet} = O, Fields) ->
    O#op{ selection_set = [field(Path, OType, S, Fields) || S <- SSet]}.

%% Fields are either fragment spreads, inline fragments, or fields. Recurse and
%% elaborate on the congruence in a straightforward way.
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
        %% Elaborate for the introspection system. __typename is always a valid name
        %% since it refers to the type of the object
        not_found when Name == <<"__typename">> ->
            F#field { schema = {introspection, typename},
                      directives = ElabDirs };
        not_found ->
            err([F|Path], unknown_field);
        #schema_field{ ty = Ty, args = SArgs } = SF ->
            {ok, Type} = output_type(Ty),
            SSet2 = field_sset([F|Path], Type, SSet),
            F#field {
                args = field_args([F | Path], Args, SArgs),
                schema = SF#schema_field{ ty = Type },
                directives = ElabDirs,
                selection_set = SSet2 }
     end.

field_args(Path, Args, SArgs) ->
    [field_arg(Path, K, V, SArgs) || {K,V} <- Args].

field_arg(Path, K, V, SArgs) ->
    N = graphql_ast:name(K),
    case maps:get(N, SArgs, not_found) of
        not_found ->
            err(Path, {unknown_argument, N});
        #schema_arg{ ty = Ty } ->
            {ok, ElabTy} = input_type(Ty),
            {K, #{ type => ElabTy, value => V}}
    end.

%% Evaluate the Type of a field and its selection set in order to
%% elaborate the selection set of fields according to the type given
field_sset(Path, {non_null, Obj}, SSet)                            -> field_sset(Path, Obj, SSet);
field_sset(Path, {list, Obj}, SSet)                                -> field_sset(Path, Obj, SSet);
field_sset(Path, not_found, _SSet)                                 -> err(Path, unknown_field);
field_sset(Path, #scalar_type{}, [_|_])                            -> err(Path, selection_on_scalar);
field_sset(Path, #scalar_type{}, SSet)                             -> [field(Path, undefined, S, #{}) || S <- SSet];
field_sset(Path, #object_type{}, [])                               -> err(Path, fieldless_object);
field_sset(Path, #object_type{ fields = Fields } = OType, SSet)    -> [field(Path, OType, S, Fields) || S <- SSet];
field_sset(Path, #interface_type{}, [])                            -> err(Path, fieldless_interface);
field_sset(Path, #interface_type{ fields = Fields } = IType, SSet) -> [field(Path, IType, S, Fields) || S <- SSet];
field_sset(Path, #union_type{} = UType, SSet)                      -> [field(Path, UType, S, #{}) || S <- SSet];
field_sset(_Path, #enum_type{}, [])                                -> [];
field_sset(Path, #enum_type{}, _SSet)                              -> err(Path, selection_on_enum).

%% -- ERROR HANDLING ------------------------------------------
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

