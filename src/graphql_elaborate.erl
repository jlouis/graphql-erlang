-module(graphql_elaborate).

-include("graphql_schema.hrl").
-include("graphql_internal.hrl").

-export([x/2]).
-export([type/2]).
-export([mk_varenv/1, mk_funenv/1]).

-spec x(graphql_schema:endpoint_context(), graphql:ast()) -> graphql:ast().
x(EP, Doc) -> document(EP, Doc).

document(EP, {document, Ops}) ->
    {document, operations(EP, [document], Ops)}.

operations(EP, Path, Operations) ->
    [operation_(EP, Path, Op) || Op <- Operations].

operation_(EP, Path, #frag{} = F) -> frag(EP, Path, fragment_definition, F);
operation_(EP, Path, #op{} = O) -> op(EP, Path, O).

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
type(EP, {non_null, Ty}) ->
    case type(EP, Ty) of
        {error, Reason} -> {error, Reason};
        {Polarity, V} -> {Polarity, {non_null, V}}
    end;
type(EP, {list, Ty}) ->
    case type(EP, Ty) of
        {error, Reason} -> {error, Reason};
        {Polarity, V} -> {Polarity, {list, V}}
    end;
type(EP, {scalar, Name}) ->
    #scalar_type{} = Ty = graphql_schema:get(EP, Name),
    {_polarity, Ty} = type(EP, Ty);
type(_EP, #scalar_type{} = Ty) -> {'*', Ty};
type(_EP, {enum, _} = E) -> {'*', E};
type(_EP, #enum_type{} = Ty) -> {'*', Ty};
type(EP, {name, _, N}) -> type(EP, N);
type(EP, N) when is_binary(N) ->
    case graphql_schema:lookup(EP, N) of
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

%% Assert a type is an input type
input_type(EP, Ty) ->
    case type(EP, Ty) of
        {error, Reason} -> {error, Reason};
        {'*', V} -> {ok, V};
        {'+', V} -> {ok, V};
        {'-', _} -> {error, {invalid_input_type, Ty}}
    end.

%% Assert a type is an output type
output_type(EP, Ty) ->
    case type(EP, Ty) of
        {error, Reason} -> {error, Reason};
        {'*', V} -> {ok, V};
        {'-', V} -> {ok, V};
        {'+', _} -> {error, {invalid_output_type, Ty}}
    end.


%% -- ROOT ----------------------------------------
root(EP, Path, #op { ty = T } = Op) ->
    case graphql_schema:lookup(EP, 'ROOT') of
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
frag(EP, Path, Context, #frag { ty = undefined, directives = Dirs } = Frag) ->
    frag_sset(EP, Path,
          Frag#frag {
            directives = directives(EP, [Frag | Path], Context, Dirs)
           });
frag(EP, Path, Context, #frag { ty = T, directives = Dirs } = Frag) ->
    Ty = graphql_ast:name(T),
    case graphql_schema:lookup(EP, Ty) of
        not_found ->
            err([Frag | Path], {type_not_found, Ty});
        TypeSchema ->
            frag_sset(EP, Path,
                  Frag#frag {
                    schema = TypeSchema,
                    directives = directives(EP, [Frag|Path], Context, Dirs) })
    end.

%% Handle the fields in a fragment by looking at its object type
frag_sset(EP, Path, #frag { schema = #object_type{ fields = Fields }} = F) ->
    sset(EP, [F | Path], F, Fields);
frag_sset(EP, Path, #frag { schema = #interface_type{ fields = Fields }} = F) ->
    sset(EP, [F | Path], F, Fields);
frag_sset(EP, Path, #frag { schema = #union_type{}} = F) ->
    %% Unions are always on the empty field set
    %% This should return quickly, but is here for consistency
    sset(EP, [F | Path], F, #{}).

%% -- OPERATIONS -----------------------------------

%% Determine the kind of operation context we have from a type
operation_context(undefined) -> query;
operation_context({query, _}) -> query;
operation_context({mutation, _}) -> mutation;
operation_context({subscription, _}) -> subscription.

%% Operations are straightforward congruences: elaborate into the structure
op(EP, Path, #op { ty = Ty, vardefs = VDefs, directives = Dirs } = Op) ->
    RootSchema = root(EP, Path, Op),
    case graphql_schema:lookup(EP, RootSchema) of
        not_found ->
            err([Op | Path], {type_not_found, RootSchema});
        #object_type{ fields = Fields } = Obj ->
            OperationType = operation_context(Ty),
            sset(EP, [Op | Path],
                 Op#op{ schema = Obj,
                        directives = directives(EP,
                                                [Op | Path], OperationType, Dirs),
                        vardefs = var_defs(EP, [Op | Path], VDefs) }, Fields)
    end.

%% Handle a list of vardefs by elaboration of their types
var_defs(EP, Path, VDefs) ->
    [case input_type(EP, V#vardef.ty) of
         {ok, Ty} -> V#vardef { ty = Ty };
         {error, not_found} -> err(Path, {type_not_found, graphql_ast:id(V)});
         {error, {invalid_input_type, T}} -> err(Path, {not_input_type, T})
     end || V <- VDefs].

%% -- DIRECTIVES -----------------------------------
directives(EP, Path, Context, Ds) ->
    NamedDirectives = [{graphql_ast:name(ID), D} 
                       || #directive { id = ID } = D <- Ds],
    case graphql_ast:uniq(NamedDirectives) of
        ok ->
            try [directive(EP, Path, Context, D) || D <- Ds]
            catch throw:{unknown, Unknown} ->
                    err(Path, {unknown_directive, graphql_ast:id(Unknown)})
            end;
        {not_unique, X} ->
            err(Path, {directives_not_unique, X})
    end.

directive(EP, Path, Context, #directive{ id = ID, args = Args } = D) ->
    Schema = #directive_type { args = SArgs,
                               locations = Locations } =
        case graphql_ast:name(ID) of
            <<"include">> ->
                graphql_builtins:directive_schema(EP, include);
            <<"skip">> ->
                graphql_builtins:directive_schema(EP, skip);
            _Name ->
                throw({unknown, D})
        end,
    case lists:member(Context, Locations) of
        true ->
            D#directive { args = field_args(EP, [D | Path], Args, SArgs),
                          schema = Schema };
        false ->
            err(Path, {invalid_directive_location, graphql_ast:name(ID), Context})
    end.

%% -- SELECTION SETS -------------------------------

%% A selection set is handled by recursing into the fragment or operation,
%% then process each field inside the selection set of that operation.
sset(EP, Path, #frag { schema = OType, selection_set = SSet} = F, Fields) ->
    F#frag{ selection_set = [field(EP, Path, OType, S, Fields) || S <- SSet] };
sset(EP, Path, #op{ schema = OType, selection_set = SSet} = O, Fields) ->
    O#op{ selection_set = [field(EP, Path, OType, S, Fields) || S <- SSet]}.

%% Fields are either fragment spreads, inline fragments, or fields. Recurse and
%% elaborate on the congruence in a straightforward way.
field(EP, Path, _OType, #frag_spread { directives = Dirs } = FragSpread, _Fields) ->
    ElabDirs = directives(EP, [FragSpread | Path], fragment_spread, Dirs),
    FragSpread#frag_spread { directives = ElabDirs };
%% Inline fragments are elaborated the same way as fragments
field(EP, Path, OType, #frag { id = '...' } = Frag, _Fields) ->
    frag(EP, Path, inline_fragment, Frag#frag { schema = OType });
field(EP, Path, _OType, #field { id = ID, args = Args, selection_set = SSet, directives = Dirs } = F, Fields) ->
    Name = graphql_ast:name(ID),
    ElabDirs = directives(EP, [F | Path], field, Dirs),
    case maps:get(Name, Fields, not_found) of
        %% Elaborate for the introspection system. __typename is always a valid name
        %% since it refers to the type of the object
        not_found when Name == <<"__typename">> ->
            F#field { schema = {introspection, typename},
                      directives = ElabDirs };
        not_found ->
            err([F|Path], unknown_field);
        #schema_field{ ty = Ty, args = SArgs } = SF ->
            {ok, Type} = output_type(EP, Ty),
            SSet2 = field_sset(EP, [F|Path], Type, SSet),
            F#field {
                args = field_args(EP, [F | Path], Args, SArgs),
                schema = SF#schema_field{ ty = Type },
                directives = ElabDirs,
                selection_set = SSet2 }
     end.

field_args(EP, Path, Args, SArgs) ->
    [field_arg(EP, Path, K, V, SArgs) || {K,V} <- Args].

field_arg(EP, Path, K, V, SArgs) ->
    N = graphql_ast:name(K),
    case maps:get(N, SArgs, not_found) of
        not_found ->
            err(Path, {unknown_argument, N});
        #schema_arg{ ty = Ty } ->
            {ok, ElabTy} = input_type(EP, Ty),
            {K, #{ type => ElabTy, value => V}}
    end.

%% Evaluate the Type of a field and its selection set in order to
%% elaborate the selection set of fields according to the type given
field_sset( EP, Path, {non_null, Obj}, SSet)                            -> field_sset(EP, Path, Obj, SSet);
field_sset( EP, Path, {list, Obj}, SSet)                                -> field_sset(EP, Path, Obj, SSet);
field_sset(_EP, Path, not_found, _SSet)                                 -> err(Path, unknown_field);
field_sset(_EP, Path, #scalar_type{}, [_|_])                            -> err(Path, selection_on_scalar);
field_sset( EP, Path, #scalar_type{}, SSet)                             -> [field(EP, Path, undefined, S, #{}) || S <- SSet];
field_sset(_EP, Path, #object_type{}, [])                               -> err(Path, fieldless_object);
field_sset( EP, Path, #object_type{ fields = Fields } = OType, SSet)    -> [field(EP, Path, OType, S, Fields) || S <- SSet];
field_sset(_EP, Path, #interface_type{}, [])                            -> err(Path, fieldless_interface);
field_sset( EP, Path, #interface_type{ fields = Fields } = IType, SSet) -> [field(EP, Path, IType, S, Fields) || S <- SSet];
field_sset( EP, Path, #union_type{} = UType, SSet)                      -> [field(EP, Path, UType, S, #{}) || S <- SSet];
field_sset(_EP,_Path, #enum_type{}, [])                                -> [];
field_sset(_EP, Path, #enum_type{}, _SSet)                              -> err(Path, selection_on_enum).

%% -- ERROR HANDLING ------------------------------------------
-spec err(term(), term()) -> no_return().
err(Path, Reason) ->
    graphql_err:abort(Path, elaborate, Reason).

