-module(graphql_elaborate).

-include_lib("graphql/include/graphql.hrl").
-include("graphql_schema.hrl").
-include("graphql_internal.hrl").

-export([x/1]).
-export([mk_varenv/1, mk_funenv/1]).

-spec x(graphql:ast()) -> graphql:ast().
x(Doc) -> document(Doc).

document(#document { definitions = Defs }) ->
    #document { definitions = operations([document], Defs)}.

operations(Path, Operations) ->
    [operation_(Path, Op) || Op <- Operations].

operation_(Path, #frag{} = F) -> frag(Path, fragment_definition, F).

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
frag(Path, Context, #frag { ty = undefined, directives = Dirs } = Frag) ->
    frag_sset(Path,
          Frag#frag {
            directives = directives([Frag | Path], Context, Dirs)
           });
frag(Path, Context, #frag { ty = T, directives = Dirs } = Frag) ->
    Ty = graphql_ast:name(T),
    case graphql_schema:lookup(Ty) of
        not_found ->
            err([Frag | Path], {type_not_found, Ty});
        TypeSchema ->
            frag_sset(Path,
                  Frag#frag {
                    schema = TypeSchema,
                    directives = directives([Frag|Path], Context, Dirs) })
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
    ElabDirs = directives([FragSpread | Path], fragment_spread, Dirs),
    FragSpread#frag_spread { directives = ElabDirs };
%% Inline fragments are elaborated the same way as fragments
field(Path, OType, #frag { id = '...' } = Frag, _Fields) ->
    frag(Path, inline_fragment, Frag#frag { schema = OType }).


%% -- ERROR HANDLING ------------------------------------------
-spec err(term(), term()) -> no_return().
err(Path, Reason) ->
    graphql_err:abort(Path, elaborate, Reason).

