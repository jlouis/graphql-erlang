-module(graphql_elaborate).

-include_lib("graphql/include/graphql.hrl").
-include("graphql_schema.hrl").
-include("graphql_internal.hrl").

-export([mk_varenv/1, mk_funenv/1]).


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

