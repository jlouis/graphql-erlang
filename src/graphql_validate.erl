-module(graphql_validate).

-include("graphql_internal.hrl").

-export([x/1]).

-spec x(graphql:ast()) -> ok.
x(AST) -> 
    ok = unique_operations(AST),
    ok = no_fragment_cycles(AST),
    ok.

unique_operations({document, Ops}) ->
    OpIDs = [graphql_ast:name(ID) || #op{ id = ID } <- Ops],
    ok = uniq(lists:sort(OpIDs)),
    FragIDs = [graphql_ast:name(ID) || #frag { id = ID } <- Ops],
    ok = uniq(lists:sort(FragIDs)),
    ok.

no_fragment_cycles({document, Ops}) ->
    Frags = [Frag || Frag = #frag{} <- Ops],
    Links = sofs:family([frag_link(F) || F <- Frags]),
    G = sofs:family_to_digraph(Links, [private]),
    try digraph_utils:cyclic_strong_components(G) of
        [] -> ok;
        Cycles ->
            graphql_err:abort({cycles_in_fragments, Cycles})
    after
        digraph:delete(G)
    end,
    ok.
    
frag_link(#frag { id = ID, selection_set = Fields }) ->
    {graphql_ast:name(ID), frag_link_fields(Fields)}.

frag_link_fields([]) -> [];
frag_link_fields([#field{ selection_set = Fields } | Next]) ->
    frag_link_fields(Fields) ++ frag_link_fields(Next);
frag_link_fields([#frag_spread { id = ID } | Next]) -> [graphql_ast:name(ID) | frag_link_fields(Next)];
frag_link_fields([#frag { id = '...', selection_set = Fields } | Next]) ->
    frag_link_fields(Fields) ++ frag_link_fields(Next).
    
%% Uniqueness among a set of names
uniq([]) -> ok;
uniq(L) -> uniq_(L).

uniq_([_]) -> ok;
uniq_([X,X | _Xs]) -> graphql_err:abort([], {not_unique, X});
uniq_([_, X | Xs]) -> uniq([X | Xs]).

