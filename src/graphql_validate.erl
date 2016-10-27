-module(graphql_validate).

-include("graphql_internal.hrl").
-include("graphql_schema.hrl").

-export([x/1]).

-spec x(graphql:ast()) -> ok.
x(AST) -> 
    ok = unique_operations(AST),
    ok = no_fragment_cycles(AST),
    ok = variables_are_input_objects(AST),
    ok.

variables_are_input_objects({document, Ops}) ->
    Operations = [O || O = #op{} <- Ops],
    variables_are_input_objects(Operations);
variables_are_input_objects([]) -> ok;
variables_are_input_objects([#op { id = ID, vardefs = Defs } | Next]) ->
    case vardefs_are_input_objects(Defs) of
        ok -> variables_are_input_objects(Next);
        {wrong_type, VarID} ->
            err({wrong_input_type, graphql_ast:name(ID), VarID})
    end.

vardefs_are_input_objects([]) -> ok;
vardefs_are_input_objects([#vardef { id = ID, ty = Ty } | Next]) ->
    case polarity(Ty) of
        '+' -> vardefs_are_input_objects(Next);
        '*' -> vardefs_are_input_objects(Next);
        '-' -> {wrong_type, ID}
    end.

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
            err({cycles_in_fragments, Cycles})
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
uniq_([X,X | _Xs]) -> err({not_unique, X});
uniq_([_, X | Xs]) -> uniq([X | Xs]).

%% Type polarity. Types are moded according to this scheme.
polarity({non_null, T}) -> polarity(T);
polarity({list, T}) -> polarity(T);
polarity(#input_object_type{}) -> '+';
polarity(#object_type{}) -> '-';
polarity(#interface_type{}) -> '-';
polarity(#union_type{}) -> '-';
polarity(#enum_type{}) -> '*';
polarity(#scalar_type{}) -> '*';
polarity({scalar, _}) -> '*';
polarity({name, _, N}) ->
    T = graphql_schema:lookup(N),
    polarity(T).

%% Errors
err(Reason) -> exit({validate, Reason}).
