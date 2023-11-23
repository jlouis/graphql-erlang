-module(dungeon_item).
-include("dungeon.hrl").

-export([execute/4]).

execute(_Ctx, #item { id = ID,
                      name = Name,
                      contents = Contents,
                      description = Description } = Item, Field, _) ->
    case Field of
        <<"id">> -> dungeon:wrap({item, ID});
        <<"name">> -> {ok, Name};
        <<"description">> -> {ok, Description};
        <<"weight">> ->
            graphql:throw({ok, weight(Item)});
        <<"weightSum">> -> {ok, weight_sum(Item)};
        <<"contents">> ->
            {ok, [dungeon:load(OID) || OID <- Contents]}
    end.

weight(#item { weight = W }) -> W;
weight(_) -> 0.0.

weight_sum(#item { weight = W, contents = C }) ->
    W + lists:sum([dungeon:load(OID) || OID <- C]);
weight_sum(_) -> 0.0.

