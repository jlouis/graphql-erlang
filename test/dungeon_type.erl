-module(dungeon_type).
-include("dungeon.hrl").

-export([execute/1]).

execute(#monster{}) -> {ok, 'Monster'};
execute(#dice{}) -> {ok, 'Dice'};
execute(#item{}) -> {ok, 'Item'};
execute(kraken) -> {error, kraken};
execute(X) ->
    case dungeon:unwrap(X) of
        {Ty, _} -> object_type(Ty)
    end.

object_type(room) -> {ok, 'Room'};
object_type(dice) -> {ok, 'Dice'};
object_type(monster) -> {ok, 'Monster'};
object_type(item) -> {ok, 'Item'};
object_type(_) ->
    {error, unknown}.
