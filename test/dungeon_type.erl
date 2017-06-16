-module(dungeon_type).
-include("dungeon.hrl").

-export([execute/1]).

execute(#monster{}) -> {ok, 'Monster'};
execute(X) -> 
    case dungeon:unwrap(X) of
        {Ty, _} ->
            {ok, object_type(Ty)}
    end.

object_type(room) -> 'Room';
object_type(monster) -> 'Monster';
object_type(item) -> 'Item'.

    
