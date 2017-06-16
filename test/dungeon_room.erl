-module(dungeon_room).
-include("dungeon.hrl").

-export([execute/4]).

execute(_Ctx, #room { id = ID,
                      description = Desc,
                      contents = Contents }, F, A) ->
    case F of
        <<"id">> -> dungeon:wrap({room, ID});
        <<"description">> -> {ok, Desc};
        <<"contents">> -> {ok, [dungeon:load(OID) || OID <- Contents]}
    end.
