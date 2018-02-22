-module(dungeon_room).
-include("dungeon.hrl").

-export([execute/4]).

execute(_Ctx, #room { id = ID,
                      description = Desc,
                      contents = Contents }, F, _A) ->
    case F of
        <<"id">> -> dungeon:wrap({room, ID});
        <<"description">> -> graphql:throw({ok, Desc});
        <<"magic">> -> graphql:throw({error, unsupported});
        <<"leyline">> ->
            %% Force a crash to test the crash path
            X = true,
            X = false,
            {ok, 1};
        <<"contents">> -> {ok, [dungeon:load(OID) || OID <- Contents]}
    end.
