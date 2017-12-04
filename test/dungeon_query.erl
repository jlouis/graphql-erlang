-module(dungeon_query).
-include_lib("stdlib/include/qlc.hrl").

-include("dungeon.hrl").


-export([execute/4]).

execute(_Ctx, _, <<"monster">>, #{ <<"id">> := InputID }) ->
    case dungeon:unwrap(InputID) of
        {monster, _ID} = OID ->
            dungeon:dirty_load(OID)
    end;
execute(_Ctx, _, <<"monsters">>, #{ <<"ids">> := InputIDs }) ->
    {ok, [begin
              {monster, _} = OID = dungeon:unwrap(ID),
              dungeon:dirty_load(OID)
          end || ID <- InputIDs]};
execute(_Ctx, _, <<"findMonsters">>, #{ <<"moods">> := Moods }) ->
    QH = qlc:q([M || M <- mnesia:table(monster),
                     lists:member(M#monster.mood, Moods)]),
    {ok, Monsters} = dungeon:load_txn(QH),
    {ok, [{ok, M} || M <- Monsters]};
execute(_Ctx, _, <<"thing">>, #{ <<"id">> := InputID }) ->
    case dungeon:unwrap(InputID) of
        {monster, _ID} = OID -> dungeon:dirty_load(OID);
        {item, _ID} = OID -> dungeon:dirty_load(OID);
        {kraken, _ID} = _OID -> {ok, kraken}
    end;
execute(_Ctx, _, <<"room">>, #{ <<"id">> := InputID }) ->
    case dungeon:unwrap(InputID) of
        {room, _ID} = OID -> dungeon:dirty_load(OID)
    end;
execute(_Ctx, _, <<"rooms">>, #{ <<"ids">> := InputIDs }) ->
    {ok, [begin
              {room, _} = OID = dungeon:unwrap(ID),
              dungeon:dirty_load(OID)
          end || ID <- InputIDs]};
execute(Ctx, _, <<"roll">>, Args) ->
    TimeOut = maps:get(<<"delay">>,  Args, 0),
    {ok, #dice{ delay = TimeOut } }.

