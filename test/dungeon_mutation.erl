-module(dungeon_mutation).
-include("dungeon.hrl").

-export([execute/4]).

execute(_Ctx, _, <<"introduceRoom">>, #{ <<"input">> := Input }) ->
    #{ <<"clientMutationId">> := MID,
       <<"description">> := D } = Input,
    {atomic, Room} = dungeon:insert(#room { description = D }),
    {ok, #{
        <<"clientMutationId">> => MID,
        <<"room">> => Room } };
execute(_Ctx, _, <<"introduceMonster">>, #{ <<"input">> := Input }) ->
    #{ <<"clientMutationId">> := MID,
       <<"name">> := N,
       <<"color">> := #{} = C,
       <<"hitpoints">> := HP,
       <<"stats">> := Stats,
       <<"mood">> := M,
       <<"properties">> := Props,
       <<"plushFactor">> := PF
    } = Input,
    Ss = input_stats(Stats),
    case is_atom(M) of
        true -> ok;
        false ->
            exit({bad_mood_value, M})
    end,
    PlushFactor = case PF of
                      null -> 0.01;
                      PlushValue when is_float(PlushValue) -> PlushValue
                  end,
    {atomic, Monster} = dungeon:insert(#monster {
    	properties = Props,
    	plush_factor = PlushFactor,
    	stats = Ss,
    	name = N,
    	color = C,
    	hitpoints = HP,
    	mood = M }),
    {ok, #{
        <<"clientMutationId">> => MID,
        <<"monster">> => Monster } };
execute(_Ctx, _, <<"introduceItem">>, #{ <<"input">> := Input }) ->
    #{ <<"clientMutationId">> := MID,
       <<"name">> := N,
       <<"description">> := D,
       <<"weight">> := W } = Input,
    {atomic, Item} = dungeon:insert(
                       #item { name = N,
                               description = D,
                               weight = W }),
    {ok, #{
       <<"clientMutationId">> => MID,
       <<"item">> => Item } };
execute(_Ctx, _, <<"spawnMinion">>, #{ <<"input">> := Input }) ->
    #{ <<"clientMutationId">> := MID,
       <<"monsterId">> := MonsterID,
       <<"roomId">> := RoomID } = Input,
    Txn = fun() ->
                  [Room = #room{ contents = Contents }] =
                      mnesia:read(dungeon:unwrap(RoomID)),
                  [Monster] = mnesia:read(dungeon:unwrap(MonsterID)),
                  NewRoom =
                      Room#room {
                        contents = ordsets:add_element(
                                     {monster, Monster#monster.id},
                                     Contents) },
                  ok = mnesia:write(NewRoom),
                  #{ <<"monster">> => Monster,
                     <<"room">> => NewRoom }
          end,
    {atomic, Result} = mnesia:transaction(Txn),
    {ok, Result#{ <<"clientMutationId">> => MID }}.
    

%% -- INTERNAL FUNCTIONS ----------------------------
input_stats(null) -> null;
input_stats([]) -> [];
input_stats([#{ <<"attack">> := Attack,
                <<"shellScripting">> := SHScript,
                <<"yell">> := Yell } | Next]) ->
    S = #stats {
      attack = Attack,
      shell_scripting = SHScript,
      yell = Yell },
    [S | input_stats(Next)].
