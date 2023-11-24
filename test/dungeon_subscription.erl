-module(dungeon_subscription).
-include("./dungeon.hrl").

-export([cancel/1]).
-export([subscribe/3, execute/4]).

cancel(Pid) ->
    exit(Pid, shutdown).

subscribe(_Ctx, <<"monsterIntroduced">>, #{<<"mood">> := Mood}) when Mood =/= null ->
    Parent = self(),
    Tag = monster_sub,
    Pid = spawn_link(
            fun() ->
                    dungeon:subscribe(monster),
                    monster_loop(Parent, Tag, fun(#monster{mood = MMood}) -> MMood == Mood end)
            end
           ),
    {subscription, {Tag, Pid}};
subscribe(_Ctx, <<"monsterIntroduced">>, #{}) ->
    Parent = self(),
    Tag = monster_sub,
    Pid = spawn_link(
            fun() ->
                    dungeon:subscribe(monster),
                    monster_loop(Parent, Tag, fun(#monster{}) -> true end)
            end
           ),
    {subscription, {Tag, Pid}};
subscribe(_Ctx, <<"thingIntroduced">>, #{}) ->
    Parent = self(),
    Tag = thing_sub,
    Pid = spawn_link(
            fun() ->
                    dungeon:subscribe(monster),
                    dungeon:subscribe(item),
                    monster_loop(Parent,
                                 Tag,
                                 fun(#monster{}) -> true;
                                    (#item{}) -> true
                                 end)
            end
           ),
    {subscription, {Tag, Pid}};
subscribe(_Ctx, <<"break">>, #{<<"reason">> := Reason}) ->
    {error, binary_to_atom(Reason, utf8)}.

execute(_Ctx, {{monster_sub, _Pid}, #monster{} = Monster}, <<"monsterIntroduced">>, _) ->
    {ok, Monster};
execute(_Ctx, {{thing_sub, _Pid}, Thing}, <<"thingIntroduced">>, _) ->
    {ok, Thing}.

monster_loop(Parent, Tag, Predicate) ->
    receive
        {mnesia_table_event, {write, Record, _}} ->
            case Predicate(Record) of
                true ->
                    notify(Parent, Tag, Record);
                false ->
                    noop
            end;
        Unexpected ->
            ct:pal("Subscription received unexpected message: ~p", [Unexpected])
    end,
    monster_loop(Parent, Tag, Predicate).

notify(Parent, Tag, Record) ->
    Parent ! {subscription_msg, {Tag, self()}, Record}.
