-module(dungeon_stats).
-include("dungeon.hrl").

-export([execute/4]).

execute(Ctx, #stats { attack = Attack,
                       yell = Yell,
                       shell_scripting = ShellScripting},
        Field, _Args) ->
    case Field of
        <<"attack">> ->
            AttackToken = graphql:token(Ctx),
            spawn_link(fun() ->
                               Reply = case Attack of
                                           13 -> {ok, owl};
                                           A -> {ok, A}
                                       end,
                               graphql:reply_cast(AttackToken, Reply)
                       end),
            {defer, AttackToken};
        <<"yell">> ->
            {ok, Yell};
        <<"shellScripting">> ->
            {ok, ShellScripting}
    end.
