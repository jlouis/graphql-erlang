-module(dungeon_stats).
-include("dungeon.hrl").

-export([execute/4]).

execute(Ctx, #stats { attack = Attack,
                       yell = Yell,
                       shell_scripting = ShellScripting},
        Field, _Args) ->
    case Field of
        <<"attack">> when Attack == 13 ->
            {ok, null};
        <<"attack">> ->
            {ok, Attack};
        <<"yell">> ->
            {ok, Yell};
        <<"shellScripting">> ->
            {ok, ShellScripting}
    end.

