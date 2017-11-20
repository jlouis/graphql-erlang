-module(dungeon_dice).
-include("dungeon.hrl").

-export([execute/4]).

execute(Ctx, #dice { delay = TimeOut }, Field, _) ->
    case Field of
        <<"rollDefer">> ->
            Token = graphql:token(Ctx),
            spawn_link(fun() ->
                           graphql:reply_cast(Token, {ok, TimeOut})
                       end),
            {defer, Token, #{timeout => TimeOut}};

        <<"rollDeferX2">> ->
            roll_defer(Ctx, TimeOut, 2);

        <<"rollDeferX3">> ->
            roll_defer(Ctx, TimeOut, 3);

        <<"rollDeferX5">> ->
            roll_defer(Ctx, TimeOut, 5)
    end.

roll_defer(Ctx, TimeOut, Multiplier) ->
    Token = graphql:token(Ctx),
    TimeOutMultiplied = TimeOut * Multiplier,
    spawn_link(fun() ->
                   timer:sleep(TimeOutMultiplied),
                   timer:sleep(1000),
                   graphql:reply_cast(Token, {ok, TimeOutMultiplied })
               end),
    {defer, Token, #{timeout => TimeOutMultiplied}}.
