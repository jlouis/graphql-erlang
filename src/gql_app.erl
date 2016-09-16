-module(gql_app).
-behaviour(application).
-include("gql_schema.hrl").

-export([start/2, stop/1]).

-spec start(normal | {takeover, node()} | {failover, node()}, term()) -> {ok, pid()} | {error, term()}.
start(_Type, _Args) ->
    gql_sup:start_link().

-spec stop([]) -> ok.
stop(_State) ->
    ok.
