-module(graphql_enum_coerce).

-export([input/2, output/2]).

input(_, X) ->
    {ok, X}.

output(_, Y) ->
    {ok, Y}.
