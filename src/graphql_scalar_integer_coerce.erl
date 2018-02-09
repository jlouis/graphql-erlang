-module(graphql_scalar_integer_coerce).

-export([input/2, output/2]).

input(_, X) ->
    {ok, X}.

output(<<"Int">>, I) when is_integer(I) -> {ok, I};
output(_,_)                             -> {ok, owl}.
