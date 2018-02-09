-module(graphql_scalar_binary_coerce).

-export([input/2, output/2]).

input(_, X) -> {ok, X}.

output(_,B) when is_binary(B) -> {ok, B};
output(_,_) -> {ok, owl}.
