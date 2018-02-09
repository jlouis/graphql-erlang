-module(graphql_enum_coerce).

-export([input/2, output/2]).

input(_, Str) when is_binary(Str) -> {ok, {enum, Str}};
input(_, _) -> {error, not_valid_enum_input}.

output(_, {enum, X}) -> {ok, X};
output(_, Str) when is_binary(Str) -> {ok, Str}.
