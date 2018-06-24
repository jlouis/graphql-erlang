-module(graphql_scalar_float_coerce).

-export([input/2, output/2]).

input(_, X) when is_float(X)   -> {ok, X};
input(_, X) when is_integer(X) -> {ok, float(X)};
input(_, _)                    -> {error, not_float_coercible}.

output(<<"Float">>, F) when is_float(F)   -> {ok, F};
output(<<"Float">>, I) when is_integer(I) -> {ok, float(I)};
output(_,_)                               -> {error, not_float_coercible}.
