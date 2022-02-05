-module(graphql_scalar_bool_coerce).

-export([input/2, output/2]).

input(_, true) -> {ok, true};
input(_, false) -> {ok, false};
input(_, _) -> {error, not_bool}.

output(<<"Boolean">>, true)                 -> {ok, true};
output(<<"Boolean">>, <<"true">>)           -> {ok, true};
output(<<"Boolean">>, false)                -> {ok, false};
output(<<"Boolean">>, <<"false">>)          -> {ok, false};
output(<<"Boolean">>, 0)                    -> {ok, false};
output(<<"Boolean">>, X) when is_integer(X) -> {ok, true};
output(_,_)                              -> {error, not_coercible}.
