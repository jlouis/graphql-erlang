-module(graphql_scalar_binary_coerce).

-export([input/2, output/2]).

input(_, X) -> {ok, X}.

output(_,B) when is_binary(B) -> {ok, B};
output(_,X) when is_list(X) ->
    try iolist_to_binary(X) of
        Val -> {ok, Val}
    catch _:_ -> {error, not_coercible}
    end;
output(_, false) -> {ok, <<"false">>};
output(_, true)  -> {ok, <<"true">>};
output(_, I) when is_integer(I) ->
    {ok, integer_to_binary(I)};
output(_, F) when is_float(F) ->
    {ok, float_to_binary(F)};
output(_, _) ->
    {error, not_coercible}.
