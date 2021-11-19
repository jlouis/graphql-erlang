-module(graphql_scalar_integer_coerce).

-export([input/2, output/2]).

-define(MAX_INT, (1 bsl 31)).
-define(MIN_INT, (-(1 bsl 31) - 1)).

input(Ty, X) when is_float(X) ->
    T = trunc(X),
    if
        T == X -> input(Ty, T);
        true ->
            {error, float_truncate_not_integer}
    end;
input(_, X) when is_integer(X), X > ?MAX_INT -> {error, not_int32_value};
input(_, X) when is_integer(X), X < ?MIN_INT -> {error, not_int32_value};
input(_, X) when is_integer(X) -> {ok, X};
input(_, _X)  -> {error, not_integer}.

output(<<"Int">>, I) when is_integer(I) ->
    if
        I > ?MAX_INT ->
            {error, not_int32_value};
        I < ?MIN_INT ->
            {error, not_int32_value};
        true ->
            {ok, I}
    end;
output(<<"Int">>, F) when is_float(F) ->
    Trunc = trunc(F),
    if
        Trunc == F ->
            {ok, Trunc};
        true ->
            {error, not_integer}
    end;
output(_,_) ->
    {ok, null}.
