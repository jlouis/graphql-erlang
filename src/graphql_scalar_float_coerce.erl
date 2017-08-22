-module(graphql_scalar_float_coerce).

-export([input/2, output/2]).

input(_, X) ->
    {ok, X}.

output(<<"Float">>, F) when is_float(F) ->
    {ok, F};

output(<<"Float">>,I) when is_integer(I) ->
    {ok, float(I)};

output(_,_) ->
  {ok, null}.
