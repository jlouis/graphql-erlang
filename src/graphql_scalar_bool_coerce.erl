-module(graphql_scalar_bool_coerce).

-export([input/2, output/2]).

input(_, X) ->
    {ok, X}.

output(<<"Bool">>, true) ->
    {ok, true};

output(<<"Bool">>, <<"true">>) ->
    {ok, true};

output(<<"Bool">>, false) ->
    {ok, false};

output(<<"Bool">>, <<"false">>) ->
    {ok, false};

output(_,_) ->
    {ok, null}.