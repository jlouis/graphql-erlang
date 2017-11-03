-module(dungeon_enum).

-export([input/2,
	 output/2]).

input(<<"Mood">>, <<Bin/bitstring>>) ->
    {ok, binary_to_existing_atom(Bin, utf8)};
input(<<"Mood">>, X) ->
    {error, {invalid_mood, X}}.

output(<<"Mood">>, 'DODGY') -> {ok, <<"DODGY">>};
output(<<"Mood">>, 'TRANQUIL') -> {ok, <<"TRANQUIL">>};
output(<<"Mood">>, 'AGGRESSIVE') -> {ok, <<"AGGRESSIVE">>};
%% This is a deliberate error case
output(<<"Mood">>, <<"INVALIDMOOD">>) -> {ok, <<"INVALIDMOOD">>}.

