-module(dungeon_enum).

-export([input/2,
	 output/2]).

bintolower(X) ->
    string:to_lower(
      binary_to_list(X)).

input(<<"Mood">>, <<Bin/bitstring>>) ->
    StrMood = bintolower(Bin),
    {ok, {enum, #{ mood => StrMood }}};
input(<<"Mood">>, X) ->
    {error, {invalid_mood, X}}.

output(<<"Mood">>, <<"DODGY">>) ->
    {ok, <<"DODGY">>};
output(<<"Mood">>, #{ mood := Mood }) ->
    case Mood of 
        "tranquil"  -> {ok, <<"TRANQUIL">>};
 	"dodgy"  -> {ok, <<"DODGY">>};
 	"aggressive"  -> {ok, <<"AGGRESSIVE">>};
 	_ -> {error, {invalid_mood}}
    end.



    


