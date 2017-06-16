-module(dungeon_scalar).

-export([input/2,
         output/2]).

input(<<"ColorType">>, Bin) ->
    {ok, list_to_binary(
           string:to_lower(
             binary_to_list(Bin)))};
input(<<"Color">>, <<"#", R1, R2, G1, G2, B1, B2>>) ->
    Red = binary_to_integer(<<R1, R2>>, 16),
    Green = binary_to_integer(<<G1, G2>>, 16),
    Blue = binary_to_integer(<<B1, B2>>, 16),
    {ok, #{ r => Red, g => Green, b => Blue}};
input(<<"Color">>, <<"#">>) ->
    exit(argh);
input(<<"Color">>, X) ->
    {error, {invalid_color, X}}.

output(<<"ColorType">>, X) ->
    {ok, X};
output(<<"Color">>, #{ r := R, g := G, b := B}) ->
    R1 = integer_to_binary(R, 16),
    G1 = integer_to_binary(G, 16),
    B1 = integer_to_binary(B, 16),
    {ok, <<"#", R1/binary, G1/binary, B1/binary>>}.

