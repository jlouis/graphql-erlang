-module(graphql_scalar_binary_coerce).

-export([input/2, output/2]).

input(_, B) when is_binary(B) -> {ok, B};
input(_, _) -> {error, not_string}.

%% According to the specification, Jun2018, we should
%% accept coercion as long as we are not "loosing information"
%% in the process
output(_,B) when is_binary(B) ->
    %% Standard case where the code provided a String value
    {ok, B};
output(_, []) ->
    %% Output an empty list as "[]". This is a choice for the reason
    %% of convenience. We could have chosen something differently,
    %% but in general, users ought to use <<>> for the empty string
    %% in the binarized GraphQL-Erlang world.
    %%
    {ok, <<"[]">>};
output(_, [{ok, _V}|_] = Lst) ->
    %% Assume we have a list of output values we can turn into strings
    %% Then collect them as a string. This happens when you have a
    %% default value which is a list of enum values for instance
    Res = [output(<<"String">>, S) || {ok, S} <- Lst],
    {ok, iolist_to_binary(["[",
                           lists:join(",", [V || {ok, V} <- Res]),
                           "]"])};
output(_,X) when is_list(X) ->
    %% Literal string values in erlang are lists, so treat the data
    %% as iodata() and output it if it doesn't follow the above schema
    try iolist_to_binary(X) of
        Val -> {ok, Val}
    catch _:_ -> {error, not_coercible}
    end;
output(_, A) when is_atom(A) ->
    %% Atoms can be output stringently
    {ok, atom_to_binary(A, utf8)};
output(_, {enum, E}) ->
    %% Default internal enum representation can be output as well
    {ok, E};
output(_, I) when is_integer(I) ->
    %% Integers can be embedded in the string type
    {ok, integer_to_binary(I)};
output(_, F) when is_float(F) ->
    %% Likewise floating point values, although a small loss of precision might occur here
    {ok, float_to_binary(F)};
output(_, _) ->
    {error, not_coercible}.
