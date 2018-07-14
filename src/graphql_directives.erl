-module(graphql_directives).

-export([include/0,
         skip/0]).

%% Construct schema types for the directives the system supports
include() ->
    #directive_type {
       id = <<"include">>,
       locations = [field, fragment_spread, inline_fragment],
       args = #{
         <<"if">> =>
             #schema_arg{
                ty = graphql_schema:get(<<"Bool">>),
                default = false,
                description = <<"Wether or not the item should be included">> }
        }};
skip() ->
    #directive_type {
       id = <<"skip">>,
       locations = [field, fragment_spread, inline_fragment],
       args = #{
         <<"if">> =>
             #schema_arg{
                ty = graphql_schema:get(<<"Bool">>),
                default = false,
                description = <<"Wether or not the item should be skipped">> }
        }}.

