-module(graphql_builtins).

-include("graphql_schema.hrl").

-export([directive_schema/2]).
-export([standard_types_inject/1]).

-spec standard_types_inject(graphql_schema:endpoint_context()) -> ok.
standard_types_inject(EP) ->
    String = {scalar, #{
    	id => 'String',
    	description => <<"UTF-8 Text Strings"/utf8>> }},
    Float = {scalar, #{
    	id => 'Float',
    	description => <<"Floating point values, IEEE 754, but not infinity, nor NaN"/utf8>> }},
    Int = {scalar, #{
    	id => 'Int',
    	description => <<"Integers in the range ±2^53. The GraphQL standard only allows for 32-bit signed integers, but we can support up to the larger range."/utf8>> }},
    Bool = {scalar, #{
    	id => 'Bool',
    	description => <<"Boolean values, either given as the *true* and *false* values of JSON, or as the strings \"true\" and \"false\"."/utf8>> }},
    ID = {scalar, #{
    	id => 'ID',
    	description => <<"Representation of an opaque ID in the system. Always returned/given as strings, but clients are not allowed to deconstruct them. The server might change them as it sees fit later on, and the clients must be able to handle this situation."/utf8>> }},
    ok = graphql:ep_insert_schema_definition(EP, String),
    ok = graphql:ep_insert_schema_definition(EP, Float),
    ok = graphql:ep_insert_schema_definition(EP, Int),
    ok = graphql:ep_insert_schema_definition(EP, Bool),
    ok = graphql:ep_insert_schema_definition(EP, ID),
    ok.

%% Construct schema types for the directives the system supports
directive_schema(EP, include) ->
    #directive_type {
       id = <<"include">>,
       locations = [field, fragment_spread, inline_fragment],
       args = #{
         <<"if">> =>
             #schema_arg{
                ty = graphql_schema:get(EP, <<"Bool">>),
                default = false,
                description = <<"Wether or not the item should be included">> }
        }};
directive_schema(EP, skip) ->
    #directive_type {
       id = <<"skip">>,
       locations = [field, fragment_spread, inline_fragment],
       args = #{
         <<"if">> =>
             #schema_arg{
                ty = graphql_schema:get(EP, <<"Bool">>),
                default = false,
                description = <<"Wether or not the item should be skipped">> }
        }}.

