-module(graphql_builtins).

-include("graphql_schema.hrl").

-export([standard_types_inject/0, standard_directives_inject/0]).

-spec standard_types_inject() -> ok.
standard_types_inject() ->
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
    	id => 'Boolean',
    	description => <<"Boolean values, either given as the *true* and *false* values of JSON, or as the strings \"true\" and \"false\"."/utf8>> }},
    ID = {scalar, #{
    	id => 'ID',
    	description => <<"Representation of an opaque ID in the system. Always returned/given as strings, but clients are not allowed to deconstruct them. The server might change them as it sees fit later on, and the clients must be able to handle this situation."/utf8>> }},
    ok = graphql:insert_schema_definition(String),
    ok = graphql:insert_schema_definition(Float),
    ok = graphql:insert_schema_definition(Int),
    ok = graphql:insert_schema_definition(Bool),
    ok = graphql:insert_schema_definition(ID),
    ok.

-spec standard_directives_inject() -> ok.
standard_directives_inject() ->
    SkipDirective = {directive, #{
        id => <<"skip">>,
        description => <<"Allows excluding a field depending on argument">>,
        locations => ['FIELD', 'FRAGMENT_SPREAD', 'INLINE_FRAGMENT'],
        resolve_module => graphql_directives,
        args => #{ <<"if">> => #{
            type => 'Boolean',
            default => false,
            description => <<"Wether or not the item should be skipped">> }}
        }},
    IncludeDirective = {directive, #{
        id => <<"include">>,
        description => <<"Allows including a field depending on argument">>,
        locations => ['FIELD', 'FRAGMENT_SPREAD', 'INLINE_FRAGMENT'],
        resolve_module => graphql_directives,
        args => #{ <<"if">> => #{
            type => 'Boolean',
            default => false,
            description => <<"Wether or not the item should be included">> }}
        }},
    DeprecatedDirective = {directive, #{
        id => <<"deprecated">>,
        description => <<"Mark field as deprecated with a helpful message on what to use instead">>,
        locations => ['FIELD_DEFINITION', 'ENUM_VALUE'],
        resolve_module => graphql_directives,
        args => #{ <<"reason">> => #{
            type => 'String',
            default => <<"No longer supported">>,
            description => <<"A message to the developer on why this field is deprecated and what to use instead">> }}
        }},
    ok = graphql:insert_schema_definition(SkipDirective),
    ok = graphql:insert_schema_definition(IncludeDirective),
    ok = graphql:insert_schema_definition(DeprecatedDirective),
    ok.
