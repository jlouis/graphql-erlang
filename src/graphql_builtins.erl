-module(graphql_builtins).

-export([standard_types_inject/0]).

-spec standard_types_inject() -> ok.
standard_types_inject() ->
    String = {scalar, #{
    	id => 'String',
    	description => "UTF-8 Text Strings" }},
    Float = {scalar, #{
    	id => 'Float',
    	description => "Floating point values, IEEE 754, but not ±infty, nor NaN" }},
    Int = {scalar, #{
    	id => 'Int',
    	description => "Integers in the range ±2^53. The GraphQL standard only allows for 32-bit signed integers, but we can support up to the larger range." }},
    Bool = {scalar, #{
    	id => 'Bool',
    	description => "Boolean values, either given as the *true* and *false* values of JSON, or as the strings \"true\" and \"false\"." }},
    ID = {scalar, #{
    	id => 'ID',
    	description => "Representation of an opaque ID in the system. Always returned/given as strings, but clients are not allowed to deconstruct them. The server might change them as it sees fit later on, and the clients must be able to handle this situation." }},
    ok = graphql:load(String),
    ok = graphql:load(Float),
    ok = graphql:load(Int),
    ok = graphql:load(Bool),
    ok = graphql:load(ID),
    ok.
