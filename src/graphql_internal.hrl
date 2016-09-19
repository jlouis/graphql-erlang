%% GQL AST Records and types

%% All identifiers in the AST are given like this
-type id() :: {name, binary(), pos_integer()}.
-type tycond() :: id() | {scalar, string | int | float | id | bool | id()}.
-type typ() :: {non_null, {list, tycond()}} | {non_null, tycond()} | {list, tycond()} | tycond().
-type value() ::
	  id()
	| {int, integer(), pos_integer()}
	| {float, float(), pos_integer()}
	| {string, binary(), pos_integer()}
	| {bool, true | false, pos_integer()}
	| {enum, binary()}
	| {list, value()}
	| {object, [value()]}.

-type operation_type() :: {query, pos_integer()} | {mutation, pos_integer()} | {subscription, pos_integer()}.

-type selection_set() :: field() | frag() | frag_spread().

-record(field, {
	id :: id(),
	args = [] :: [any()],
	directives = [] :: [any()],
	selection_set = [] :: [any()],
	alias = undefined :: undefined | id(),
	schema = undefined :: 'undefined' | any(),
	schema_obj :: 'scalar' | any()
}).
-type field() :: #field{}.

-record(frag, {
	id :: '...' | id(), %% One variant is for inline fragments
	ty :: undefined | tycond(),
	directives = [] :: [directive()],
	selection_set = [] :: [#field{}],
	schema = undefined :: 'undefined' | any()
}).
-type frag() :: #frag{}.

-record(frag_spread, {
	id :: id(),
	directives = [] :: [directive()]
}).
-type frag_spread() :: #frag_spread{}.

-record(vardef, {
	id :: id(),
	ty :: {ty, typ()},
	default = undefined :: undefined | value(),
	schema = undefined :: 'undefined' | any()
}).

-record(directive, {
	id :: id(),
	args = [] :: [any()]
}).
-type directive() :: #directive{}.

-record(op, {
	ty :: undefined | operation_type(),
	id = 'ROOT' :: id() | 'ROOT',
	vardefs = [] :: [#vardef{}],
	directives = [] :: [#directive{}],
	selection_set = [] :: [#field{}],
	schema = undefined :: 'undefined' | any()
}).
-type op() :: #op{}.

-type ast() :: {document, [#op{}]}.
-type context() :: #{ atom() => any() }.

-type graphql_type() :: string | int | bool | float | {non_null, graphql_type()} | [graphql_type()] | binary().
-type graphql_type_resolution() :: scalar | {list, graphql_type_resolution()} | binary().
