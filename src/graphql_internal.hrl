%% GQL AST Records and types

%% All identifiers in the AST are given like this
-type name() :: {name, binary(), pos_integer()}.
-type tycond() :: name() | {scalar, string | int | float | id | bool}.
-type graphql_type() ::
          {non_null, {list, tycond()}}
        | {non_null, tycond()}
        | {list, tycond()} | tycond().

-type value() ::
	  name()
	| {int, integer(), pos_integer()}
	| {float, float(), pos_integer()}
	| {string, binary(), pos_integer()}
	| {bool, true | false, pos_integer()}
	| {enum, binary()}
	| {list, value()}
	| {object, [value()]}.

-type operation_type() ::
          {query, pos_integer()}
        | {mutation, pos_integer()}
        | {subscription, pos_integer()}.

-type selection_set() :: field() | frag() | frag_spread().

-record(field, {
	id :: name(),
	args = [] :: [any()],
	directives = [] :: [any()],
	selection_set = [] :: [any()],
	alias = undefined :: undefined | name(),
	schema = undefined :: 'undefined' | any(),
	schema_obj :: 'scalar' | any()
}).
-type field() :: #field{}.

-record(frag, {
	id :: '...' | name(), %% One variant is for inline fragments
	ty :: undefined | tycond(),
	directives = [] :: [directive()],
	selection_set = [] :: [#field{}],
	schema = undefined :: 'undefined' | any()
}).
-type frag() :: #frag{}.

-record(frag_spread, {
	id :: name(),
	directives = [] :: [directive()]
}).
-type frag_spread() :: #frag_spread{}.

-record(vardef, {
	id :: name(),
	ty :: graphql_type(),
	default = undefined :: undefined | value(),
	schema = undefined :: 'undefined' | any()
}).

-record(directive, {
	id :: name(),
	args = [] :: [any()]
}).
-type directive() :: #directive{}.

-record(op, {
	ty :: undefined | operation_type(),
	id = 'ROOT' :: name() | 'ROOT',
	vardefs = [] :: [#vardef{}],
	directives = [] :: [#directive{}],
	selection_set = [] :: [#field{}],
	schema = undefined :: 'undefined' | any()
}).
-type op() :: #op{}.

-type ast() :: {document, [#op{}]}.
-type context() :: #{ atom() => any() }.
