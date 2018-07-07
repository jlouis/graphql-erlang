%% GQL AST Records and types

%% All identifiers in the AST are given like this
-type name() :: {name, pos_integer(), binary()}.
-type graphql_base_type() :: name() | binary().
-type graphql_type() ::
          {non_null, graphql_type()}
        | {list, graphql_type()}
        | graphql_base_type().

-type value() ::
	  name()
        | null
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
	schema :: any()
}).
-type field() :: #field{}.

-record(frag,
        { id :: '...' | name(), %% One variant is for inline fragments
          ty :: undefined | graphql_base_type(),
          directives = [] :: [directive()],
          selection_set = [] :: [#field{}],
          schema = undefined :: 'undefined' | any()
        }).
-type frag() :: #frag{}.

-record(frag_spread,
        { id :: name(),
          directives = [] :: [directive()]
        }).
-type frag_spread() :: #frag_spread{}.

-record(vardef,
        { id :: name(),
          ty :: graphql_type(),
          default = null :: value()
        }).

-record(directive,
        { id :: name(),
          args = [] :: [any()],
          schema :: any()
        }).
-type directive() :: #directive{}.

-record(op,
        { ty :: undefined | operation_type(),
          id = 'ROOT' :: name() | 'ROOT',
          vardefs = [] :: [#vardef{}],
          directives = [] :: [directive()],
          selection_set = [] :: [#field{} | #frag_spread{}],
          schema = undefined :: 'undefined' | any()
        }).
-type op() :: #op{}.

-type ast() :: {document, [#op{}]}.
-type context() :: #{ atom() => any() }.

%%% --- Parsed Schemas
%% Parsed schemas all starts with a p_ suffix

-record(p_field_def,
        { id :: name(),
          description = undefined :: 'undefined' | binary(),
          args = [] :: any(),
          type :: graphql_type(),
          directives = [] :: [any()]
        }).
-type p_field_def() :: #p_field_def{}.

-record(p_object,
        { id :: name(),
          description = undefined :: 'undefined' | binary(),
          fields = [] :: [p_field_def()],
          directives = [] :: [directive()],
          interfaces = [] :: [name()]
        }).

-record(p_input_value,
        { id :: name(),
          description = undefined :: 'undefined' | binary(),
          default = null :: any(),
          directives = [] :: [directive()],          
          type :: graphql_type()
        }).

-record(p_interface,
        { id :: name(),
          description = undefined :: 'undefined' | binary(),
          directives = [] :: [directive()],
          fields = [] :: any()
        }).

-record(p_union,
        { id :: name(),
          description = undefined :: 'undefined' | binary(),
          directives = [] :: [directive()],
          members :: [name()]
        }).

-record(p_scalar,
        { id :: name(),
          description = undefined :: 'undefined' | binary(),
          directives = [] :: [directive()]
        }).

-record(p_enum_value,
        { id :: binary(),
          description = undefined :: 'undefined' | binary(),
          directives = [] :: [directive()]}).

-type p_enum_value() :: #p_enum_value{}.

-record(p_enum,
        { id :: name(),
          description = undefined :: 'undefined' | binary(),
          directives = [] :: [directive()],
          variants = [] :: [p_enum_value()]
        }).

-record(p_input_object,
        { id :: name(),
          description = undefined :: 'undefined' | binary(),
          defs = [] :: any(),
          directives = [] :: [directive()]
        }).

-record(p_root_operation,
        { op_type :: operation_type(),
          name :: name()
        }).

-record(p_schema_definition,
        { defs = [] :: [#p_root_operation{}],
          directives = [] :: [directive()] }).

