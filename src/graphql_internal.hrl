%% GQL AST Records and types

%% All identifiers in the AST are given like this

-type graphql_base_type() :: graphql:name() | binary().
-type graphql_type() ::
          {non_null, graphql_type()}
        | {list, graphql_type()}
        | graphql_base_type().

-type value() ::
	  graphql:name()
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
	id :: graphql:name(),
	args = [] :: [any()],
	directives = [] :: [any()],
	selection_set = [] :: [any()],
	alias = undefined :: undefined | graphql:name(),
	schema :: any()
}).
-type field() :: #field{}.

-record(frag,
        { id :: '...' | graphql:name(), %% One variant is for inline fragments
          ty :: undefined | graphql_base_type(),
          directives = [] :: [graphql:directive()],
          selection_set = [] :: [#field{}],
          schema = undefined :: 'undefined' | any()
        }).
-type frag() :: #frag{}.

-record(frag_spread,
        { id :: graphql:name(),
          directives = [] :: [graphql:directive()]
        }).
-type frag_spread() :: #frag_spread{}.

-record(vardef,
        { id :: graphql:name(),
          ty :: graphql_type(),
          default = null :: value()
        }).

-record(op,
        { ty :: undefined | operation_type(),
          id = 'ROOT' :: graphql:name() | 'ROOT',
          vardefs = [] :: [#vardef{}] | #{ binary() => #vardef{} },
          directives = [] :: [graphql:directive()],
          selection_set = [] :: [#field{} | #frag_spread{}],
          schema = undefined :: 'undefined' | any()
        }).
-type op() :: #op{}.
-type context() :: #{ atom() => any() }.

%%% --- Parsed Schemas
%% Parsed schemas all starts with a p_ suffix

-record(p_field_def,
        { id :: graphql:name(),
          description = undefined :: 'undefined' | binary(),
          args = [] :: any(),
          type :: graphql_type(),
          directives = [] :: [any()]
        }).
-type p_field_def() :: #p_field_def{}.

-record(p_object,
        { id :: graphql:name(),
          description = undefined :: 'undefined' | binary(),
          fields = [] :: [p_field_def()],
          directives = [] :: [graphql:directive()],
          interfaces = [] :: [graphql:name()]
        }).

-record(p_input_value,
        { id :: graphql:name(),
          description = undefined :: 'undefined' | binary(),
          default = null :: any(),
          directives = [] :: [graphql:directive()],          
          type :: graphql_type()
        }).

-record(p_interface,
        { id :: graphql:name(),
          description = undefined :: 'undefined' | binary(),
          directives = [] :: [graphql:directive()],
          fields = [] :: any()
        }).

-record(p_union,
        { id :: graphql:name(),
          description = undefined :: 'undefined' | binary(),
          directives = [] :: [graphql:directive()],
          members :: [graphql:name()]
        }).

-record(p_scalar,
        { id :: graphql:name(),
          description = undefined :: 'undefined' | binary(),
          directives = [] :: [graphql:directive()]
        }).

-record(p_enum_value,
        { id :: binary(),
          description = undefined :: 'undefined' | binary(),
          directives = [] :: [graphql:directive()]}).

-type p_enum_value() :: #p_enum_value{}.

-record(p_enum,
        { id :: graphql:name(),
          description = undefined :: 'undefined' | binary(),
          directives = [] :: [graphql:directive()],
          variants = [] :: [p_enum_value()]
        }).

-record(p_input_object,
        { id :: graphql:name(),
          description = undefined :: 'undefined' | binary(),
          defs = [] :: any(),
          directives = [] :: [graphql:directive()]
        }).

-record(p_root_operation,
        { op_type :: operation_type(),
          type :: graphql_type()
        }).

-record(p_schema_definition,
        { defs = [] :: [#p_root_operation{}],
          directives = [] :: [graphql:directive()] }).

-record(p_directive,
        { id :: graphql:name(),
          description = undefined :: 'undefined' | binary(),
          args = [] :: [any()],
          locations = [] :: [atom()]}).

-ifdef(OTP_RELEASE). %% this implies 21 or higher
-define(EXCEPTION(Class, Reason, Stacktrace), Class:Reason:Stacktrace).
-define(GET_STACK(Stacktrace), Stacktrace).
-else.
-define(EXCEPTION(Class, Reason, _), Class:Reason).
-define(GET_STACK(_), erlang:get_stacktrace()).
-endif.
