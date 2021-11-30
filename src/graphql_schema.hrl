%% Schema definitions
-type mod() :: atom().
-type schema_base_type() :: scalar_type()
                          | enum_type()
                          | binary().

-type schema_type() ::
        {non_null, schema_base_type()}
      | {non_null, {list, schema_base_type()}}
      | {list, schema_base_type()}
      | schema_base_type().

-type resolver_args() :: #{ binary() => term() }.

-type directive_location() :: 'QUERY' | 'MUTATION' | 'SUBSCRIPTION' | 'FIELD'
        | 'FRAGMENT_DEFINITION' | 'FRAGMENT_SPREAD' | 'INLINE_FRAGMENT' | 'SCHEMA'
        | 'SCALAR' | 'OBJECT' | 'FIELD_DEFINITION' | 'ARGUMENT_DEFINITION' | 'INTERFACE'
        | 'UNION' | 'ENUM' | 'ENUM_VALUE' | 'INPUT_OBJECT' | 'INPUT_FIELD_DEFINITION'.

-type resolver() :: fun ((ctx, term(), binary(), resolver_args()) -> term()).

-record(directive_type,
        { id :: binary(),
          description :: undefined | binary(),
          locations :: [directive_location()],
          args = #{} :: #{ binary() => schema_arg() }
        }).
-type directive_type() :: #directive_type{}.

-record(enum_value,
        { val :: binary(),
          description :: binary(),
          directives = [] :: [graphql:directive()],
          deprecation = undefined :: undefined | binary()
        }).
-type enum_value() :: #enum_value{}.

-record(enum_type,
        { id :: binary(),
          description :: binary(),
          resolve_module = graphql_enum_coerce :: mod(),
          directives = [] :: [graphql:directive()],
          values :: #{ integer() => enum_value() }
        }).
-type enum_type() :: #enum_type{}.

-record(interface_type,
        { id :: binary(),
          description :: binary(),
          resolve_type :: mod() | fun ((any()) -> {ok, atom()} | {error, term()}),
          directives = [] :: [graphql:directive()],
          fields :: #{ binary() => schema_field() }
        }).
-type interface_type() :: #interface_type{}.

-record(union_type,
        { id :: binary(),
          description :: binary(),
          resolve_type :: mod() | fun ((any()) -> {ok, atom()} | {error, term()}),
          directives = [] :: [graphql:directive()],
          types :: [binary() | {name, non_neg_integer(), binary()}]
        }).
-type union_type() :: #union_type{}.

-record(schema_arg,
        { ty :: schema_type(),
          default = null :: any(),
          description :: binary(),
          directives = [] :: [graphql:directive()]
        }).
-type schema_arg() :: #schema_arg{}.

-record(schema_field,
        { ty :: schema_type(),
          description :: binary() | undefined,
          resolve = undefined :: undefined | resolver(),
          %% FIXME: deprecations are directives so they can be cast as one
          deprecation = undefined :: undefined | binary(),
          directives = [] :: [graphql:directive()],
          args = #{} :: #{ binary() => schema_arg() }
        }).
-type schema_field() :: #schema_field{}.

-record(scalar_type,
        { id :: binary(),
          description :: binary(),
          directives = [] :: [graphql:directive()],
          resolve_module = graphql_enum_coerce :: mod()
        }).
-type scalar_type() :: #scalar_type{}.

-record(input_object_type,
        { id :: binary(),
          description :: binary(),
          directives = [] :: [graphql:directive()],
          fields = #{} :: #{ binary() => schema_arg() }
        }).
-type input_object_type() :: #input_object_type{}.

-record(object_type,
        { id :: binary(),
          description :: binary(),
          directives = [] :: [graphql:directive()],
          resolve_module :: mod(),
          fields = #{} :: #{ binary() => schema_field() },
          interfaces = [] :: [binary()]
        }).
-type object_type() :: #object_type{}.

-record(root_schema,
        { id = 'ROOT' :: atom(),
          query = undefined :: undefined | binary(),
          mutation = undefined :: undefined | binary(),
          subscription = undefined :: undefined | binary(),
          directives = [] :: [graphql:directive()],
          interfaces = [] :: [binary()]
        }).
-type root_schema() :: #root_schema{}.

-type schema_object() ::
        object_type() | interface_type() | scalar_type()
      | input_object_type() | union_type() | enum_type()
      | directive_type()
      | root_schema().

-record(document,
        { definitions :: [any()]
        }).

-record(endpoint_context, {name :: atom(), pid :: pid(), enums_tab :: atom(), objects_tab :: atom()}).

-type endpoint_context() :: #endpoint_context{}.
