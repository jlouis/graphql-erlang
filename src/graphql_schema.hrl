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
-type ctx() :: #{ atom() => term() }.
-type location() :: query | mutation | field
                  | fragment_definition | fragment_spread | inline_fragment.

-type resolver() :: fun ((ctx(), term(), resolver_args()) -> term()).

-record(enum_value,
        { val :: binary(),
          description :: binary(),
          annotations = #{} :: #{ binary() => any() },
          deprecation = undefined :: undefined | binary()
        }).
-type enum_value() :: #enum_value{}.

-record(enum_type,
        { id :: binary(),
          description :: binary(),
          resolve_module = graphql_enum_coerce :: mod(),
          annotations = #{} :: #{ binary() => any() },
          values :: #{ integer() => enum_value() }
        }).
-type enum_type() :: #enum_type{}.

-record(interface_type,
        { id :: binary(),
          description :: binary(),
          resolve_type :: mod() | fun ((any()) -> {ok, atom()} | {error, term()}),
          annotations = #{} :: #{ binary() => any() },
          fields :: #{ binary() => schema_field() }
        }).
-type interface_type() :: #interface_type{}.

-record(union_type,
        { id :: binary(),
          description :: binary(),
          resolve_type :: mod() | fun ((any()) -> {ok, atom()} | {error, term()}),
          annotations = #{} :: #{ binary() => any() },
          types :: [binary()]
        }).
-type union_type() :: #union_type{}.

-record(schema_arg,
        { ty :: schema_type(),
          default = null :: any(),
          description :: binary()
        }).
-type schema_arg() :: #schema_arg{}.

-record(directive_type,
        { id :: binary(),
          locations :: [location()],
          args = #{} :: #{ binary() => schema_arg() }
        }).
-type directive_type() :: #directive_type{}.

-record(schema_field,
        { ty :: schema_type(),
          description :: binary() | undefined,
          resolve = undefined :: undefined | resolver(),
          deprecation = undefined :: undefined | binary(),
          annotations = #{} :: #{ binary() => any() },
          args = #{} :: #{ binary() => schema_arg() }
        }).
-type schema_field() :: #schema_field{}.

-record(scalar_type,
        { id :: binary(),
          description :: binary(),
          annotations = #{} :: #{ binary() => any() },
          resolve_module = graphql_enum_coerce :: mod()
        }).
-type scalar_type() :: #scalar_type{}.

-record(input_object_type,
        { id :: binary(),
          description :: binary(),
          annotations = #{} :: #{ binary() => any() },
          fields = #{} :: #{ binary() => schema_arg() }
        }).
-type input_object_type() :: #input_object_type{}.

-record(object_type,
        { id :: binary(),
          description :: binary(),
          annotations = #{} :: #{ binary() => any() },
          resolve_module :: mod(),
          fields = #{} :: #{ binary() => schema_field() },
          interfaces = [] :: [binary()]
        }).
-type object_type() :: #object_type{}.

-record(root_schema,
        { id = 'ROOT' :: atom(),
          query :: binary(),
          mutation = undefined :: undefined | binary(),
          subscription = undefined :: undefined | binary(),
          interfaces = [] :: [binary()]
        }).
-type root_schema() :: #root_schema{}.

-type schema_object() ::
        object_type() | interface_type() | scalar_type()
      | input_object_type() | union_type() | enum_type()
      | root_schema().
