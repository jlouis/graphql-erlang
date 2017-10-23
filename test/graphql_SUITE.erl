-module(graphql_SUITE).
-include_lib("common_test/include/ct.hrl").

-compile(export_all).

suite() ->
    [{timetrap, {seconds, 30}}].

init_per_group(introspection, Config) ->
    ok = is_schema(),
    ok = graphql:validate_schema(),
    Config;
init_per_group(schema_test, Config) ->
    ok = blog:inject(),
    ok = graphql:validate_schema(),
    Config;
init_per_group(basic, Config) ->
    ok = basic:inject(),
    ok = graphql:validate_schema(),
    Config;
init_per_group(validation, Config) ->
    ok = pet:inject(),
    ok = graphql:validate_schema(),
    Config;
init_per_group(_Group, Config) ->
    Config.

end_per_group(_Group, _Config) ->
    graphql_schema:reset(),
    ok.

init_per_suite(Config) ->
    application:ensure_all_started(graphql),
    Config.

end_per_suite(_Config) ->
    application:stop(graphql),
    ok.

init_per_testcase(v_5_4_2_3_1, _Config) ->
    {skip, needs_more_validation};
init_per_testcase(x, Config) ->
    {ok, _} = dbg:tracer(),
    dbg:p(all, c),
    dbg:tpl(graphql_execute, lookup_field, '_', cx),
    Config;
init_per_testcase(_Case, Config) ->
    Config.

end_per_testcase(x, _Config) ->
    dbg:stop_clear(),
    ok;
end_per_testcase(_Case, _Config) ->
    ok.

groups() ->
    Schema = {schema, [shuffle, parallel],
              [
               lex_schema,
               parse_schema
              ]},

    Basic = {basic, [shuffle, parallel], [ hello_world, user_queries ] },

    SchemaTest = {schema_test, [shuffle, parallel], [
        schema_test
    ]},

    Introspection = {introspection, [shuffle, parallel], [
       is_supports_type
    ]},

    Validation =
        {validation, [parallel],
         [ v_5_1_1_1
         , v_5_1_2_1
         , v_5_2_1
         , v_5_2_3
         , v_5_3_1
         , v_5_3_2
         , v_5_4_1_1
         , v_5_4_1_2
         , v_5_4_1_3
         , v_5_4_2_1
         , v_5_4_2_2
         , v_5_4_2_3_1
         ]},

    [Basic, Schema, SchemaTest, Validation, Introspection].

all() -> [
    {group, schema},
    {group, validation},
    {group, basic},
    {group, schema_test},
    {group, introspection} ].

%% -- BASIC --------------------------------------
hello_world(Config) ->
    Query = "{ hello }",
    #{ data :=
       #{ <<"hello">> := <<"world">> } } = th:x(Config, Query),
    ok.

user_queries(Config) ->
    Query = "{ user(id: \"2\") { id name } }",
    #{ data :=
       #{ <<"user">> := #{ <<"id">> := <<"2">>, <<"name">> := <<"Marie">> }}} = th:x(Config, Query),
    ok.

%% -- SCHEMA --------------------------------
lex_schema(Config) ->
    FName = filename:join([?config(data_dir, Config), "test_schema.spec"]),
    {ok, Data} = file:read_file(FName),
    case graphql_scanner:string(binary_to_list(Data)) of
        {ok, _Token, _EndLine} ->
            ok;
        {error, Err, _EndLine} ->
            ct:fail({parse_error, Err})
    end.

parse_schema(Config) ->
    FName = filename:join([?config(data_dir, Config), "test_schema.spec"]),
    {ok, Data} = file:read_file(FName),
    case graphql:load_schema(#{ scalars => #{
                                 default => scalar_resource },
                                interfaces => #{
                                 'Node' => node_resource
                                 },
                enums => #{default => enum_resource},
                                unions => #{
                                 'Thing' => node_resource
                                 },
                                objects => #{
                                  'Monster' => monster_resource,
                                  'Item' => item_resource,
                                  'Stats' => stats_resource } }, Data) of
        ok ->
            ok = graphql:validate_schema();
        {error, Reason} ->
            ct:fail(Reason)
    end.

%% -- SCHEMA TEST --------------------------------
schema_test(Config) ->
    Q =
      "{ feed { id, title }, "
      "  article(id: 1) { ...articleFields, author { id, name, pic(width: 640, height: 480) {"
      "     url, width, height }, recentArticle { ...articleFields, keywords }}}} "
      "fragment articleFields on Article { id, isPublished, title, body }",
    #{data := #{
       <<"article">> := #{
           <<"author">> := #{
               <<"id">> := <<"123">>,
               <<"name">> := <<"John Smith">>,
               <<"pic">> := #{ <<"height">> := 480, <<"url">> := <<"cdn://123">>, <<"width">> := 640},
                <<"recentArticle">> := #{
                    <<"body">> := <<"This is a post">>,
                    <<"id">> := <<"1">>,
                    <<"isPublished">> := true,
                    <<"keywords">> := [<<"foo">>,<<"bar">>, null, null, null],
                    <<"title">> := <<"My article number 1">>
                }
            },
            <<"body">> := <<"This is a post">>,
            <<"id">> := <<"1">>,
            <<"isPublished">> := true,
            <<"title">> := <<"My article number 1">>
        },
        <<"feed">> := [
             #{<<"id">> := <<"1">>,<<"title">> := <<"My article number 1">>},
             #{<<"id">> := <<"2">>,<<"title">> := <<"My article number 2">>},
             #{<<"id">> := <<"3">>,<<"title">> := <<"My article number 3">>},
             #{<<"id">> := <<"4">>,<<"title">> := <<"My article number 4">>},
             #{<<"id">> := <<"5">>,<<"title">> := <<"My article number 5">>},
             #{<<"id">> := <<"6">>,<<"title">> := <<"My article number 6">>},
             #{<<"id">> := <<"7">>,<<"title">> := <<"My article number 7">>},
             #{<<"id">> := <<"8">>,<<"title">> := <<"My article number 8">>},
             #{<<"id">> := <<"9">>,<<"title">> := <<"My article number 9">>},
             #{<<"id">> := <<"10">>,<<"title">> := <<"My article number 10">>}]}}
                  = th:x(Config, Q),
    ok.

is_supports_type(Config) ->
    Query = "{ __type(name: \"TestType\") { name } }",
    #{ data := #{
        <<"__type">> := #{
            <<"name">> := <<"TestType">> } } } = th:x(Config, Query),
    ok.

v_5_1_1_1(_Config) ->
    Q1 =
      "query getDogName { dog { name } } "
      "query getOwnerName { dog { owner { name } } }",
    true = th:v(Q1),

    Q2 =
      "query getName { dog { name } } "
      "query getName { dog { owner { name } } }",
    false = th:v(Q2),

    Q3 =
      "query dogOperation { dog { name } } "
      "mutation dogOperation { mutateDog { id } } ",
    false = th:v(Q3),
    ok.

v_5_1_2_1(_Config) ->
    Q1 = "{ dog { name } }",
    true = th:v(Q1),
    Q2 = "{ dot { name } } query getName { dog { owner { name } } }",
    false = th:v(Q2),
    ok.

v_5_2_1(_Config) ->
    Q1 =
      "{ dog { ...fieldNotDefined } } "
      "fragment fieldNotDefined on Dog { meowVolume }",
    false = th:v(Q1),

    Q2 =
      "{ dog { ...lyingFragment } } "
      "fragment lyingFragment on Dog { barkVolume: kawVolume }",
    false = th:v(Q2),

    Q3 =
      "{ dog { ...PetFragment } } "
      "fragment PetFragment on Pet { name }",
    true = th:v(Q3),

    Q4 =
      "{ dog { ...PetFragment } } "
      "fragment PetFragment on Pet { nickname }",
    false = th:v(Q4),

    Q5 =
      "{ dog { ...Fragment } } "
      "fragment Fragment on CatOrDog { __typename ... on Pet { name } ... on Dog { barkVolume } }",
    true = th:v(Q5),

    Q6 =
      "{ dog { ...Fragment } } "
      "fragment Fragment on CatOrDog { name barkVolume }",
    false = th:v(Q6),
    ok.

v_5_2_3(_Config) ->
    Q1 =
       "{ dog { ...scalarSelection } } fragment scalarSelection on Dog { barkVolume }",
    true = th:v(Q1),

    Q2 =
       "{ dog { ...Fragment }} fragment Fragment on Dog { barkVolume { sinceWhen }}",
    false = th:v(Q2),

    false = th:v("query Q { human } }"),
    false = th:v("query Q { pet } }"),
    false = th:v("query Q { catOrDog } }"),
    ok.

v_5_3_1(_Config) ->
    true = th:v(
      "{ dog { ...F } } fragment F on Dog { doesKnowCommand(dogCommand: SIT) }"),

    false = th:v(
      "{ dog { ...F } } fragment F on Dog { doesKnowCommand(command: CLEAN_UP_HOUSE) }"),

    true = th:v(
      "{ dog { ...F } } fragment F on Arguments { multipleReqs(x: 1, y: 2) }"),

    true = th:v(
      "{ dog { ...F } } fragment F on Arguments { multipleReqs(y:1, x: 2) }"),

    ok.

v_5_3_2(_Config) ->
    true = th:v(
      "{ dog { ...F } } fragment F on Dog { doesKnowCommand(dogCommand: SIT) }"),

    false = th:v(
      "{ dog { ...F } } fragment F on Dog { doesKnowCommand(dogCommand: SIT, dogCommand: HEEL) }"),
    ok.

v_5_4_1_1(_Config) ->
    Q1 =
        "{ dog { ...fragOne ...fragTwo } } "
        "fragment fragOne on Dog { name } "
        "fragment fragTwo on Dog { owner { name } }",
    true = th:v(Q1),

    Q2 =
        "{ dog { ...fragOne } } "
        "fragment fragOne on Dog { name } "
        "fragment fragOne on Dog { owner { name } }",
    false = th:v(Q2),
    ok.

v_5_4_1_2(_Config) ->
    Q1 =
        "{ dog { ...correctType } } fragment correctType on Dog { name }",
    true = th:v(Q1),

    Q2 =
        "{ dog { ...inlineFragment } } fragment inlineFragment on Dog { ... on Dog { name } }",
    true = th:v(Q2),

    Q3 =
        "{ dog { ...inlineFragment2 } } fragment inlineFragment2 on Dog { ... @include(if: true) { name } }",
    true = th:v(Q3),

    Q4 =
        "{ dog { ...F } } fragment F on NotInSchema { name }",
    false = th:v(Q4),

    Q5 =
        "{ dog { ...F } } fragment F on Dog { ... on NotInSchema { name } }",
    false = th:v(Q5),
    ok.

v_5_4_1_3(_Config) ->
    true = th:v(
      "{ dog { ...F } } fragment F on Dog { name }"),
    true = th:v(
      "{ dog { ...F } } fragment F on Pet { name }"),
    true = th:v(
      "{ dog { ...F } } fragment F on CatOrDog { ... on Dog { name } }"),

    false = th:v(
      "{ dog { ...F } } fragment F on Int { something }"),
    false = th:v(
      "{ dog { ...F } } fragment F on Dog { ... on Boolean { somethingElse } }"),
    false = th:v(
      "{ dog { ...F } } fragment F on DogCommand { argh }"),
    ok.

v_5_4_2_1(_Config) ->
    false = th:v("{ dog { ...undefinedFragment } }"),
    ok.

v_5_4_2_2(_Config) ->
    Q1 =
      "{ dog { ...nameFragment } } "
      "fragment nameFragment on Dog { name ...barkVolumeFragment } "
      "fragment barkVolumeFragment on Dog { barkVolume ...nameFragment }",
    false = th:v(Q1),

    Q2 =
      "{ dog { ...dogFragment } } "
      "fragment dogFragment on Dog { name owner { ...ownerFragment } } "
      "fragment ownerFragment on Human { name pets { ...dogFragment } }",
    false = th:v(Q2),
    ok.

v_5_4_2_3_1(_Config) ->
   true = th:v(
     "{ dog { ...F } } fragment F on Dog { ... on Dog { barkVolume } }"),

   false = th:v(
     "{ dog { ...F } } fragment F on Dog { ... on Cat { meowVolume } }"),

   ok.

%% -- INTERNALS ----------------------------------

is_schema() ->
    Test = {object, #{
        id => 'TestType',
        description => "A simple test object.",
        fields => #{
            testField => #{ type => 'String', description => "A test field" }
        }
    }},
    Schema = {root, #{
        query => 'TestType'
    }},
    ok = graphql:insert_schema_definition(Test),
    ok = graphql:insert_schema_definition(Schema),
    ok.
