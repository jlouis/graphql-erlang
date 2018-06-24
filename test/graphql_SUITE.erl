-module(graphql_SUITE).
-include_lib("common_test/include/ct.hrl").

-compile(export_all).

suite() ->
    [{timetrap, {seconds, 30}}].

init_per_group(introspection, Config) ->
    ok = introspection_schema(),
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

    Basic = {basic, [shuffle, parallel],
             [ hello_world,
               hello_world_query,
               named_queries,
               user_queries ] },

    SchemaTest = {schema_test, [shuffle, parallel],
                  [
                   schema_test,
                   double_iface
                  ]},

    Introspection = {introspection, [shuffle, parallel], [
       is_supports_type
    ]},

    [Basic, Schema, SchemaTest, Introspection].

all() -> [
    {group, schema},
    {group, basic},
    {group, schema_test},
    {group, introspection} ].

%% -- BASIC --------------------------------------
hello_world(Config) ->
    Query = "{ hello }",
    #{ data :=
       #{ <<"hello">> := <<"world">> } } = th:x(Config, Query),
    ok.

hello_world_query(Config) ->
    Query = "query { hello }",
    #{ data :=
           #{ <<"hello">> := <<"world">> } } = th:x(Config, Query),
    ok.

user_queries(Config) ->
    Query = "{ user(id: \"2\") { id name } }",
    #{ data :=
       #{ <<"user">> := #{ <<"id">> := <<"2">>, <<"name">> := <<"Marie">> }}} = th:x(Config, Query),
    ok.

named_queries(Config) ->
    %% Standard query with a name
    Query1 = "query Q($id : String) { user(id: $id) { id name } }",
    %% The same query, but without a name. This seems to be allowed by
    %% many clients in what they produce so make sure we accept it as well
    %%
    %% My Proglang heart weeps, but so be it...
    Query2 = "query ($id : String) { user(id: $id) { id name } }",
    Expected =
        #{ data =>
               #{ <<"user">> => #{ <<"id">> => <<"2">>,
                                   <<"name">> => <<"Marie">> }}},
    Expected = th:x(Config, Query1, <<"Q">>, #{ <<"id">> => <<"2">> }),
    Expected = th:x(Config, Query2, <<>>, #{ <<"id">> => <<"2">> }),
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

double_iface(Config) ->
    FName = filename:join([?config(data_dir, Config), "double_iface.spec"]),
    {ok, Data} = file:read_file(FName),
    try graphql:load_schema(#{ scalars =>
                                    #{ default => scalar_resource },
                                interfaces =>
                                    #{ 'Node' => node_resource}
                             },
                             Data) of
        _Res ->
            ct:fail(schema_load_passed_but_must_fail)
    catch
        exit:{entry_already_exists_in_schema, <<"Node">>} ->
            ok
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
                    <<"keywords">> := [<<"foo">>,<<"bar">>, <<"1">>, <<"true">>, null],
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

%% -- INTERNALS ----------------------------------

introspection_schema() ->
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
