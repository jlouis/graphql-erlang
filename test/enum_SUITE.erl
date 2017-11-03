-module(enum_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([suite/0, all/0, groups/0, init_per_suite/1, end_per_suite/1,
         init_per_group/2, end_per_group/2,
         init_per_testcase/2, end_per_testcase/2]).

%% Test cases
-export([
         input/1,
         output/1,
         input_output/1,
         no_string_literal/1,
         no_incorrect_internal_value/1,
         no_internal_enum/1,
         no_literal_int/1,
         accept_json/1,
         mutation_accept_json/1,
         no_accept_internal_query/1,
         no_accept_internal_query_2/1,
         no_accept_internal_query_3/1,
         internal_zero/1,
         input_nullable/1
        ]).

suite() ->
    [{timetrap, {seconds, 30}}].

init_per_suite(Config) ->
    application:ensure_all_started(graphql),
    ok = schema_colors:inject(),
    ok = graphql:validate_schema(),
    Config.

init_per_group(_Group, Config) ->
    Config.

end_per_group(_Group, _Config) ->
    ok.

end_per_suite(_Config) ->
    graphql_schema:reset(),
    application:stop(graphql),
    ok.

init_per_testcase(_Case, Config) ->
    Config.

end_per_testcase(_Case, _Config) ->
    ok.

groups() ->
    EnumType = {enum_type, [shuffle, parallel], [
        input,
        output,
        input_output,
        no_string_literal,
        no_incorrect_internal_value,
        no_internal_enum,
        no_literal_int,
        accept_json,
        mutation_accept_json,
        no_accept_internal_query,
        no_accept_internal_query_2,
        no_accept_internal_query_3,
        internal_zero,
        input_nullable
    ]},

    [EnumType].


all() ->
    [{group, enum_type}].

input(Config) ->
    Q1 = "{ colorInt(fromEnum: GREEN) }",
    #{ data := #{ <<"colorInt">> := 1 }} = th:x(Config, Q1),
    ok.

output(Config) ->
    Q1 = "{ colorEnum(fromInt: 1) }",
    #{ data := #{ <<"colorEnum">> := <<"GREEN">> }} = th:x(Config, Q1),
    ok.

input_output(Config) ->
    Q1 = "{ colorEnum(fromEnum: GREEN) }",
    #{ data := #{ <<"colorEnum">> := <<"GREEN">> }} = th:x(Config, Q1),
    ok.

no_string_literal(Config) ->
    Q1 = "{ colorEnum(fromEnum: \"GREEN\") }",
    th:errors(th:x(Config, Q1)),
    ok.

no_incorrect_internal_value(Config) ->
    Q1 = "{ colorEnum(fromString: \"YELLOW\") }",
    #{ data := #{
        <<"colorEnum">> := null }} = th:x(Config, Q1),
    ok.

no_internal_enum(Config) ->
    Q1 = "{ colorEnum(fromEnum: 1) }",
    th:errors(th:x(Config, Q1)),
    ok.

no_literal_int(Config) ->
    Q1 = "{ colorEnum(fromInt: GREEN) }",
    th:errors(th:x(Config, Q1)),
    ok.

accept_json(Config) ->
    Q1 = "query test($color: Color!) { colorEnum(fromEnum: $color) }",
    #{ data :=
       #{ <<"colorEnum">> := <<"BLUE">> }} =
           th:x(Config, Q1, <<"test">>, #{ <<"color">> => <<"BLUE">> }),
    ok.

mutation_accept_json(Config) ->
    Q1 = "mutation x($color: Color!) { favoriteEnum(color: $color) }",
    #{ data :=
       #{ <<"favoriteEnum">> := <<"GREEN">> }} =
           th:x(Config, Q1, <<"x">>, #{ <<"color">> => <<"GREEN">> }),
    ok.

no_accept_internal_query(Config) ->
    Q1 = "query test($color: Color!) { colorEnum(fromEnum: $color) }",
    th:errors(th:x(Config, Q1, <<"test">>, #{ <<"color">> => 2 })),
    ok.

no_accept_internal_query_2(Config) ->
    Q1 = "query test($color: String!) { colorEnum(fromEnum: $color) }",
    th:errors(th:x(Config, Q1, <<"test">>, #{ <<"color">> => <<"BLUE">> })),
    ok.

no_accept_internal_query_3(Config) ->
    Q1 = "query test($color: Int!) { colorEnum(fromEnum: $color) }",
    th:errors(th:x(Config, Q1, <<"test">>, #{ <<"color">> => 2 })),
    ok.

internal_zero(Config) ->
    Q1 = "{ colorEnum(fromEnum: RED) colorInt(fromEnum: RED) }",
    #{ data :=
        #{ <<"colorEnum">> := <<"RED">>,
            <<"colorInt">> := 0 }} = th:x(Config, Q1),
    ok.

input_nullable(Config) ->
    Q1 = "{ colorEnum colorInt }",
    #{ data := #{
        <<"colorEnum">> := null,
        <<"colorInt">> := null
    }} = th:x(Config, Q1),
    ok.

