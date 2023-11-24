-module(star_wars_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([suite/0, all/0, groups/0, init_per_suite/1, end_per_suite/1,
         init_per_group/2, end_per_group/2,
         init_per_testcase/2, end_per_testcase/2]).

-export([hero/1, friends/1, query_id_params/1, query_aliases/1,
         fragments/1, typename/1, dump_dot/1]).

-export([complex/1, non_existent_field/1, fields_on_objects/1,
         no_fields_on_interfaces/1, no_fields_on_scalars/1,
         object_field_in_fragments/1, object_field_in_inline_fragments/1]).

suite() ->
    [{timetrap, {seconds, 30}}].

init_per_group(_Group, Config) ->
    Config.

end_per_group(_Group, _Config) ->
    ok.

init_per_suite(Config) ->
    application:ensure_all_started(graphql),
    ok = schema_star_wars:inject(),
    ok = graphql:validate_schema(),
    Config.

end_per_suite(_Config) ->
    graphql_schema:reset(),
    application:stop(graphql),
    ok.

init_per_testcase(_Case, Config) ->
    Config.

end_per_testcase(_Case, _Config) ->
    ok.

groups() ->
    Basic = {basic, [shuffle, parallel],
             [
              hero,
              friends,
              query_id_params,
              query_aliases,
              fragments,
              typename,
              dump_dot
             ]},

    Validation = {validation, [shuffle, parallel],
                  [
                   complex,
                   non_existent_field,
                   fields_on_objects,
                   no_fields_on_scalars,
                   no_fields_on_interfaces,
                   object_field_in_inline_fragments,
                   object_field_in_fragments
                  ]},
    [Basic, Validation].

all() -> [
          {group, basic},
          {group, validation}
         ].

%% -- STAR WARS™ --------------------------------

hero(Config) ->
    Q1 =
        "query HeroNameQuery {"
        "  hero {"
        "     name"
        "  }"
        "}",
    #{ data :=
        #{ <<"hero">> := #{ <<"name">> := <<"R2-D2">> } } } = th:x(Config, Q1),

    Q2 =
       "query HeroNameQuery {"
       "  hero(episode: EMPIRE) {"
       "    name"
       "  }"
       "}",
    #{ data :=
        #{ <<"hero">> := #{ <<"name">> := <<"Luke Skywalker">> } } } = th:x(Config, Q2),
    ok.

friends(Config) ->
    Q1 =
      "query HeroNameAndFriendsQuery {"
      "    hero {"
      "      id"
      "      name"
      "      friends {"
      "        name"
      " }}}",
    #{ data :=
        #{ <<"hero">> := #{
            <<"id">> := <<"2001">>,
            <<"name">> := <<"R2-D2">>,
            <<"friends">> := [
                #{ <<"name">> := <<"Luke Skywalker">> },
                #{ <<"name">> := <<"Han Solo">> },
                #{ <<"name">> := <<"Leia Organa">> } ] } } } = th:x(Config, Q1),

    Q2 =
      "query NestedQuery {"
      "  hero {"
      "      name"
      "      friends {"
      "        name"
      "        appearsIn"
      "        friends {"
      "          name"
      "        }"
      "      }"
      "  }"
      "}",
    #{ data :=
        #{ <<"hero">> := #{
            <<"name">> := <<"R2-D2">>,
            <<"friends">> := [
                #{ <<"name">> := <<"Luke Skywalker">>,
                     <<"appearsIn">> := [<<"NEWHOPE">>, <<"EMPIRE">>, <<"JEDI">> ],
                     <<"friends">> := [
                         #{ <<"name">> := <<"Han Solo">> },
                         #{ <<"name">> := <<"Leia Organa">> },
                         #{ <<"name">> := <<"C-3PO">> },
                         #{ <<"name">> := <<"R2-D2">> } ] },
                #{ <<"name">> := <<"Han Solo">>,
                     <<"appearsIn">> := [<<"NEWHOPE">>, <<"EMPIRE">>, <<"JEDI">> ],
                     <<"friends">> := [
                         #{ <<"name">> := <<"Luke Skywalker">> },
                         #{ <<"name">> := <<"Leia Organa">> },
                         #{ <<"name">> := <<"R2-D2">> } ] },
                #{ <<"name">> := <<"Leia Organa">>,
                     <<"appearsIn">> := [<<"NEWHOPE">>, <<"EMPIRE">>, <<"JEDI">> ],
                     <<"friends">> := [
                         #{ <<"name">> := <<"Luke Skywalker">> },
                         #{ <<"name">> := <<"Han Solo">> },
                         #{ <<"name">> := <<"C-3PO">> },
                         #{ <<"name">> := <<"R2-D2">> } ] }
             ] } } } = th:x(Config, Q2),
    ok.

query_id_params(Config) ->
    Q1 =
      "query FetchLukeQuery {"
      "    human(id: \"1000\") {"
      "      name"
      " }}",
    #{ data :=
        #{ <<"human">> := #{ <<"name">> := <<"Luke Skywalker">> } } } = th:x(Config, Q1),
    Q2 =
      "query FetchSomeIDQuery($someID: string!) {"
      "    human(id: $someID) {"
      "        name"
      "    }"
      "}",
    #{ data :=
        #{ <<"human">> :=
            #{ <<"name">> := <<"Luke Skywalker">> } } } =
                th:x(Config, Q2, <<"FetchSomeIDQuery">>, #{ <<"someID">> => <<"1000">> }),
    #{ data :=
        #{ <<"human">> :=
            #{ <<"name">> := <<"Han Solo">> } } } =
                th:x(Config, Q2, <<"FetchSomeIDQuery">>, #{ <<"someID">> => <<"1002">> }),
    #{ data :=
        #{ <<"human">> := null } } =
            th:x(Config, Q2, <<"FetchSomeIDQuery">>, #{ <<"someID">> => <<"Not a valid query">> }),
    ok.

query_aliases(Config) ->
    Q1 =
      "query FetchLukeAliased {"
      "     luke: human(id: \"1000\") {"
      "          name"
      "     }"
      "}",
    #{ data :=
        #{ <<"luke">> :=
            #{ <<"name">> := <<"Luke Skywalker">> } } } = th:x(Config, Q1),
    Q2 =
      "query FetchLukeAndLeiaAliased {"
      "     luke: human(id: \"1000\") {"
      "          name"
      "     }"
      "     leia: human(id: \"1003\") {"
      "          name"
      "     }"
      "}",
    #{ data := #{
          <<"luke">> := #{ <<"name">> := <<"Luke Skywalker">> },
          <<"leia">> := #{ <<"name">> := <<"Leia Organa">> } } } = th:x(Config, Q2),
    ok.

fragments(Config) ->
    Q1 =
      "query DuplicateFields {"
      "   luke: human(id: \"1000\") { name homePlanet }"
      "   leia: human(id: \"1003\") { name homePlanet } }",
    Expected = #{ data => #{
          <<"luke">> => #{
            <<"name">> => <<"Luke Skywalker">>,
            <<"homePlanet">> => <<"Tatooine">> },
          <<"leia">> => #{
            <<"name">> => <<"Leia Organa">>,
            <<"homePlanet">> => <<"Alderaan">> } } },
    Expected = th:x(Config, Q1),
    Q2 =
      "query UseFragment {"
      "    luke: human(id: \"1000\") { ...HumanFragment }"
      "    leia: human(id: \"1003\") { ...HumanFragment }"
      "}"
      "fragment HumanFragment on Human {"
      "    name"
      "    homePlanet"
      "}",
    Expected = th:x(Config, Q2),

    ok.

typename(Config) ->
    Q1 =
      "query CheckTypeOfR2 { hero { __typename name } }",
    Expected = #{ data => #{
        <<"hero">> => #{
            <<"name">> => <<"R2-D2">>,
            <<"__typename">> => <<"Droid">>
        }
    }},
    Expected = th:x(Config, Q1),
    ok.

dump_dot(_Config) ->
    ok = graphql_dot:dump("./star_wars_schema.dot").

%% -- STAR WARS™ VALIDATION ------------------------------

complex(Config) ->
    Q1 =
      "query NestedQueryWithFragment {"
      "  hero {"
      "    ...NameAndAppearances"
      "    friends {"
      "       ...NameAndAppearances"
      "       friends {"
      "           ...NameAndAppearances"
      " }}}} "
      ""
      "fragment NameAndAppearances on Character {"
      "  name"
      "  appearsIn"
      "}",
    th:no_errors(th:x(Config, Q1)),
    ok.

non_existent_field(Config) ->
    Q1 =
      "query HeroSpaceshipQuery {"
      "  hero {"
      "    favoriteSpaceship"
      "  }"
      "}",
    th:errors(th:x(Config, Q1)),
    ok.

fields_on_objects(Config) ->
    Q1 =
      "query HeroSpaceshipQuery {"
      "  hero"
      "}",
    th:errors(th:x(Config, Q1)),
    ok.

no_fields_on_scalars(Config) ->
    Q1 =
      "query HeroSpaceshipQuery {"
      "  hero { name { firstCharacterOfName } }"
      "}",
    th:errors(th:x(Config, Q1)),
    ok.

no_fields_on_interfaces(Config) ->
    Q1 =
      "query DroidFieldOnCharacter {"
      "  hero { name primaryFunction }"
      "}",
    th:errors(th:x(Config, Q1)),
    ok.

object_field_in_fragments(Config) ->
    Q1 =
      "query DroidFieldInFragment { hero { name ...DroidFields } }"
      "fragment DroidFields on Droid { primaryFunction }",
    th:no_errors(th:x(Config, Q1)),
    ok.

object_field_in_inline_fragments(Config) ->
    Q1 =
      "query DroidFieldInFragment { hero { name ... on Droid { primaryFunction } } }",
    th:no_errors(th:x(Config, Q1)),
    ok.
