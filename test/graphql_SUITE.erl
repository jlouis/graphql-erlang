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
init_per_group(enum_type, Config) ->
    ok = colors:inject(),
    ok = graphql:validate_schema(),
    Config;
init_per_group(star_wars, Config) ->
    ok = star_wars:inject(),
    ok = graphql:validate_schema(),
    Config;
init_per_group(basic, Config) ->
    ok = basic:inject(),
    ok = graphql:validate_schema(),
    Config;
init_per_group(dungeon, Config) ->
    ok = dungeon:inject(),
    ok = dungeon:start(),
    ok = graphql:validate_schema(),
    Config;
init_per_group(validation, Config) ->
    ok = pet:inject(),
    ok = graphql:validate_schema(),
    Config;
init_per_group(_Group, Config) ->
    Config.

end_per_group(dungeon, _Config) ->
    dungeon:stop(),
    graphql_schema:reset(),
    ok;
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
    {skip, needs_more_type_checking};
init_per_testcase(enum_no_incorrect_internal_value, _Config) ->
    {skip, no_output_validation_yet};
init_per_testcase(_Case, Config) ->
    Config.

end_per_testcase(_Case, _Config) ->
    ok.

groups() ->
    Basic = {basic, [shuffle, parallel], [ hello_world, user_queries ] },
    SW = {star_wars, [shuffle, parallel], [
        %% Basic tests
        star_wars_hero,
        star_wars_friends,
        star_wars_query_id_params,
        star_wars_query_aliases,
        star_wars_fragments,
        star_wars_typename,
        
        %% Validation tests
        star_wars_complex,
        star_wars_non_existent_field,
        star_wars_fields_on_objects,
        star_wars_no_fields_on_scalars,
        star_wars_no_fields_on_interfaces,
        star_wars_object_field_in_inline_fragments,
        star_wars_object_field_in_fragments
    ]},
    
    EnumType = {enum_type, [shuffle, parallel], [
        enum_input,
        enum_output,
        enum_input_output,
        enum_no_string_literal,
        enum_no_incorrect_internal_value,
        enum_no_internal_enum,
        enum_no_literal_int,
        enum_accept_json,
        enum_mutation_accept_json,
        enum_no_accept_internal_query,
        enum_no_accept_internal_query_2,
        enum_no_accept_internal_query_3,
        enum_internal_zero,
        enum_input_nullable
    ]},

    SchemaTest = {schema_test, [shuffle, parallel], [
        schema_test
    ]},

    Introspection = {introspection, [shuffle, parallel], [
       is_supports_type
    ]},

    Dungeon = {dungeon, [],
               [ unions,
                 union_errors,
                 populate,
                 direct_input,
                 inline_fragment,
                 fragment_over_union_interface,
                 simple_field_merge ]},

    Validation = {validation, [parallel], [
    	v_5_1_1_1,
    	v_5_1_2_1,
    	v_5_2_1,
    	v_5_2_3,
    	v_5_3_1,
    	v_5_3_2,
    	v_5_4_1_1,
    	v_5_4_1_2,
    	v_5_4_1_3,
	v_5_4_2_1,
	v_5_4_2_2,
	v_5_4_2_3_1
    ]},

    [Dungeon, Basic, SW, EnumType, SchemaTest, Validation, Introspection].

all() -> [
    {group, validation},
    {group, basic},
    {group, star_wars},
    {group, enum_type},
    {group, schema_test},
    {group, introspection},
    {group, dungeon} ].

%% -- BASIC --------------------------------------
hello_world(Config) ->
    Query = "{ hello }",
    #{ data :=
       #{ <<"hello">> := <<"world">> } } = x(Config, Query),
    ok.

user_queries(Config) ->
    Query = "{ user(id: \"2\") { id name } }",
    #{ data :=
       #{ <<"user">> := #{ <<"id">> := <<"2">>, <<"name">> := <<"Marie">> }}} = x(Config, Query),
    ok.

%% -- STAR WARS™ --------------------------------

star_wars_hero(Config) ->
    Q1 =
        "query HeroNameQuery {"
        "  hero {"
        "     name"
        "  }"
        "}",
    #{ data :=
        #{ <<"hero">> := #{ <<"name">> := <<"R2-D2">> } } } = x(Config, Q1),
    
    Q2 =
       "query HeroNameQuery {"
       "  hero(episode: EMPIRE) {"
       "    name"
       "  }"
       "}",
    #{ data :=
        #{ <<"hero">> := #{ <<"name">> := <<"Luke Skywalker">> } } } = x(Config, Q2),
    ok.
    
star_wars_friends(Config) ->
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
                #{ <<"name">> := <<"Leia Organa">> } ] } } } = x(Config, Q1),
                
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
             ] } } } = x(Config, Q2),
    ok.

star_wars_query_id_params(Config) ->
    Q1 =
      "query FetchLukeQuery {"
      "    human(id: \"1000\") {"
      "      name"
      " }}",
    #{ data :=
        #{ <<"human">> := #{ <<"name">> := <<"Luke Skywalker">> } } } = x(Config, Q1),
    Q2 =
      "query FetchSomeIDQuery($someID: string!) {"
      "    human(id: $someID) {"
      "        name"
      "    }"
      "}",
    #{ data :=
        #{ <<"human">> :=
            #{ <<"name">> := <<"Luke Skywalker">> } } } =
                x(Config, Q2, <<"FetchSomeIDQuery">>, #{ <<"someID">> => <<"1000">> }),
    #{ data :=
        #{ <<"human">> :=
            #{ <<"name">> := <<"Han Solo">> } } } =
                x(Config, Q2, <<"FetchSomeIDQuery">>, #{ <<"someID">> => <<"1002">> }),
    #{ data :=
        #{ <<"human">> := null } } =
            x(Config, Q2, <<"FetchSomeIDQuery">>, #{ <<"someID">> => <<"Not a valid query">> }),
    ok.

star_wars_query_aliases(Config) ->
    Q1 =
      "query FetchLukeAliased {"
      "     luke: human(id: \"1000\") {"
      "          name"
      "     }"
      "}",
    #{ data :=
        #{ <<"luke">> :=
            #{ <<"name">> := <<"Luke Skywalker">> } } } = x(Config, Q1),
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
          <<"leia">> := #{ <<"name">> := <<"Leia Organa">> } } } = x(Config, Q2),
    ok.

star_wars_fragments(Config) ->
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
    Expected = x(Config, Q1),
    Q2 =
      "query UseFragment {"
      "    luke: human(id: \"1000\") { ...HumanFragment }"
      "    leia: human(id: \"1003\") { ...HumanFragment }"
      "}"
      "fragment HumanFragment on Human {"
      "    name"
      "    homePlanet"
      "}",
    Expected = x(Config, Q2),
    
    ok.

star_wars_typename(Config) ->
    Q1 =
      "query CheckTypeOfR2 { hero { __typename name } }",
    Expected = #{ data => #{
        <<"hero">> => #{
            <<"name">> => <<"R2-D2">>,
            <<"__typename">> => <<"Droid">>
        }
    }},
    Expected = x(Config, Q1),
    ok.
    
%% -- STAR WARS™ VALIDATION ------------------------------

star_wars_complex(Config) ->
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
    no_errors(x(Config, Q1)),
    ok.

star_wars_non_existent_field(Config) ->
    Q1 =
      "query HeroSpaceshipQuery {"
      "  hero {"
      "    favoriteSpaceship"
      "  }"
      "}",
    errors(x(Config, Q1)),
    ok.

star_wars_fields_on_objects(Config) ->
    Q1 =
      "query HeroSpaceshipQuery {"
      "  hero"
      "}",
    errors(x(Config, Q1)),
    ok.

star_wars_no_fields_on_scalars(Config) ->
    Q1 =
      "query HeroSpaceshipQuery {"
      "  hero { name { firstCharacterOfName } }"
      "}",
    errors(x(Config, Q1)),
    ok.

star_wars_no_fields_on_interfaces(Config) ->
    Q1 =
      "query DroidFieldOnCharacter {"
      "  hero { name primaryFunction }"
      "}",
    errors(x(Config, Q1)),
    ok.

star_wars_object_field_in_fragments(Config) ->
    Q1 =
      "query DroidFieldInFragment { hero { name ...DroidFields } }"
      "fragment DroidFields on Droid { primaryFunction }",
    no_errors(x(Config, Q1)),
    ok.

star_wars_object_field_in_inline_fragments(Config) ->
    Q1 =
      "query DroidFieldInFragment { hero { name ... on Droid { primaryFunction } } }",
    no_errors(x(Config, Q1)),
    ok.

%% -- ENUMeration TYPE TESTS ----------------------------------

enum_input(Config) ->
    Q1 = "{ colorInt(fromEnum: GREEN) }",
    #{ data := #{ <<"colorInt">> := 1 }} = x(Config, Q1),
    ok.

enum_output(Config) ->
    Q1 = "{ colorEnum(fromInt: 1) }",
    #{ data := #{ <<"colorEnum">> := <<"GREEN">> }} = x(Config, Q1),
    ok.

enum_input_output(Config) ->
    Q1 = "{ colorEnum(fromEnum: GREEN) }",
    #{ data := #{ <<"colorEnum">> := <<"GREEN">> }} = x(Config, Q1),
    ok.

enum_no_string_literal(Config) ->
    Q1 = "{ colorEnum(fromEnum: \"GREEN\") }",
    errors(x(Config, Q1)),
    ok.

enum_no_incorrect_internal_value(Config) ->
    Q1 = "{ colorEnum(fromString: \"GREEN\") }",
    #{ data := #{
        <<"colorEnum">> := null }} = x(Config, Q1),
    ok.

enum_no_internal_enum(Config) ->
    Q1 = "{ colorEnum(fromEnum: 1) }",
    errors(x(Config, Q1)),
    ok.

enum_no_literal_int(Config) ->
    Q1 = "{ colorEnum(fromInt: GREEN) }",
    errors(x(Config, Q1)),
    ok.
    
enum_accept_json(Config) ->
    Q1 = "query test($color: Color!) { colorEnum(fromEnum: $color) }",
    #{ data :=
       #{ <<"colorEnum">> := <<"BLUE">> }} =
           x(Config, Q1, <<"test">>, #{ <<"color">> => <<"BLUE">> }),
    ok.

enum_mutation_accept_json(Config) ->
    Q1 = "mutation x($color: Color!) { favoriteEnum(color: $color) }",
    #{ data :=
       #{ <<"favoriteEnum">> := <<"GREEN">> }} =
           x(Config, Q1, <<"x">>, #{ <<"color">> => <<"GREEN">> }),
    ok.

enum_no_accept_internal_query(Config) ->
    Q1 = "query test($color: Color!) { colorEnum(fromEnum: $color) }",
    errors(x(Config, Q1, <<"test">>, #{ <<"color">> => 2 })),
    ok.
    
enum_no_accept_internal_query_2(Config) ->
    Q1 = "query test($color: String!) { colorEnum(fromEnum: $color) }",
    errors(x(Config, Q1, <<"test">>, #{ <<"color">> => <<"BLUE">> })),
    ok.

enum_no_accept_internal_query_3(Config) ->
    Q1 = "query test($color: Int!) { colorEnum(fromEnum: $color) }",
    errors(x(Config, Q1, <<"test">>, #{ <<"color">> => 2 })),
    ok.

enum_internal_zero(Config) ->
    Q1 = "{ colorEnum(fromEnum: RED) colorInt(fromEnum: RED) }",
    #{ data :=
        #{ <<"colorEnum">> := <<"RED">>,
            <<"colorInt">> := 0 }} = x(Config, Q1),
    ok.

enum_input_nullable(Config) ->
    Q1 = "{ colorEnum colorInt }",
    #{ data := #{
    	<<"colorEnum">> := null,
    	<<"colorInt">> := null
    }} = x(Config, Q1),
    ok.

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
                    <<"keywords">> := [<<"foo">>,<<"bar">>],
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
                  = x(Config, Q),
    ok.

is_supports_type(Config) ->
    Query = "{ __type(name: \"TestType\") { name } }",
    #{ data := #{
        <<"__type">> := #{
            <<"name">> := <<"TestType">> } } } = x(Config, Query),
    ok.

unions(Config) ->
    ct:log("Initial query on the schema"),
    Goblin = base64:encode(<<"monster:1">>),
    Q1 = "{ goblin: monster(id: \"" ++ binary_to_list(Goblin) ++ "\") { id name hitpoints } }",
    #{ data := #{
        <<"goblin">> := #{
            <<"id">> := Goblin,
            <<"name">> := <<"goblin">>,
            <<"hitpoints">> := 10 }}} = x(Config, Q1),
            
    ct:log("Same query, but on items"),
    Q2 =
        "{ goblin: thing(id: \"" ++ binary_to_list(Goblin) ++ "\") { ...MonsterFragment } } "
        "fragment MonsterFragment on Monster { id name hitpoints }",
    #{ data := #{
        <<"goblin">> := #{
            <<"id">> := Goblin,
            <<"name">> := <<"goblin">>,
            <<"hitpoints">> := 10 }}} = x(Config, Q2),
    ok.

union_errors(Config) ->
    ct:log("You may not request fields on unions"),
    Q1 = "{ goblin: thing(id: \"bW9uc3Rlcjox\") { id} }",
    errors(x(Config, Q1)),
    ok.


populate(Config) ->
    ct:log("Create a monster in the dungeon"),
    QM =
        "mutation IMonster($input : IntroduceMonsterInput!) { "
        "  introduceMonster(input: $input) { "
        "    clientMutationId "
        "    monster { "
        "     id "
        "     name "
        "     color "
        "     hitpoints "
        "     mood "
        "    }}}",
    Input = #{
      <<"clientMutationId">> => <<"MUTID">>,
      <<"name">> => <<"orc">>,
      <<"color">> => <<"#593E1A">>,
      <<"hitpoints">> => 30,
      <<"mood">> => <<"AGGRESSIVE">>
     },
    Expected = #{ data => #{
                     <<"introduceMonster">> => #{
                       <<"clientMutationId">> => <<"MUTID">>,
                       <<"monster">> => #{
                         <<"id">> => base64:encode(<<"monster:2">>),
                         <<"name">> => <<"orc">>,
                         <<"color">> => <<"#593E1A">>,
                         <<"hitpoints">> => 30,
                         <<"mood">> => <<"AGGRESSIVE">>}
                      }}},
    Expected = x(Config, QM, <<"IMonster">>, #{ <<"input">> => Input }),

    ct:log("Missing names should lead to failure"),
    MissingNameInput = #{
      <<"clientMutationId">> => <<"MUTID">>,
      <<"hitpoints">> => 30
     },
    errors(x(Config, QM, <<"IMonster">>, #{ <<"input">> => MissingNameInput })),
    
    ct:log("Missing an input field should lead to failure"),
    errors(x(Config, QM, <<"IMonster">>, #{ })),
    
    ct:log("Using the wrong type should lead to failure"),
    errors(x(Config, QM, <<"IMonster">>,
        #{ <<"input">> => Input#{ <<"hitpoints">> := <<"hello">> }})),

    ct:log("Creating with a default parameter"),
    HobgoblinInput = #{
    	<<"clientMutationId">> => <<"MUTID">>,
    	<<"name">> => <<"hobgoblin">>,
    	<<"color">> => <<"#266A2E">>
    },
    HobgoblinExpected = #{ data => #{
                     <<"introduceMonster">> => #{
                       <<"clientMutationId">> => <<"MUTID">>,
                       <<"monster">> => #{
                         <<"color">> => <<"#266A2E">>,
                         <<"id">> => base64:encode(<<"monster:3">>),
                         <<"name">> => <<"hobgoblin">>,
                         <<"hitpoints">> => 15,
                         <<"mood">> => <<"DODGY">>
                       }
         }}},
    HobgoblinExpected = x(Config, QM, <<"IMonster">>, #{ <<"input">> => HobgoblinInput }),
    
    ct:log("Create a room"),
    QR =
        "mutation IRoom($input : IntroduceRoomInput!) { "
        "  introduceRoom(input: $input) { "
        "    clientMutationId "
        "    room { "
        "     id "
        "     description "
        "    }}}",
    RoomInput = #{
        <<"clientMutationId">> => <<"MUTID">>,
        <<"description">> => <<"This is the dungeon entrance">> },
    ExpectedR = #{ data => #{
                     <<"introduceRoom">> => #{
                       <<"clientMutationId">> => <<"MUTID">>,
                       <<"room">> => #{
                         <<"id">> => base64:encode(<<"room:1">>),
                         <<"description">> => <<"This is the dungeon entrance">> }
                      }}},
    ExpectedR = x(Config, QR, <<"IRoom">>, #{ <<"input">> => RoomInput }),
    
    ct:log("Put a monster in a room"),
    QPut =
        "mutation SM($input : SpawnMinionInput!) { "
        "  spawnMinion(input: $input) { "
        "    clientMutationId "
        "    room { "
        "      id "
        "      description "
        "      contents { ...SimpleMonster }"
        "    } "
        "    monster { "
        "      name "
        "      id "
        "    }}} "
        " fragment SimpleMonster on Monster { name hitpoints }",
    SpawnInput = #{
        <<"clientMutationId">> => <<"MUTID">>,
        <<"monsterId">> => base64:encode(<<"monster:2">>),
        <<"roomId">> => base64:encode(<<"room:1">>) },
    ExpectedSM = #{ data => #{
                     <<"spawnMinion">> => #{
                       <<"clientMutationId">> => <<"MUTID">>,
                       <<"room">> => #{
                         <<"id">> => base64:encode(<<"room:1">>),
                         <<"description">> => <<"This is the dungeon entrance">>,
                         <<"contents">> => [#{ <<"name">> => <<"orc">>, <<"hitpoints">> => 30 }]},
                       <<"monster">> => #{
                         <<"id">> => base64:encode(<<"monster:2">>),
                         <<"name">> => <<"orc">> }
                      }}},
    ExpectedSM = x(Config, QPut, <<"SM">>, #{ <<"input">> => SpawnInput }),
    ok.

direct_input(Config) ->
    QM =
        "mutation IMonster { "
        "  introduceMonster(input: {name: \"Albino Hobgoblin\", color: \"#ffffff\", hitpoints: 5, mood: AGGRESSIVE}) { "
        "    clientMutationId "
        "    monster { "
        "     id "
        "     name "
        "     color "
        "     hitpoints "
        "     mood "
        "    }}}",
    Expected = #{ data => #{
        <<"introduceMonster">> => #{
            <<"clientMutationId">> => null,
            <<"monster">> => #{
                <<"id">> => base64:encode(<<"monster:4">>),
                <<"name">> => <<"Albino Hobgoblin">>,
                <<"color">> => <<"#ffffff">>,
                <<"hitpoints">> => 5,
                <<"mood">> => <<"AGGRESSIVE">>}
        }}},
    Expected = x(Config, QM).

inline_fragment(Config) ->
    ID = base64:encode(<<"monster:1">>),
    Q =
      "query TEST { "
      "  thing(id: \"" ++ binary_to_list(ID) ++ "\") { "
      "     ... on Monster { "
      "         id hitpoints "
      "     } "
      "  } "
      "}",
    Expected = #{ data => #{
        <<"thing">> => #{
            <<"id">> => ID,
            <<"hitpoints">> => 10 }
    }},
    Expected = x(Config, Q).

fragment_over_union_interface(Config) ->
    ID = base64:encode(<<"monster:1">>),
    Q = "query TEST { monster(id: \"" ++ binary_to_list(ID) ++ "\") { "
        " ... on Thing { ... on Monster { id } } } }",
    Expected = #{ data => #{
    	<<"monster">> => #{
    		<<"id">> => ID }}},
    Expected = x(Config, Q),
    ct:log("Same as before, but on a named fragment instead"),
    Q2 =
        "query TEST { monster(id: \"" ++ binary_to_list(ID) ++ "\") { "
        "   ... TFrag } } "
        " fragment TFrag on Thing { ... on Monster { id } } ",
    Expected = x(Config, Q2),
    
    ct:log("Same as before, but via an interface type"),
    Q3 =
        "query TEST { monster(id: \"" ++ binary_to_list(ID) ++ "\") { "
        "   ... on Node { id } } } ",
    Expected = x(Config, Q3),
    ok.

simple_field_merge(Config) ->
    ID = base64:encode(<<"monster:1">>),
    Q = "query TEST { monster(id: \"" ++ binary_to_list(ID) ++ "\") { "
        " id hitpoints hitpoints } }",
    Expected = #{ data => #{
    	<<"monster">> => #{
    		<<"id">> => ID,
    		<<"hitpoints">> => 10 }}},
    Expected = x(Config, Q).

v_5_1_1_1(_Config) ->
    Q1 =
      "query getDogName { dog { name } } "
      "query getOwnerName { dog { owner { name } } }",
    true = v(Q1),
    
    Q2 =
      "query getName { dog { name } } "
      "query getName { dog { owner { name } } }",
    false = v(Q2),
    
    Q3 =
      "query dogOperation { dog { name } } "
      "mutation dogOperation { mutateDog { id } } ",
    false = v(Q3),
    ok.

v_5_1_2_1(_Config) ->
    Q1 = "{ dog { name } }",
    true = v(Q1),
    Q2 = "{ dot { name } } query getName { dog { owner { name } } }",
    false = v(Q2),
    ok.

v_5_2_1(_Config) ->
    Q1 =
      "{ dog { ...fieldNotDefined } } "
      "fragment fieldNotDefined on Dog { meowVolume }",
    false = v(Q1),
    
    Q2 =
      "{ dog { ...lyingFragment } } "
      "fragment lyingFragment on Dog { barkVolume: kawVolume }",
    false = v(Q2),
    
    Q3 =
      "{ dog { ...PetFragment } } "
      "fragment PetFragment on Pet { name }",
    true = v(Q3),
    
    Q4 =
      "{ dog { ...PetFragment } } "
      "fragment PetFragment on Pet { nickname }",
    false = v(Q4),
    
    Q5 =
      "{ dog { ...Fragment } } "
      "fragment Fragment on CatOrDog { __typename ... on Pet { name } ... on Dog { barkVolume } }",
    true = v(Q5),
    
    Q6 =
      "{ dog { ...Fragment } } "
      "fragment Fragment on CatOrDog { name barkVolume }",
    false = v(Q6),
    ok.

v_5_2_3(_Config) ->
    Q1 =
       "{ dog { ...scalarSelection } } fragment scalarSelection on Dog { barkVolume }",
    true = v(Q1),
    
    Q2 =
       "{ dog { ...Fragment }} fragment Fragment on Dog { barkVolume { sinceWhen }}",
    false = v(Q2),
    
    false = v("query Q { human } }"),
    false = v("query Q { pet } }"),
    false = v("query Q { catOrDog } }"),
    ok.

v_5_3_1(_Config) ->
    true = v(
      "{ dog { ...F } } fragment F on Dog { doesKnowCommand(dogCommand: SIT) }"),
      
    false = v(
      "{ dog { ...F } } fragment F on Dog { doesKnowCommand(command: CLEAN_UP_HOUSE) }"),
    
    true = v(
      "{ dog { ...F } } fragment F on Arguments { multipleReqs(x: 1, y: 2) }"),
    
    true = v(
      "{ dog { ...F } } fragment F on Arguments { multipleReqs(y:1, x: 2) }"),

    ok.

v_5_3_2(_Config) ->
    true = v(
      "{ dog { ...F } } fragment F on Dog { doesKnowCommand(dogCommand: SIT) }"),
      
    false = v(
      "{ dog { ...F } } fragment F on Dog { doesKnowCommand(dogCommand: SIT, dogCommand: HEEL) }"),
    ok.

v_5_4_1_1(_Config) ->
    Q1 =
        "{ dog { ...fragOne ...fragTwo } } "
        "fragment fragOne on Dog { name } "
        "fragment fragTwo on Dog { owner { name } }",
    true = v(Q1),
    
    Q2 =
        "{ dog { ...fragOne } } "
        "fragment fragOne on Dog { name } "
        "fragment fragOne on Dog { owner { name } }",
    false = v(Q2),
    ok.

v_5_4_1_2(_Config) ->
    Q1 =
        "{ dog { ...correctType } } fragment correctType on Dog { name }",
    true = v(Q1),
    
    Q2 =
        "{ dog { ...inlineFragment } } fragment inlineFragment on Dog { ... on Dog { name } }",
    true = v(Q2),
    
    %% Note: This doesn't validate if we have no directives support. The test case will fail once
    %% we have it in which case the test here should pass.
    Q3 =
        "{ dog { ...inlineFragment2 } } fragment inlineFragment2 on Dog { ... @include(if: true) { name } }",
    false = v(Q3),
    
    Q4 =
        "{ dog { ...F } } fragment F on NotInSchema { name }",
    false = v(Q4),
    
    Q5 =
        "{ dog { ...F } } fragment F on Dog { ... on NotInSchema { name } }",
    false = v(Q5),
    ok.

v_5_4_1_3(_Config) ->
    true = v(
      "{ dog { ...F } } fragment F on Dog { name }"),
    true = v(
      "{ dog { ...F } } fragment F on Pet { name }"),
    true = v(
      "{ dog { ...F } } fragment F on CatOrDog { ... on Dog { name } }"),
      
    false = v(
      "{ dog { ...F } } fragment F on Int { something }"),
    false = v(
      "{ dog { ...F } } fragment F on Dog { ... on Boolean { somethingElse } }"),
    false = v(
      "{ dog { ...F } } fragment F on DogCommand { argh }"),
    ok.

v_5_4_2_1(_Config) ->
    false = v("{ dog { ...undefinedFragment } }"),
    ok.

v_5_4_2_2(_Config) ->
    Q1 =
      "{ dog { ...nameFragment } } "
      "fragment nameFragment on Dog { name ...barkVolumeFragment } "
      "fragment barkVolumeFragment on Dog { barkVolume ...nameFragment }",
    false = v(Q1),
    
    Q2 =
      "{ dog { ...dogFragment } } "
      "fragment dogFragment on Dog { name owner { ...ownerFragment } } "
      "fragment ownerFragment on Human { name pets { ...dogFragment } }",
    false = v(Q2),
    ok.

v_5_4_2_3_1(_Config) ->
   true = v(
     "{ dog { ...F } } fragment F on Dog { ... on Dog { barkVolume } }"),
   
   false = v(
     "{ dog { ...F } } fragment F on Dog { ... on Cat { meowVolume } }"),
     
   ok.

%% -- INTERNALS ----------------------------------

errors(#{ errors := [] }) -> ct:fail(no_errors_present);
errors(#{ errors := _}) -> ok;
errors(#{ }) -> ct:fail(no_errors_present).

no_errors(#{ errors := [] }) -> ok;
no_errors(#{ errors := Es }) ->
    ct:log("Errors: ~p", [Es]),
    ct:fail(errors_present);
no_errors(#{}) -> ok.

%% v/1 predicates valid queries
v(Q) ->
    ct:log("Query: ~s", [Q]),
    case graphql:parse(Q) of
        {ok, AST} ->
            try
               Elab = graphql:elaborate(AST),
               {ok, #{
                   ast := AST2,
                   fun_env := _FunEnv }} = graphql:type_check(Elab),
               ok = graphql:validate(AST),
               true
            catch
                Class:Err ->
                  ct:log("Error: ~p", [{Class, Err, erlang:get_stacktrace()}]),
                  false
            end;
        Err ->
            ct:log("Error: ~p", [Err]),
            false
    end.

x(Config, Input) -> x(Config, Input, #{}).

x(Config, Input, Params) -> x(Config, Input, undefined, Params).

x(Config, Input, OpName, Params) ->
    ct:log("Query: ~s", [Input]),
    Track0 = track_new(),
    case graphql:parse(Input) of
        {ok, AST} ->
           Track1 = track(parse, Track0),
           ct:log("AST: ~p", [AST]),
           ct:log("Params: ~p", [Params]),
           try
               Elaborated = graphql:elaborate(AST),
               ct:log("Elaborated: ~p", [Elaborated]),
               Track2 = track(elaborate, Track1),
               {ok, #{
                   ast := AST2,
                   fun_env := FunEnv }} = graphql:type_check(Elaborated),
               Track3 = track(type_check, Track2),
               CoercedParams = graphql:type_check_params(FunEnv, OpName, Params),
               Track4 = track(type_check_params, Track3),
               Ctx = #{ params => CoercedParams },
               ok = graphql:validate(AST2),
               Track5 = track(validate, Track4),
               Res = case OpName of
                   undefined -> graphql:execute(Ctx, AST2);
                   Op -> graphql:execute(Ctx#{ operation_name => Op }, Elaborated)
               end,
               
               Track6 = track(execute, Track5),
               ct:log("Result: ~p", [Res]),
               track_report(Config, Track6),
               Res
            catch
                throw:{error, Error} -> #{ errors => Error }
            end;
         Err ->
             Err
    end.

is_schema() ->
    Test = {object, #{
    	id => 'TestType',
    	description => "A simple test object.",
    	fields => #{
    		testField => #{ type => string, description => "A test field" }
    	}
    }},
    Schema = {root, #{
    	query => 'TestType'
    }},
    ok = graphql:insert_schema_definition(Test),
    ok = graphql:insert_schema_definition(Schema),
    ok.

track_ets() ->
    ets:new(graphql_SUITE_track, [named_table, public, {keypos, 1}]),
    ok.

track_new() ->
    T = erlang:monotonic_time(),
    #{ '$last' => T }.
    
track(Event, #{ '$last' := Start } = M) ->
    End = erlang:monotonic_time(),
    Diff = erlang:convert_time_unit(End - Start, native, micro_seconds),
    M#{ '$last' := End, Event => Diff}.

track_report(Config, M) ->
    Name = proplists:get_value(name, ?config(tc_group_properties, Config)),
    ct:log("Timings ~p: ~p", [Name, maps:remove('$last', M)]).

