-module(dungeon_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-compile([export_all, nowarn_export_all]).

suite() ->
    [{timetrap,
      {seconds, 10}}].

init_per_suite(Config) ->
    application:ensure_all_started(graphql),
    {ok, Doc} = read_doc(Config, "query.graphql"),
    ok = dungeon:inject(Config),
    ok = dungeon:start(),
    ok = graphql:validate_schema(),
    GoblinId1 = base64:encode(<<"monster:1">>),
    GoblinId2 = base64:encode(<<"monster:2">>),
    NonExistentGoblinId1 = base64:encode(<<"monster:3">>),
    RoomId = base64:encode(<<"room:1">>),
    NonExistentRoom = base64:encode(<<"room:2">>),
    [ {document, Doc}
    , {known_goblin_id_1, GoblinId1}
    , {known_goblin_id_2, GoblinId2}
    , {known_non_existent_goblin_id_1, NonExistentGoblinId1}
    , {known_room_id, RoomId}
    , {known_non_existent_room_id, NonExistentRoom}
    | Config].

end_per_suite(_Config) ->
    dungeon:stop(),
    graphql_schema:reset(),
    application:stop(graphql),
    ok.

init_per_group(_Group, Config) ->
    Config.

end_per_group(_Group, _Config) ->
    ok.

init_per_testcase(x, Config) ->
    {ok, _} = dbg:tracer(),
    dbg:p(all, c),
    dbg:tpl(graphql_execute, does_fragment_type_apply, '_', cx),
    Config;
init_per_testcase(_Case, Config) ->
    Config.

end_per_testcase(x, _Config) ->
    dbg:stop_clear(),
    ok;
end_per_testcase(_Case, _Config) ->
    ok.

groups() ->
    Dungeon =
        {dungeon, [],
         [ unions,
           defer,
           union_errors,
           error_handling,
           scalar_output_coercion,
           populate,
           default_query,
           direct_input,
           fixed_input,
           nested_input_object,
           inline_fragment,
           fragment_over_union_interface,
           integer_in_float_context,
           scalar_as_expression_coerce,
           non_null_field,
           complex_modifiers,
           simple_field_merge,
           nested_field_merge,
           multiple_monsters_and_rooms,
           include_directive,
           introspection,
           introspection_with_variable,
           get_operation,
           coercion_int_float,
           replace_enum_representation,
           auxiliary_data,
           find_monster,
           find_monster_singleton,
           invalid_scalar_int_input,
           introspect_default_value,
           default_parameter,
           subscribe_monster_introduced,
           subscribe_thing_introduced,
           multiple_subscriptions,
           subscribe_error ]},
    Errors =
        {errors, [],
         [ unknown_variable,
           null_input,
           missing_fragment,
           quoted_input_error,
           input_coerce_error_exception,
           input_coerce_error,
           invalid_enums,
           invalid_enum_result,
           invalid_type_resolution,
           duplicate_validation,
           invalid_list_resolver ]},
    %% Groups
    [ Dungeon
    , Errors
    ].

all() ->
    [ {group, dungeon}
    , {group, errors}
    ].

read_doc(Config, File) ->
    DocFile = filename:join(
                [?config(data_dir, Config), File]),
    file:read_file(DocFile).

run(Config, Q, Params) ->
    Doc = ?config(document, Config),
    th:x(Config, Doc, Q, Params).

run(Config, File, Q, Params) ->
    {ok, Doc} = read_doc(Config, File),
    th:x(Config, Doc, Q, Params).

subscription_receive(0) -> [];
subscription_receive(N) ->
    receive
        {subscription_msg, Subscription, Msg} ->
            [{Subscription, Msg} | subscription_receive(N - 1)]
    after 5000 ->
            error(timeout)
    end.

default_query(Config) ->
    ID = ?config(known_goblin_id_1, Config),
    #{ data := #{ <<"goblin">> := #{ <<"id">> := ID, <<"name">> := <<"goblin!">>, <<"hitpoints">> := 10 }}} =
        run(Config, <<"GoblinQuery">>, #{}),
    #{ data := #{ <<"goblin">> := #{ <<"id">> := ID, <<"name">> := <<"goblin!">>, <<"stats">> := [#{ <<"attack">> := 3 }] }}} =
        run(Config, <<"MinGoblin">>, #{<<"minAttack">> => 0 }),
    #{ data := #{ <<"goblin">> := #{ <<"id">> := ID, <<"name">> := <<"goblin!">>, <<"stats">> := [] }}} =
        run(Config, <<"MinGoblin">>, #{<<"minAttack">> => 30 }),
    ok.

introspection(Config) ->
    case run(Config,
             <<"introspection.graphql">>,
             <<"IntrospectionQuery">>, #{}) of
        #{ errors := [] } ->
            ok;
        #{ errors := Errs } ->
            ct:log("Errors: ~p", [Errs]),
            ct:fail(introspection_errors);
        #{ } ->
            ok %% No Errors present, so this is OK
    end.

introspection_with_variable(Config) ->
    case run(Config,
             <<"introspection_with_variable.graphql">>,
             <<"IntrospectionQuery">>, #{<<"includeDeprecated">> => false}) of
        #{ errors := [] } ->
            ok;
        #{ errors := Errs } ->
            ct:log("Errors: ~p", [Errs]),
            ct:fail(introspection_errors);
        #{ } ->
            ok %% No Errors present, so this is OK
    end.

invalid_list_resolver(Config) ->
    GoblinId = ?config(known_goblin_id_1, Config),
    Q1 = "query Q { monster(id: \"" ++ binary_to_list(GoblinId) ++ "\") { errorListResolution }} ",
    Expected =
        #{data => #{<<"monster">> => #{<<"errorListResolution">> => null}},
          errors =>
              [#{ extensions => #{code => list_resolution },
                  message =>
                      <<"Internal Server error: A list is being incorrectly resolved">>,
                  path =>
                      [<<"monster">>,<<"errorListResolution">>]}]},
    Expected = th:x(Config, Q1),
    ok.

duplicate_validation(Config) ->
    GoblinId = ?config(known_goblin_id_1, Config),
    Q1 = "query Q { monster(id: \"" ++ binary_to_list(GoblinId) ++ "\") { name }} ",
    #{ errors := _} = th:x(Config, Q1 ++ Q1),
    ok.

coercion_int_float(Config) ->
    GoblinId = ?config(known_goblin_id_1, Config),
    Expected = #{ data => #{<<"monster">> => #{ <<"spikyness">> => 5.0 }}},
    Q1 = "{ monster(id: \"" ++ binary_to_list(GoblinId) ++ "\") { spikyness }}",
    Expected = th:x(Config, Q1),
    ok.

get_operation(Config) ->
    GoblinId = ?config(known_goblin_id_1, Config),
    Expected = #{ data => #{<<"monster">> => #{ <<"name">> => <<"goblin!">> }}},
    Q1 = "{ monster(id: \"" ++ binary_to_list(GoblinId) ++ "\") { name }}",
    Expected = th:x(Config, Q1),
    Q2 = "query Q { monster(id: \"" ++ binary_to_list(GoblinId) ++ "\") { name }}",
    Expected = th:x(Config, Q2),
    Q3 = "query Q($id : ID!) { monster(id: $id) { name }}",
    Expected = th:x(Config, Q3, <<"Q">>, #{ <<"id">> => GoblinId }),
    Expected = th:x(Config, Q3, #{ <<"id">> => GoblinId }),
    ok.

include_directive(Config) ->
    GoblinId = ?config(known_goblin_id_1, Config),
    case run(Config, <<"GoblinQueryDirectives">>,
             #{ <<"fat">> => false }) of
        #{ data := #{ <<"goblin">> := Goblin }} ->
            case maps:is_key(<<"name">>, Goblin) of
                true -> ct:fail("Not handling include");
                false -> ok
            end
    end,
    #{ data := #{
         <<"goblin">> := #{
             <<"id">> := GoblinId,
             <<"name">> := <<"goblin!">>,
             <<"hitpoints">> := 10 }}} =
        run(Config, <<"GoblinQueryDirectives">>, #{ <<"fat">> => true }),

    ct:log("Do check for inline fragments of no type designator"),
    case run(Config, <<"GoblinQueryDirectivesInline">>,
             #{ <<"fat">> => false }) of
        #{ data := Data } ->
            GoblinIL = maps:get(<<"goblin">>, Data),
            0 = maps:size(maps:with([<<"name">>, <<"hitpoints">>], GoblinIL)),
            ok
    end,
    #{ data := #{
         <<"goblin">> := #{
             <<"id">> := GoblinId,
             <<"name">> := <<"goblin!">>,
             <<"hitpoints">> := 10 }}} =
        run(Config, <<"GoblinQueryDirectivesInline">>, #{ <<"fat">> => true }),
    ok.

unions(Config) ->
    ct:log("Initial query on the schema"),
    Monster = dungeon:create(monster, [{name, <<"goblin">>}, {hitpoints, 10}]),
    [{GoblinId, _Goblin}] = dungeon:batch_create([{Monster, insert}]),
    OpaqueId = dungeon:opaque_id(GoblinId),
    Expected1 = #{ data => #{
                     <<"goblin">> => #{
                     <<"id">> => OpaqueId,
                     <<"name">> => <<"goblin!">>,
                     <<"hitpoints">> => 10 }}},
    Expected1 = run(Config, <<"GoblinQuery">>, #{<<"id">> => OpaqueId}),
    ct:log("Same query, but on items"),
    Expected2 = #{ data => #{
                     <<"goblin">> => #{
                         <<"id">> => OpaqueId,
                         <<"name">> => <<"goblin!">>,
                         <<"hitpoints">> => 10 }}},
    Expected2 = run(Config, <<"GoblinThingQuery">>, #{ <<"id">> => OpaqueId }),
    ct:log("Union expansions"),
    Expected3 = #{ data => #{ <<"things">> => [#{}]}},
    Expected3 = run(Config, <<"ThingQ1">>, #{ }),

    Expected4 = #{ data => #{ <<"things">> => [#{ <<"__typename">> => <<"Monster">>, <<"name">> => <<"goblin!">> }]}},
    Expected4 = run(Config, <<"ThingQ2">>, #{ }),

    Expected5 = #{ data => #{ <<"things">> => [#{ <<"__typename">> => <<"Monster">> }]}},
    Expected5 = run(Config, <<"ThingQ3">>, #{ }),

    ok.

union_errors(Config) ->
    ct:log("You may not request fields on unions"),
    Monster = dungeon:create(monster),
    [{GoblinId, _Goblin}] = dungeon:batch_create([{Monster, insert}]),
    OpaqueId = dungeon:opaque_id(GoblinId),
    Query = iolist_to_binary(["{ goblin: thing(id: \"", OpaqueId ,"\") { id } }"]),
    Q1 = binary_to_list(Query),
    th:errors(th:x(Config, Q1)),
    ok.

scalar_output_coercion(Config) ->
    ct:log("Test output coercion"),
    Monster =
        dungeon:create(monster, [ {name, <<"goblin">>}
                                , {color, #{ r => 65, g => 146, b => 75}}
                                , {hitpoints, 10}
                                ]),
    [{GoblinId, _Goblin}] = dungeon:batch_create([{Monster, insert}]),
    OpaqueId = dungeon:opaque_id(GoblinId),
    #{ data := #{
        <<"goblin">> := #{
            <<"id">> := OpaqueId,
            <<"name">> := <<"goblin!">>,
            <<"color">> := <<"#41924B">>,
            <<"hitpoints">> := 10 }}} =
        run(Config, <<"ScalarOutputCoercion">>, #{ <<"id">> => OpaqueId }),
    ok.

invalid_enum_result(Config) ->
    ct:log("Test against an invalid enum representation"),
    #{ data := #{
         <<"goblin">> := #{
           <<"id">> := <<"bW9uc3Rlcjox">>,
           <<"mood">> := null }}} =
        run(Config, <<"InvalidEnumOutput">>, #{}),
    ok.

replace_enum_representation(Config) ->
    ct:log("Test replace enum representation"),
    Monster = dungeon:create(monster, [{mood, 'DODGY'}]),
    [{GoblinId, _Goblin}] = dungeon:batch_create([{Monster, insert}]),
    OpaqueId = dungeon:opaque_id(GoblinId),
    #{ data := #{
         <<"goblin">> := #{
             <<"id">> := OpaqueId,
             <<"mood">> := <<"DODGY">>
            }
        }} =
        run(Config, <<"ReplaceEnumRepresentation">>, #{ <<"id">> => OpaqueId }),
    ok.

populate(Config) ->
    ct:log("Create a monster in the dungeon"),
    Input = #{
      <<"clientMutationId">> => <<"MUTID">>,
      <<"name">> => <<"orc">>,
      <<"color">> => <<"#593E1A">>,
      <<"hitpoints">> => 30,
      <<"mood">> => <<"AGGRESSIVE">>
     },
    #{ data := #{
                     <<"introduceMonster">> := #{
                       <<"clientMutationId">> := <<"MUTID">>,
                       <<"monster">> := #{
                         <<"id">> := _,
                         <<"name">> := <<"orc">>,
                         <<"color">> := <<"#593E1A">>,
                         <<"hitpoints">> := 30,
                         <<"properties">> := [],
                         <<"mood">> := <<"AGGRESSIVE">>}
                      }}} = run(Config, <<"IntroduceMonster">>, #{ <<"input">> => Input }),

    ct:log("Missing names should lead to failure"),
    MissingNameInput = #{
      <<"clientMutationId">> => <<"MUTID">>,
      <<"hitpoints">> => 30
     },
    th:errors(run(Config, <<"IntroduceMonster">>, #{ <<"input">> => MissingNameInput })),

    ct:log("Missing an input field should lead to failure"),
    th:errors(run(Config, <<"IntroduceMonster">>, #{})),

    ct:log("Using the wrong type should lead to failure"),
    th:errors(run(Config, <<"IntroduceMonster">>,
        #{ <<"input">> => Input#{ <<"hitpoints">> := <<"hello">> }})),

    ct:log("Creating with a default parameter"),
    HobgoblinInput = #{
        <<"clientMutationId">> => <<"MUTID">>,
        <<"name">> => <<"hobgoblin">>,
        <<"color">> => <<"#266A2E">>
    },
    #{ data := #{
                     <<"introduceMonster">> := #{
                       <<"clientMutationId">> := <<"MUTID">>,
                       <<"monster">> := #{
                         <<"color">> := <<"#266A2E">>,
                         <<"id">> := _,
                         <<"name">> := <<"hobgoblin">>,
                         <<"hitpoints">> := 15,
                         <<"mood">> := <<"DODGY">>
                       }
         }}} = run(Config, <<"IntroduceMonster">>, #{ <<"input">> => HobgoblinInput }),

    ct:log("Create a room"),
    ExpectedRoomId = dungeon:next_id(room),
    RoomInput = #{
        <<"clientMutationId">> => <<"MUTID">>,
        <<"description">> => <<"This is the dungeon entrance">> },
    ExpectedR = #{ data => #{
                     <<"introduceRoom">> => #{
                       <<"clientMutationId">> => <<"MUTID">>,
                       <<"room">> => #{
                         <<"id">> => ExpectedRoomId,
                         <<"description">> => <<"This is the dungeon entrance">> }
                      }}},
    ExpectedR = run(Config, <<"IntroduceRoom">>, #{ <<"input">> => RoomInput }),

    ct:log("Put a monster in a room"),
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
    ExpectedSM = run(Config, <<"SpawnMinion">>, #{ <<"input">> => SpawnInput }),

    NestedInput = #{ <<"mood">> => <<"AGGRESSIVE">> },
    ExpectedNestedInput =
        #{data =>
              #{<<"introduceMonster">> =>
                    #{<<"clientMutationId">> => <<"123">>,
                      <<"monster">> =>
                          #{<<"color">> => <<"#888888">>,
                            <<"hitpoints">> => 100,
                            <<"id">> => <<"bW9uc3RlcjoxMDA3">>,
                            <<"mood">> => <<"AGGRESSIVE">>,
                            <<"name">> => <<"Giant Spider">>,
                            <<"plushFactor">> => 0.01,
                            <<"properties">> => [<<"BEAST">>],
                            <<"stats">> =>
                                [#{<<"attack">> => 12,
                                   <<"shellScripting">> => 0,
                                   <<"yell">> =>
                                       <<"I LOVE WEAVING!">>}]}}}},

    ExpectedNestedInput = run(Config, <<"IntroduceMonsterNestedVar">>, NestedInput),

    ExpectedDefaultNestedInput =
        #{data =>
              #{<<"introduceMonster">> =>
                    #{<<"clientMutationId">> => <<"123">>,
                      <<"monster">> =>
                          #{<<"color">> => <<"#444444">>,
                            <<"hitpoints">> => 9001,
                            <<"id">> => <<"bW9uc3RlcjoxMDA4">>,
                            <<"mood">> => <<"AGGRESSIVE">>,
                            <<"name">> => <<"Tiny Evil Cat">>,
                            <<"plushFactor">> => 57.0,
                            <<"properties">> => [<<"BEAST">>],
                            <<"stats">> =>
                                [#{<<"attack">> => 1337,
                                   <<"shellScripting">> => 10,
                                   <<"yell">> =>
                                       <<"Purrrrrrrrrrrrrr!">>}]}}}},

    ExpectedDefaultNestedInput = run(Config, <<"IntroduceMonsterDefaultNestedVar">>, #{}),

    ExpectedOptionalNestedInput =
        #{data =>
              #{<<"introduceMonster">> =>
                    #{<<"clientMutationId">> => <<"123">>,
                      <<"monster">> =>
                          #{<<"color">> => <<"#FFFFFF">>,
                            <<"hitpoints">> => 1,
                            <<"id">> => <<"bW9uc3RlcjoxMDA5">>,
                            <<"mood">> => <<"DODGY">>,
                            <<"name">> => <<"Teeny Tiny Mouse">>,
                            <<"plushFactor">> => 10.0,
                            <<"properties">> => [<<"BEAST">>],
                            <<"stats">> =>
                                [#{<<"attack">> => 1,
                                   <<"shellScripting">> => 1,
                                   <<"yell">> =>
                                       <<"Meek!">>}]}}}},

    ExpectedOptionalNestedInput = run(Config, <<"IntroduceMonsterOptionalNestedVar">>, #{}),


    ct:log("Check for proper null-handling"),

    ExpectedNullHandling =
        #{data =>
              #{<<"introduceMonster">> =>
                    #{<<"clientMutationId">> => <<"123">>,
                      <<"monster">> =>
                          #{<<"color">> => <<"#000">>,
                            <<"hitpoints">> => 9002,
                            <<"id">> => <<"bW9uc3RlcjoxMDEw">>,
                            <<"mood">> => <<"DODGY">>,
                            <<"name">> => <<"Tiny Black Hole">>,
                            <<"plushFactor">> => 0.01,
                            <<"properties">> => [<<"BEAST">>],
                            <<"stats">> =>
                                [#{<<"attack">> => 1,
                                   <<"shellScripting">> => 1,
                                   <<"yell">> =>
                                       <<"...">>}]}}}},

    ExpectedNullHandling = run(Config, <<"IntroduceMonsterNullHandling">>, #{}),

    ct:log("Check duplicate enum values (BEAST mood/property)"),

    DupEnumInput = #{
        <<"clientMutationId">> => <<"MUTID">>,
        <<"name">> => <<"orc">>,
        <<"color">> => <<"#593E1A">>,
        <<"hitpoints">> => 30,
        <<"properties">> => [<<"BEAST">>],
        <<"mood">> => <<"BEAST">>
    },
    #{ data := #{
        <<"introduceMonster">> := #{
            <<"clientMutationId">> := <<"MUTID">>,
            <<"monster">> := #{
                <<"id">> := _,
                <<"name">> := <<"orc">>,
                <<"color">> := <<"#593E1A">>,
                <<"hitpoints">> := 30,
                <<"properties">> := [<<"BEAST">>],
                <<"mood">> := <<"BEAST">>}
        }}} = run(Config, <<"IntroduceMonster">>, #{ <<"input">> => DupEnumInput }),
    ok.

invalid_scalar_int_input(Config) ->
    %% This is a test case where the creation of the input fails because
    %% The given integers will throw away information

    %% Base input is the following piece of data
    Input = #{
      <<"name">> => <<"Black Hobgoblin">>,
      <<"color">> => <<"#000000">>,
      <<"properties">> => [<<"DRAGON">>, <<"MURLOC">>],
      <<"mood">> => <<"AGGRESSIVE">>
    },

    #{ errors := [_] } =
        run(Config, <<"IntroduceMonster">>,
            #{ <<"input">> => Input#{ <<"hitpoints">> => 5.3 }}),

    #{ errors := [_] } =
        run(Config, <<"IntroduceMonster">>,
            #{ <<"input">> => Input#{ <<"hitpoints">> => 1 bsl 34 }}),
    ok.

direct_input(Config) ->
    Input = #{
      <<"name">> => <<"Albino Hobgoblin">>,
      <<"color">> => <<"#ffffff">>,
      <<"hitpoints">> => 5.0,
      <<"properties">> => [<<"DRAGON">>, <<"MURLOC">>],
      <<"mood">> => <<"AGGRESSIVE">>
    },
    #{ data := #{
        <<"introduceMonster">> := #{
            <<"clientMutationId">> := null,
            <<"monster">> := #{
                <<"id">> := _,
                <<"name">> := <<"Albino Hobgoblin">>,
                <<"color">> := <<"#FFFFFF">>,
                <<"hitpoints">> := 5,
                <<"properties">> := [<<"DRAGON">>, <<"MURLOC">>],
                <<"mood">> := <<"AGGRESSIVE">>,
                <<"stats">> := null}
        }}} = run(Config, <<"IntroduceMonster">>, #{ <<"input">> => Input}),
    ok.

fixed_input(Config) ->
    #{ data := #{
        <<"introduceMonster">> := #{
            <<"clientMutationId">> := <<"123">>,
            <<"monster">> := #{
                <<"id">> := _,
                <<"name">> := <<"Yellow Slime">>,
                <<"color">> := <<"#FFFF33">>,
                <<"hitpoints">> := 1337,
                <<"plushFactor">> := 0.01,
                <<"properties">> := [],
                <<"mood">> := <<"DODGY">>,
               <<"stats">> := null}
        }}} = run(Config, <<"IntroduceMonsterFatFixedInput">>, #{ }),
    ok.

nested_input_object(Config) ->
    Input = #{
      <<"clientMutationId">> => <<"123">>,
      <<"name">> => <<"Green Slime">>,
      <<"color">> => <<"#1be215">>,
      <<"hitpoints">> => 9001,
      <<"mood">> => <<"TRANQUIL">>,
      <<"stats">> => [#{
        <<"shellScripting">> => 5,
        <<"yell">> => <<"...">> }]
      },
    #{ data :=
           #{<<"introduceMonster">> := #{<<"clientMutationId">> := <<"123">>,
                                         <<"monster">> :=
                                             #{ <<"color">> := <<"#1BE215">>,
                                                <<"hitpoints">> := 9001,
                                                <<"mood">> := <<"TRANQUIL">>,
                                                <<"name">> := <<"Green Slime">>,
                                                <<"id">> := _,
                                                <<"plushFactor">> := PF,
                                                <<"stats">> :=
                                                    [#{ <<"attack">> := 7,
                                                        <<"shellScripting">> := 5,
                                                        <<"yell">> := <<"...">> }]}}}} =
        run(Config, <<"IntroduceMonsterFat">>, #{ <<"input">> => Input}),
    true = (PF - 0.01) < 0.00001,
    ok.

integer_in_float_context(Config) ->
    Input = #{
      <<"clientMutationId">> => <<"123">>,
      <<"name">> => <<"Brown Slime">>,
      <<"color">> => <<"#B7411E">>,
      <<"hitpoints">> => 7001,
      <<"mood">> => <<"TRANQUIL">>,
      <<"plushFactor">> => 1,
      <<"stats">> => [#{
        <<"attack">> => 7,
        <<"shellScripting">> => 5,
        <<"yell">> => <<"...">> }]},
    #{ data :=
                      #{<<"introduceMonster">> := #{<<"clientMutationId">> := <<"123">>,
                                                    <<"monster">> :=
                                                        #{ <<"color">> := <<"#B7411E">>,
                                                           <<"hitpoints">> := 7001,
                                                           <<"mood">> := <<"TRANQUIL">>,
                                                           <<"name">> := <<"Brown Slime">>,
                                                           <<"id">> := _,
                                                           <<"plushFactor">> := PF,
                                                           <<"stats">> := [#{
                                                            <<"attack">> := 7,
                                                            <<"shellScripting">> := 5,
                                                            <<"yell">> := <<"...">> }]}}}} =
             run(Config, <<"IntroduceMonsterFat">>, #{ <<"input">> => Input}),
    true = (PF - 1.0) < 0.00001,
    ok.

complex_modifiers(Config) ->
    Input = #{
      <<"clientMutationId">> => <<"123">>,
      <<"name">> => <<"Two-Headed Ogre">>,
      <<"color">> => <<"#2887C8">>,
      <<"hitpoints">> => 9002,
      <<"mood">> => <<"AGGRESSIVE">>,
      <<"plushFactor">> => 0.2,
      <<"stats">> => [
        %% Head ONE!
        #{
          % resolver for attack field on Stats returns {ok, null} when attack is 13
          <<"attack">> => 13,
          <<"shellScripting">> => 5,
          <<"yell">> => <<"I'M READY">> },
        %% Head TWO!
        #{
          <<"attack">> => 7,
          <<"shellScripting">> => 17,
          <<"yell">> => <<"I'M NOT READY!">> } ]},
    #{ data :=
                      #{<<"introduceMonster">> := #{
                          <<"monster">> := #{
                            <<"id">> := MonsterID } } } } =
             run(Config, <<"IntroduceMonsterFat">>, #{ <<"input">> => Input}),

    %% Standard Query
    #{ data :=
        #{ <<"monster">> := #{
            <<"stats">> := [
                null,
                #{
                  <<"attack">> := 7,
                  <<"shellScripting">> := 17,
                  <<"yell">> := <<"I'M NOT READY!">> } ]  }}} =
            run(Config, <<"MonsterStatsZero">>, #{ <<"id">> => MonsterID }),
    %% When the list is non-null, but there are the possibility of a null-value in the list
    %% and the list is correctly being rendered, then render the list as we expect.
    #{ data :=
          #{ <<"monster">> := #{
            <<"statsVariantOne">> := [
                null,
                #{
                  <<"attack">> := 7,
                  <<"shellScripting">> := 17,
                  <<"yell">> := <<"I'M NOT READY!">> } ]  }}} =
            run(Config, <<"MonsterStatsOne">>, #{ <<"id">> => MonsterID }),
    %% When the list must not contain null values, then an error in the list means the whole
    %% list becomes null, and this is a valid value. So return the list itself as the value 'null'
    #{ data :=
        #{ <<"monster">> := #{
            <<"statsVariantTwo">> := null  }}} =
            run(Config, <<"MonsterStatsTwo">>, #{ <<"id">> => MonsterID }),

    %% If the list may not be null, make sure the error propagates to the wrapper object.
    #{ data :=
        #{ <<"monster">> := null },
        errors := [#{path :=
                         [<<"monster">>, <<"statsVariantThree">>],
                     extensions := #{ code := null_value },
                     message := _} ,
                   #{path :=
                         [<<"monster">>, <<"statsVariantThree">>, 0],
                     extensions := #{ code := null_value },
                     message := _},
                   #{path :=
                         [<<"monster">>, <<"statsVariantThree">>, 0, <<"attack">>],
                     extensions := #{ code := null_value },
                     message := _}]
     } = run(Config, <<"MonsterStatsThree">>, #{ <<"id">> => MonsterID }),
    ok.

non_null_field(Config) ->
    Input = #{
      <<"clientMutationId">> => <<"123">>,
      <<"name">> => <<"Brown Slime">>,
      <<"color">> => <<"#B7411E">>,
      <<"hitpoints">> => 7001,
      <<"mood">> => <<"TRANQUIL">>,
      <<"plushFactor">> => 1,
      <<"stats">> => [#{
        <<"attack">> => 13,
        <<"shellScripting">> => 5,
        <<"yell">> => <<"...">> }]},
    #{ data :=
                      #{<<"introduceMonster">> := #{<<"clientMutationId">> := <<"123">>,
                                                    <<"monster">> :=
                                                        #{ <<"color">> := <<"#B7411E">>,
                                                           <<"hitpoints">> := 7001,
                                                           <<"mood">> := <<"TRANQUIL">>,
                                                           <<"name">> := <<"Brown Slime">>,
                                                           <<"id">> := _,
                                                           <<"plushFactor">> := PF,
                                                           <<"stats">> := [null] }}}} =
             run(Config, <<"IntroduceMonsterFat">>, #{ <<"input">> => Input}),
    true = (PF - 1.0) < 0.00001,
    ok.

scalar_as_expression_coerce(Config) ->
    ct:log("When inputtting a scalar as an expression, it must coerce"),
    #{ data :=
           #{<<"introduceMonster">> := #{<<"clientMutationId">> := <<"123">>,
                                         <<"monster">> :=
                                             #{ <<"color">> := <<"#1BE215">>,
                                                <<"hitpoints">> := 9001,
                                                <<"mood">> := <<"TRANQUIL">>,
                                                <<"name">> := <<"Green Slime">>,
                                                <<"id">> := _,
                                                <<"properties">> := [<<"MURLOC">>, <<"MECH">>],
                                                <<"plushFactor">> := PF,
                                                <<"stats">> := [#{
                                                    <<"attack">> := 7,
                                                    <<"shellScripting">> := 5,
                                                    <<"yell">> := <<"...">> }]}}}} =
             run(Config, <<"IntroduceMonsterFatExpr">>, #{ <<"properties">> => [<<"MURLOC">>,
                                                                                <<"MECH">>]}),
    true = (PF - 0.01) < 0.00001,
    ok.

multiple_monsters_and_rooms(Config) ->
    ID1 = ?config(known_goblin_id_1, Config),
    ID2 = ?config(known_goblin_id_2, Config),
    IDMissing = ?config(known_non_existent_goblin_id_1, Config),

    #{ data := #{
        <<"monsters">> := [
            #{ <<"id">> := ID1 }, #{ <<"id">> := ID2 } ]
     }} = run(Config, <<"MultipleMonsters">>, #{ <<"ids">> => [ID1, ID2] }),

    #{ data := #{
        <<"monsters">> := [
            #{ <<"id">> := ID1 }, #{ <<"id">> := ID2 } ]
     }} = run(Config, <<"MultipleMonstersExpr">>, #{}),

    #{ data := #{
        <<"monsters">> := [
            #{ <<"id">> := ID1 }, #{ <<"id">> := ID2 } , null ]},
       errors := [#{path := [<<"monsters">>, 2], message := <<"not_found">> }]
     } = run(Config, <<"MultipleMonsters">>, #{ <<"ids">> => [ID1, ID2, IDMissing] }),

    #{ data := #{
        <<"monsters">> := [
            #{ <<"id">> := ID1 }, null, #{ <<"id">> := ID2 }, null ]},
       errors := [#{path := [<<"monsters">>, 1], message := <<"not_found">>},
                  #{path := [<<"monsters">>, 3], message := <<"not_found">>}]
     } = run(Config, <<"MultipleMonstersExprMissing">>, #{}),

    Room1 = ?config(known_room_id, Config),
    NonExistentRoom = ?config(known_non_existent_room_id, Config),
    #{ data := #{
         <<"rooms">> := [#{<<"id">> := Room1}]}
     } = run(Config, <<"MultipleRooms">>, #{ <<"ids">> => [Room1]}),
    % look for an existing room and a non existing room
    #{ data := #{ <<"rooms">> := null },
       errors :=
           [#{path := [<<"rooms">>, 1],
              extensions := #{ code := null_value } },
            #{path := [<<"rooms">>, 1],
              extensions := #{ code := not_found } }]
     } = run(Config, <<"MultipleRooms">>,
             #{ <<"ids">> => [Room1, NonExistentRoom]}),
    ok.

error_handling(Config) ->
    Room1 = ?config(known_room_id, Config),
    Expected1 =
        #{data =>
              #{<<"room">> =>
                    #{<<"id">> => <<"cm9vbTox">>,<<"magic">> => null}},
          errors =>
              [#{extensions => #{ code => resolver_error },
                 message => <<"unsupported">>,
                 path => [<<"room">>,<<"magic">>]}]},
    Expected1 = run(Config, <<"RoomErrors1">>, #{ <<"id">> => Room1 }),

    Expected2 =
        #{data =>
              #{<<"room">> =>
                    #{<<"id">> => <<"cm9vbTox">>,<<"leyline">> => null}},
          errors =>
              [#{extensions => #{ code => internal_server_error },
                 message => <<"GraphQL Internal Server Error">>,
                 path => [<<"room">>,<<"leyline">>]}]},
    Expected2 = run(Config, <<"RoomErrors2">>, #{ <<"id">> => Room1 }),
    ok.

inline_fragment(Config) ->
    ID = ?config(known_goblin_id_1, Config),
    Expected = #{ data => #{
        <<"thing">> => #{
            <<"id">> => ID,
            <<"hitpoints">> => 10 }
    }},
    Expected = run(Config, <<"InlineFragmentTest">>, #{ <<"id">> => ID }),
    ok.

introspect_default_value(Config) ->
    #{data :=
          #{<<"__type">> :=
                #{<<"fields">> := Fields }}} =
        run(Config, <<"IntrospectionDefault">>, #{}),
    %% Check the two relevant entries in the monster
    [Color] = [F || F <- Fields, maps:get(<<"name">>, F) == <<"color">>],
    [Mood]  = [F || F <- Fields, maps:get(<<"name">>, F) == <<"mood">>],
    #{<<"args">> := [#{<<"defaultValue">> := <<"rgb">>,
                       <<"name">> := <<"colorType">>}],
      <<"name">> := <<"color">>} = Color,
    #{<<"args">> := [#{<<"defaultValue">> := null,
                       <<"name">> := <<"fail">>}],
      <<"name">> := <<"mood">>} = Mood,
    ok.

fragment_over_union_interface(Config) ->
    ID = ?config(known_goblin_id_1, Config),
    Expected = #{ data => #{
        <<"monster">> => #{
            <<"id">> => ID }}},
    Expected = run(Config, <<"FragmentOverUnion1">>, #{ <<"id">> => ID }),
    ct:log("Same as before, but on a named fragment instead"),
    Expected = run(Config, <<"FragmentOverUnion2">>, #{ <<"id">> => ID }),
    ct:log("Same as before, but via an interface type"),
    Expected = run(Config, <<"FragmentOverUnion3">>, #{ <<"id">> => ID }),
    ok.

find_monster(Config) ->
    Expected1 =
        lists:sort([#{<<"name">> => <<"goblin!">>},
                    #{<<"name">> => <<"Teeny Tiny Mouse!">>},
                    #{<<"name">> => <<"Tiny Black Hole!">>},
                    #{<<"name">> => <<"Auxiliary Undead!">>},
                    #{<<"name">> => <<"goblin!">>},
                    #{<<"name">> => <<"goblin!">>},
                    #{<<"name">> => <<"hobgoblin!">>},
                    #{<<"name">> => <<"Yellow Slime!">>},
                    #{<<"name">> => <<"goblin!">>},
                    #{<<"name">> => <<"goblin!">>}]),
    #{ data := #{<<"findMonsters">> := Out1 }} = run(Config, <<"FindQuery">>, #{}),
    Expected1 = lists:sort(Out1),
    #{ data := #{<<"findMonsters">> := Out2 }} = run(Config, <<"FindQueryParam">>, #{ <<"m">> => [<<"DODGY">>]}),
    Expected1 = lists:sort(Out2),
    ok.

defer(Config) ->
    TimeOut = 100,
    Expected1 = #{ data =>
                      #{<<"roll">> =>
                           #{<<"rollDefer">> => TimeOut}}},
    Expected1 = run(Config, <<"RollX1">>, #{ <<"delay">> => TimeOut}),

    TBeginX2 = erlang:monotonic_time(),
    true =
        try
            run(Config, <<"RollX2">>, #{ <<"delay">> => TimeOut}),
            throw(this_should_not_happen)
        catch
            exit:defer_timeout ->
                TEndX2 = erlang:monotonic_time(),
                DurationX2 = erlang:convert_time_unit(TEndX2 - TBeginX2, native, millisecond),
                true = DurationX2 > (TimeOut * 2)  andalso (DurationX2 < 300)
        end,

    TBeginX3 = erlang:monotonic_time(),
    true =
        try
            run(Config, <<"RollX3">>, #{ <<"delay">> => TimeOut}),
            throw(this_should_not_happen)
    catch
        exit:defer_timeout ->
           TEndX3 = erlang:monotonic_time(),
            DurationX3 = erlang:convert_time_unit(TEndX3 - TBeginX3, native, millisecond),
            true = DurationX3 > (TimeOut * 3) andalso (DurationX3 < 400)
    end,

    TBeginX5 = erlang:monotonic_time(),
    true =
        try
            run(Config, <<"RollX5">>, #{ <<"delay">> => TimeOut}),
            throw(this_should_not_happen)
        catch
            exit:defer_timeout ->
                TEndX5 = erlang:monotonic_time(),
                DurationX5 = erlang:convert_time_unit(TEndX5 - TBeginX5, native, millisecond),
                true = DurationX5 > (TimeOut * 5) andalso (DurationX5 < 600)
        end,
    ok.

find_monster_singleton(Config) ->
    Expected1 =
        lists:sort(
          [#{<<"name">> => <<"goblin!">>},
           #{<<"name">> => <<"Teeny Tiny Mouse!">>},
           #{<<"name">> => <<"Tiny Black Hole!">>},
           #{<<"name">> => <<"Auxiliary Undead!">>},
           #{<<"name">> => <<"goblin!">>},
           #{<<"name">> => <<"goblin!">>},
           #{<<"name">> => <<"hobgoblin!">>},
           #{<<"name">> => <<"Yellow Slime!">>},
           #{<<"name">> => <<"goblin!">>},
           #{<<"name">> => <<"goblin!">>}]),
    #{ data := #{ <<"findMonsters">> := Out1 }} = run(Config, <<"FindQuerySingleton">>, #{}),
    Expected1 = lists:sort(Out1),
    #{ data := #{ <<"findMonsters">> := Out2 }} = run(Config, <<"FindQueryParamSingleton">>, #{ <<"m">> => <<"DODGY">>}),
    Expected1 = lists:sort(Out2),
    ok.

simple_field_merge(Config) ->
    ID = ?config(known_goblin_id_1, Config),
    Expected = #{ data => #{
        <<"monster">> => #{
            <<"id">> => ID,
            <<"hitpoints">> => 10 }}},
    Expected = run(Config, <<"TestFieldMerge">>, #{ <<"id">> => ID }),
    ok.

nested_field_merge(Config) ->
    ID = ?config(known_goblin_id_1, Config),
    #{ data := #{
      <<"monster">> := #{
          <<"id">> := ID,
          <<"hitpoints">> := 10,
          <<"stats">> := [#{
              <<"attack">> := 3,
              <<"shellScripting">> := 3,
              <<"yell">> := <<"HELO">> }]
    }}} = run(Config, <<"TestNestedFieldMerge">>, #{ <<"id">> => ID }),
    ok.

auxiliary_data(Config) ->
    Monster = dungeon:create(monster, [{name, <<"Auxiliary Undead">>}]),
    [{UndeadId, _Undead}] = dungeon:batch_create([{Monster, insert}]),
    {ok, OpaqueId} = dungeon:wrap(UndeadId),
    Expected = #{
      aux => [{my_auxiliary_data, true}],
      data => #{ <<"monster">> => #{ <<"id">> => OpaqueId
                                   , <<"name">> => <<"Auxiliary Undead!">>}
               }
     },
    Expected = run(Config, <<"TestAuxiliaryData">>, #{<<"id">> => OpaqueId}).

unknown_variable(Config) ->
    ID = ?config(known_goblin_id_1, Config),
    #{ errors :=
          [#{extensions := #{ code := unbound_variable },
             path := [<<"GoblinQuery">>,
                      <<"monster">>,
                      <<"id">>]}]} =
        run(Config,
            "unknown_variable.graphql",
            <<"TestFieldMerge">>,
            #{ <<"id">> => ID }),
    ok.

missing_fragment(Config) ->
    ID = ?config(known_goblin_id_1, Config),
    #{ errors :=
           [#{extensions := #{ code := unknown_fragment },
              path := [<<"GoblinQuery">>,
                       <<"monster">>,
                       <<"GoblinFragment">>] }]} =
        run(Config,
            "missing_fragment.graphql",
            <<"GoblinQuery">>,
            #{ <<"id">> => ID }),
    ok.

null_input(Config) ->
    #{errors := [_]} =
        run(Config, <<"test_null_input_1.graphql">>, <<"TestNullInput">>, #{}),
    #{errors :=
          [#{extensions := #{ code := null_input },
             message :=
                 <<"The arg id is given a null, which is not allowed in the input path">>,
             path :=
                 [<<"TestNullInput">>,<<"room">>,
                  <<"id">>]}]} = run(Config, <<"test_null_input_2.graphql">>, <<"TestNullInput">>, #{}),
    %% The following bugs must fail because the value is null which is not allowed
    #{ errors := [#{ extensions := #{ code := type_mismatch }}]} =
        run(Config, <<"test_null_input_3.graphql">>, <<"TestNullInput">>, #{}),
    #{ errors := [#{ extensions := #{ code := type_mismatch }}]} =
        run(Config, <<"test_null_input_4.graphql">>, <<"TestNullInput">>,
            #{ <<"input">> =>
                   #{ <<"name">> => <<"Orc">>,
                      <<"description">> => <<"This is an ORC!">>,
                      <<"weight">> => null }}),
    #{ errors := [#{ extensions := #{ code := missing_non_null_param }}]} =
        run(Config, <<"test_null_input_4.graphql">>, <<"TestNullInput">>,
            #{ <<"input">> =>
                   #{ <<"name">> => <<"Orc">>,
                      <<"description">> => <<"This is an ORC!">> }}),
    #{ errors := [#{ extensions := #{ code := missing_non_null_param }}]} =
        run(Config, <<"test_null_input_5.graphql">>, <<"TestNullInput">>, #{}),
    ok.

quoted_input_error(Config) ->
    {error, {parser_error, {_Line, graphql_parser, _}}} =
        run(Config, "quoted_input.graphql", <<"IMonster">>, #{}).


invalid_type_resolution(Config) ->
    Input = #{
      <<"id">> => base64:encode(<<"kraken:1">>)
     },
    #{ data := #{ <<"thing">> := null },
       errors :=
           [#{ path := [<<"thing">>],
               extensions := #{ code := type_resolver_error },
               message := <<"Couldn't type-resolve: kraken">> }
             ]} = run(Config, <<"LookupThing">>, Input),
    ok.

invalid_enums(Config) ->
    Input = #{
      <<"clientMutationId">> => <<"MUTID">>,
      <<"name">> => <<"rat">>,
      <<"color">> => <<"#aaaaaa">>,
      <<"hitpoints">> => 3,
      <<"mood">> => <<"AGGRESSIF">>
     },
    #{errors :=
          [#{ extensions := #{ code := unknown_enum },
              path := [<<"IntroduceMonster">>,<<"input">>,<<"mood">>]}]} =
        run(Config, <<"IntroduceMonster">>, #{ <<"input">> => Input }),
    #{errors :=
          [ #{ extensions := #{ code := unknown_enum },
               path := [<<"IntroduceMonster">>,<<"input">>,<<"mood">>]}]} =
        run(Config, <<"IntroduceMonster">>, #{ <<"input">> => Input#{ <<"mood">> => <<"">> }}),
    #{errors :=
          [#{extensions := #{ code := unknown_enum },
             path := [<<"IntroduceMonster">>,<<"input">>,<<"mood">>]}]} =
          run(Config, <<"IntroduceMonster">>, #{ <<"input">> => Input#{ <<"mood">> => <<>> }}),
    #{errors :=
      [#{extensions := #{ code := type_mismatch },
         path := [<<"IntroduceMonster">>,<<"input">>,<<"mood">>]}]} =
        run(Config, <<"IntroduceMonster">>, #{ <<"input">> => Input#{ <<"mood">> => <<"DRAGON">> }}),
    #{ errors :=
           [#{ extensions := #{  code := unknown_enum },
               path := [<<"IMonster">>,<<"introduceMonster">>, <<"input">>, <<"mood">>]}]} =
        run(Config, "invalid_enum_1.graphql", <<"IMonster">>, #{}),
    #{ errors :=
           [#{ extensions := #{ code := enum_string_literal },
               path := [<<"IMonster">>,<<"introduceMonster">>, <<"input">>, <<"mood">>]}]} =
        run(Config, "invalid_enum_2.graphql", <<"IMonster">>, #{}),
    ok.

input_coerce_error(Config) ->
    Input = #{
      <<"clientMutationId">> => <<"MUTID">>,
      <<"name">> => <<"rat">>,
      <<"color">> => <<"Invalid Color">>,
      <<"hitpoints">> => 3,
      <<"mood">> => <<"AGGRESSIVE">>
     },
    #{errors := [#{extensions := #{ code := input_coercion },
                   path := [<<"IntroduceMonster">>,
                            <<"input">>,
                            <<"color">>]}]} =
        run(Config, <<"IntroduceMonster">>, #{ <<"input">> => Input }),
    ok.

input_coerce_error_exception(Config) ->
    Input = #{
      <<"clientMutationId">> => <<"MUTID">>,
      <<"name">> => <<"rat">>,
      <<"color">> => <<"#">>, %% Forces an exception in the dungeon code
      <<"hitpoints">> => 3,
      <<"mood">> => <<"AGGRESSIVE">>
     },
    #{errors := [#{extensions := #{ code := input_coerce_abort },
                   path := [<<"IntroduceMonster">>,
                            <<"input">>,
                            <<"color">>]}]} =
        run(Config, <<"IntroduceMonster">>, #{ <<"input">> => Input }),
    ok.

default_parameter(Config) ->
    ct:log("Run, and provide a colorType explicitly"),
    #{ data :=
           #{ <<"monster">> :=
                  #{ <<"color">> := <<"#727272">>}}} =
        run(Config, <<"GetMonster">>, #{ <<"id">> => <<"bW9uc3Rlcjox">>,
                                         <<"colorType">> => <<"gray">> }),
    ct:log("Run, and pick up the color type via the argument default"),
    #{ data :=
           #{ <<"monster">> :=
                  #{ <<"color">> := <<"#41924B">>}}} =
        run(Config, <<"GetMonster">>, #{ <<"id">> => <<"bW9uc3Rlcjox">> }),
    ok.


subscribe_monster_introduced(Config) ->
    #{subscription := {Subscription, SubCtx}} = run(Config, <<"MonsterIntroduced">>, #{}),
    Input = #{
      <<"clientMutationId">> => <<"MUTID">>,
      <<"name">> => <<"orc">>,
      <<"color">> => <<"#593E1A">>,
      <<"hitpoints">> => 30,
      <<"mood">> => <<"AGGRESSIVE">>
     },
    #{ data := #{
                     <<"introduceMonster">> := #{
                       <<"clientMutationId">> := <<"MUTID">>,
                       <<"monster">> := #{
                         <<"id">> := Id,
                         <<"name">> := <<"orc">>,
                         <<"color">> := <<"#593E1A">>,
                         <<"hitpoints">> := 30,
                         <<"properties">> := [],
                         <<"mood">> := <<"AGGRESSIVE">>}
                      }}} = run(Config, <<"IntroduceMonster">>, #{ <<"input">> => Input }),
    [{Subscription, Msg}] = subscription_receive(1),
    ?assertMatch(
       #{data :=
             #{<<"monsterIntroduced">> :=
                   #{<<"id">> := Id,
                     <<"name">> := <<"orc!">>,
                     <<"hp">> := 30}}},
       graphql:handle_subscription_event(#{}, Subscription, SubCtx, Msg)).

subscribe_thing_introduced(Config) ->
    #{subscription := {Subscription, SubCtx}} = run(Config, <<"ThingIntroduced">>, #{}),
    MonsterInput = #{
      <<"clientMutationId">> => <<"MUTID">>,
      <<"name">> => <<"orc-thing">>,
      <<"color">> => <<"#593E1A">>,
      <<"hitpoints">> => 30,
      <<"mood">> => <<"AGGRESSIVE">>
     },
    #{data :=
          #{<<"introduceMonster">> :=
               #{<<"monster">> :=
                     #{<<"id">> := MonsterId}}}} =
        run(Config, <<"IntroduceMonster">>, #{ <<"input">> => MonsterInput }),
    ItemInput = #{
      <<"clientMutationId">> => <<"MUTID">>,
      <<"name">> => <<"item-thing">>,
      <<"description">> => <<"item-description">>,
      <<"weight">> => 100
    },
    #{data :=
          #{<<"introduceItem">> :=
               #{<<"item">> :=
                     #{<<"id">> := ItemId}}}} =
        run(Config, <<"IntroduceItem">>, #{ <<"input">> => ItemInput }),

    [{Subscription, Msg1}, {Subscription, Msg2}] = subscription_receive(2),
    ?assertMatch(
       #{data :=
             #{<<"thingIntroduced">> :=
                   #{<<"__typename">> := <<"Monster">>,
                     <<"id">> := MonsterId,
                     <<"name">> := <<"orc-thing!">>,
                     <<"hp">> := 30}}},
       graphql:handle_subscription_event(#{}, Subscription, SubCtx, Msg1)),
    ?assertMatch(
       #{data :=
             #{<<"thingIntroduced">> :=
                   #{<<"__typename">> := <<"Item">>,
                     <<"id">> := ItemId,
                     <<"name">> := <<"item-thing">>,
                     <<"description">> := <<"item-description">>}}},
       graphql:handle_subscription_event(#{}, Subscription, SubCtx, Msg2)).

multiple_subscriptions(Config) ->
    #{subscription := {Sub1, SubCtx1}} = run(Config, <<"MonsterIntroduced">>, #{}),
    #{subscription := {Sub2, SubCtx2}} = run(Config, <<"MonsterIntroducedInMood">>,
                                             #{<<"mood">> => <<"AGGRESSIVE">>}),
    SubMapping = #{Sub1 => SubCtx1,
                   Sub2 => SubCtx2},
    InputAggressive = #{
      <<"clientMutationId">> => <<"MUTID">>,
      <<"name">> => <<"aggressive-orc">>,
      <<"color">> => <<"#593E1A">>,
      <<"hitpoints">> => 30,
      <<"mood">> => <<"AGGRESSIVE">>
     },
    InputDodgy = InputAggressive#{<<"name">> := <<"dodgy-orc">>,
                                  <<"mood">> := <<"DODGY">>},
    #{ data := #{
          <<"introduceMonster">> := #{
             <<"monster">> := #{
                <<"id">> := AggressiveId,
                <<"name">> := <<"aggressive-orc">>,
                <<"mood">> := <<"AGGRESSIVE">>}
     }}} = run(Config, <<"IntroduceMonster">>, #{ <<"input">> => InputAggressive }),
    #{ data := #{
          <<"introduceMonster">> := #{
             <<"monster">> := #{
                <<"id">> := DodgyId,
                <<"name">> := <<"dodgy-orc">>,
                <<"mood">> := <<"DODGY">>}
     }}} = run(Config, <<"IntroduceMonster">>, #{ <<"input">> => InputDodgy }),
    SubMsgs = lists:keysort(1, subscription_receive(3)),
    [{Sub1, Res11}, {Sub1, Res12}, {Sub2, Res2}] =
        lists:map(
           fun({Sub, Msg}) ->
                   %% It's important that SubCtx and Sub match
                   SubCtx = maps:get(Sub, SubMapping),
                   {Sub, graphql:handle_subscription_event(#{}, Sub, SubCtx, Msg)}
           end, SubMsgs),
    ?assertMatch(#{data := #{<<"monsterIntroduced">> := #{<<"id">> := AggressiveId,
                                                          <<"mood">> := <<"AGGRESSIVE">>}}},
                 Res11),
    ?assertMatch(#{data := #{<<"monsterIntroduced">> := #{<<"id">> := DodgyId,
                                                          <<"mood">> := <<"DODGY">>}}},
                 Res12),
    ?assertMatch(#{data := #{<<"monsterIntroduced">> := #{<<"id">> := AggressiveId,
                                                          <<"mood">> := <<"AGGRESSIVE">>}}},
                 Res2),
    ok.

subscribe_error(Config) ->
    Msg = <<"for test">>,
    ?assertEqual(#{errors =>
                       [
                        #{message => <<"'for test'">>,
                          path => [<<"break">>],
                          extensions => #{code => resolver_error}}
                       ]},
                 run(Config, <<"BrokenSubscription">>, #{<<"reason">> => Msg})).
