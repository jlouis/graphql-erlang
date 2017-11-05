-module(dungeon_SUITE).
-include_lib("common_test/include/ct.hrl").

-compile(export_all).

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
         [ unions
         , union_errors
         , scalar_output_coercion
         , populate
         , default_query
         , direct_input
         , fixed_input
         , nested_input_object
         , inline_fragment
         , fragment_over_union_interface
         , integer_in_float_context
         , scalar_as_expression_coerce
         , non_null_field
         , complex_modifiers
         , simple_field_merge
         , nested_field_merge
         , multiple_monsters_and_rooms
         , include_directive
         , introspection
         , get_operation
         , coercion_int_float
         , replace_enum_representation
         , auxiliary_data
         , find_monster
         , find_monster_singleton
         ]},
    Errors =
        {errors, [],
         [ unknown_variable
         , missing_fragment
         , quoted_input_error
         , input_coerce_error_exception
         , input_coerce_error
         , invalid_enums
         , invalid_enum_result
         , invalid_type_resolution
         , duplicate_validation
         , invalid_list_resolver
         ]},
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

default_query(Config) ->
    ID = ?config(known_goblin_id_1, Config),
    #{ data := #{ <<"goblin">> := #{ <<"id">> := ID, <<"name">> := <<"goblin">>, <<"hitpoints">> := 10 }}} =
        run(Config, <<"GoblinQuery">>, #{}),
    #{ data := #{ <<"goblin">> := #{ <<"id">> := ID, <<"name">> := <<"goblin">>, <<"stats">> := [#{ <<"attack">> := 3 }] }}} =
        run(Config, <<"MinGoblin">>, #{<<"minAttack">> => 0 }),
    #{ data := #{ <<"goblin">> := #{ <<"id">> := ID, <<"name">> := <<"goblin">>, <<"stats">> := [] }}} =
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

invalid_list_resolver(Config) ->
    GoblinId = ?config(known_goblin_id_1, Config),
    Q1 = "query Q { monster(id: \"" ++ binary_to_list(GoblinId) ++ "\") { errorListResolution }} ",
    Expected =
        #{data => #{<<"monster">> => #{<<"errorListResolution">> => null}},
          errors =>
              [#{key => list_resolution,
                 message =>
                     <<"Internal Server error: A list is being incorrectly resolved">>,
                 path =>
                     [<<"Q">>,<<"monster">>,<<"errorListResolution">>]}]},
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
    Expected = #{ data => #{<<"monster">> => #{ <<"name">> => <<"goblin">> }}},
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
             <<"name">> := <<"goblin">>,
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
             <<"name">> := <<"goblin">>,
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
                     <<"name">> => <<"goblin">>,
                     <<"hitpoints">> => 10 }}},
    Expected1 = run(Config, <<"GoblinQuery">>, #{<<"id">> => OpaqueId}),
    ct:log("Same query, but on items"),
    Expected2 = #{ data => #{
                     <<"goblin">> => #{
                         <<"id">> => OpaqueId,
                         <<"name">> => <<"goblin">>,
                         <<"hitpoints">> => 10 }}},
    Expected2 = run(Config, <<"GoblinThingQuery">>, #{ <<"id">> => OpaqueId }),
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
            <<"name">> := <<"goblin">>,
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
    ok.

direct_input(Config) ->
    Input = #{
      <<"name">> => <<"Albino Hobgoblin">>,
      <<"color">> => <<"#ffffff">>,
      <<"hitpoints">> => 5,
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
                         [<<"MonsterStatsThree">>, <<"monster">>, <<"statsVariantThree">>],
                     key := null_value,
                     message := _} ,
                   #{path :=
                         [<<"MonsterStatsThree">>, <<"monster">>, <<"statsVariantThree">>, 0],
                     key := null_value,
                     message := _},
                   #{path :=
                         [<<"MonsterStatsThree">>, <<"monster">>,
                          <<"statsVariantThree">>, 0, <<"attack">>],
                     key := null_value,
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
             run(Config, <<"IntroduceMonsterFatExpr">>, #{}),
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
       errors := [
           #{path := [<<"MultipleMonsters">>, <<"monsters">>, 2],
             message := <<"not_found">> }]
     } = run(Config, <<"MultipleMonsters">>, #{ <<"ids">> => [ID1, ID2, IDMissing] }),

    #{ data := #{
        <<"monsters">> := [
            #{ <<"id">> := ID1 }, null, #{ <<"id">> := ID2 }, null ]},
       errors := [
                  #{path := [<<"MultipleMonstersExprMissing">>, <<"monsters">>, 1],
                    message := <<"not_found">>},
                  #{path := [<<"MultipleMonstersExprMissing">>, <<"monsters">>, 3],
                    message := <<"not_found">>}]
     } = run(Config, <<"MultipleMonstersExprMissing">>, #{}),

    Room1 = ?config(known_room_id, Config),
    NonExistentRoom = ?config(known_non_existent_room_id, Config),
    #{ data := #{
         <<"rooms">> := [#{<<"id">> := Room1}]}
     } = run(Config, <<"MultipleRooms">>, #{ <<"ids">> => [Room1]}),
    % look for an existing room and a non existing room
    #{ data := #{ <<"rooms">> := null },
       errors :=
           [#{path := [<<"MultipleRooms">>, <<"rooms">>, 1],
              key := null_value },
            #{path := [<<"MultipleRooms">>, <<"rooms">>, 1],
              key := not_found } ]
     } = run(Config, <<"MultipleRooms">>,
             #{ <<"ids">> => [Room1, NonExistentRoom]}),
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
        #{ data =>
               #{<<"findMonsters">> =>
                     [#{<<"name">> => <<"goblin">>},
                      #{<<"name">> => <<"Auxiliary Undead">>},
                      #{<<"name">> => <<"goblin">>},
                      #{<<"name">> => <<"goblin">>},
                      #{<<"name">> => <<"hobgoblin">>},
                      #{<<"name">> => <<"Yellow Slime">>},
                      #{<<"name">> => <<"goblin">>},
                      #{<<"name">> => <<"goblin">>}]}},  
    Expected1 = run(Config, <<"FindQuery">>, #{}),
    Expected1 = run(Config, <<"FindQueryParam">>,
                    #{ <<"m">> => [<<"DODGY">>]}),
    ok.

find_monster_singleton(Config) ->
    Expected1 =
        #{ data =>
               #{<<"findMonsters">> =>
                     [#{<<"name">> => <<"goblin">>},
                      #{<<"name">> => <<"Auxiliary Undead">>},
                      #{<<"name">> => <<"goblin">>},
                      #{<<"name">> => <<"goblin">>},
                      #{<<"name">> => <<"hobgoblin">>},
                      #{<<"name">> => <<"Yellow Slime">>},
                      #{<<"name">> => <<"goblin">>},
                      #{<<"name">> => <<"goblin">>}]}},  
    Expected1 = run(Config, <<"FindQuerySingleton">>, #{}),
    Expected1 = run(Config, <<"FindQueryParamSingleton">>,
                    #{ <<"m">> => <<"DODGY">>}),
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
                                   , <<"name">> => <<"Auxiliary Undead">>}
               }
     },
    Expected = run(Config, <<"TestAuxiliaryData">>, #{<<"id">> => OpaqueId}).

unknown_variable(Config) ->
    ID = ?config(known_goblin_id_1, Config),
    #{ errors :=
          #{key := {unbound_variable,<<"i">>},
            path := [<<"document">>,
                     <<"GoblinQuery">>,
                     <<"monster">>,
                     <<"id">>]}} =
        run(Config,
            "unknown_variable.graphql",
            <<"TestFieldMerge">>,
            #{ <<"id">> => ID }),
    ok.

missing_fragment(Config) ->
    ID = ?config(known_goblin_id_1, Config),
    #{ errors :=
          #{key := unknown_fragment,
            path := [<<"document">>,
                     <<"GoblinQuery">>,
                     <<"monster">>,
                     <<"GoblinFragment">>] }} =
        run(Config,
            "missing_fragment.graphql",
            <<"GoblinQuery">>,
            #{ <<"id">> => ID }),
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
           [#{ path := [<<"LookupThing">>, <<"thing">>],
               key := {type_resolver_error, kraken},
               message := <<"kraken">> }
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
    #{
        errors := #{
            key := {enum_not_found,<<"Mood">>,<<"AGGRESSIF">>},
            message := <<"The value <<\"AGGRESSIF\">> is not a valid enum value for type Mood">>,
            path := [<<"IntroduceMonster">>,<<"input">>,<<"mood">>]}} =
                run(Config, <<"IntroduceMonster">>, #{ <<"input">> => Input }),
    #{
        errors := #{
            key := {enum_not_found,<<"Mood">>,<<>>},
            message := <<"The value <<>> is not a valid enum value for type Mood">>,
            path := [<<"IntroduceMonster">>,<<"input">>,<<"mood">>]}} =
                run(Config, <<"IntroduceMonster">>, #{ <<"input">> => Input#{ <<"mood">> => <<"">> }}),
    #{
        errors := #{
            key := {enum_not_found,<<"Mood">>,<<>>},
            message := <<"The value <<>> is not a valid enum value for type Mood">>,
            path := [<<"IntroduceMonster">>,<<"input">>,<<"mood">>]}} =
                run(Config, <<"IntroduceMonster">>, #{ <<"input">> => Input#{ <<"mood">> => <<>> }}),
    #{
        errors := #{
            key := {enum_not_found,<<"Mood">>,<<"AGGRESSIF">>},
            message := <<"The value <<\"AGGRESSIF\">> is not a valid enum value for type Mood">>,
            path := [<<"document">>,<<"IMonster">>,<<"introduceMonster">>, <<"input">>, <<"mood">>]}} =
                run(Config, "invalid_enum_1.graphql", <<"IMonster">>, #{}),
    #{
        errors := #{
            key := {enum_not_found,<<"Mood">>,<<>>},
            message := <<"The value <<>> is not a valid enum value for type Mood">>,
            path := [<<"document">>,<<"IMonster">>,<<"introduceMonster">>, <<"input">>, <<"mood">>]}} =
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
    #{errors := #{key := {input_coercion,<<"Color">>,_,_},
                  path := [<<"IntroduceMonster">>,
                           <<"input">>,
                           <<"color">>]}} =
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
    #{errors := #{key := {input_coerce_abort, _},
                  path := [<<"IntroduceMonster">>,
                           <<"input">>,
                           <<"color">>]}} =
        run(Config, <<"IntroduceMonster">>, #{ <<"input">> => Input }),
    ok.
