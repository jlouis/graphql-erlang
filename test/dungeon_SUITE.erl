-module(dungeon_SUITE).
-include_lib("common_test/include/ct.hrl").

-compile(export_all).

suite() ->
    [{timetrap,
      {seconds, 10}}].

init_per_suite(Config) ->
    application:ensure_all_started(graphql),
    {ok, Doc} = read_doc(Config, "dungeon.graphql"),

    ok = dungeon:inject(Config),
    ok = dungeon:start(),
    ok = graphql:validate_schema(),
    [{document, Doc} | Config].

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
    Dungeon = {dungeon, [],
               [
                 unions,
                 union_errors,
                 scalar_output_coercion,
                 populate,
                 default_query,
                 direct_input,
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
                 introspection
                 ]},

    Errors = {errors, [],
              [unknown_variable,
               missing_fragment,
               quoted_input_error,
               input_coerce_error_exception,
               input_coerce_error,
               invalid_enums
               ]},
    [Dungeon, Errors].

all() ->
    [{group, dungeon},
     {group, errors}].
    
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
    Goblin = base64:encode(<<"monster:1">>),
    #{ data := #{ <<"goblin">> := #{ <<"id">> := Goblin, <<"name">> := <<"goblin">>, <<"hitpoints">> := 10 }}} =
        run(Config, <<"GoblinQuery">>, #{}),
    #{ data := #{ <<"goblin">> := #{ <<"id">> := Goblin, <<"name">> := <<"goblin">>, <<"stats">> := [#{ <<"attack">> := 3 }] }}} =
        run(Config, <<"MinGoblin">>, #{<<"minAttack">> => 0 }),
    #{ data := #{ <<"goblin">> := #{ <<"id">> := Goblin, <<"name">> := <<"goblin">>, <<"stats">> := [] }}} =
        run(Config, <<"MinGoblin">>, #{<<"minAttack">> => 30 }),
    
    ok.

introspection(Config) ->
    case run(Config, <<"introspection.graphql">>, <<"IntrospectionQuery">>, #{}) of
        #{ errors := [] } ->
            ok;
        #{ errors := Errs } ->
            ct:log("Errors: ~p", [Errs]),
            ct:fail(introspection_errors);
        #{ } ->
            ok %% No Errors present, so this is OK
    end.

include_directive(Config) ->
    GoblinID = base64:encode(<<"monster:1">>),
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
             <<"id">> := GoblinID,
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
             <<"id">> := GoblinID,
             <<"name">> := <<"goblin">>,
             <<"hitpoints">> := 10 }}} =
        run(Config, <<"GoblinQueryDirectivesInline">>, #{ <<"fat">> => true }),

    ok.
    
unions(Config) ->
    ct:log("Initial query on the schema"),
    Goblin = base64:encode(<<"monster:1">>),
    Expected1 = #{ data => #{
                     <<"goblin">> => #{
                         <<"id">> => Goblin,
                         <<"name">> => <<"goblin">>,
                         <<"hitpoints">> => 10 }}},
    Expected1 = run(Config, <<"GoblinQuery">>, #{ <<"id">> => Goblin }),
    ct:log("Same query, but on items"),
    Expected2 = #{ data => #{
                     <<"goblin">> => #{
                         <<"id">> => Goblin,
                         <<"name">> => <<"goblin">>,
                         <<"hitpoints">> => 10 }}},
    Expected2 = run(Config, <<"GoblinThingQuery">>, #{ <<"id">> => Goblin }),
    ok.

union_errors(Config) ->
    ct:log("You may not request fields on unions"),
    Q1 = "{ goblin: thing(id: \"bW9uc3Rlcjox\") { id } }",
    th:errors(th:x(Config, Q1)),
    ok.

scalar_output_coercion(Config) ->
    ct:log("Test output coercion"),
    Goblin = base64:encode(<<"monster:1">>),
    #{ data := #{
        <<"goblin">> := #{
            <<"id">> := Goblin,
            <<"name">> := <<"goblin">>,
            <<"color">> := <<"#41924B">>,
            <<"hitpoints">> := 10 }}} =
        run(Config, <<"ScalarOutputCoercion">>, #{ <<"id">> => Goblin }),
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
                <<"mood">> := <<"AGGRESSIVE">>}
        }}} = run(Config, <<"IntroduceMonster">>, #{ <<"input">> => Input}),
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
                     reason := null_value},
                   #{path :=
                         [<<"MonsterStatsThree">>, <<"monster">>, <<"statsVariantThree">>, 0],
                     reason := null_value},
                   #{path :=
                         [<<"MonsterStatsThree">>, <<"monster">>,
                          <<"statsVariantThree">>, 0, <<"attack">>],
                     reason := null_value}]
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
    ID1 = base64:encode(<<"monster:1">>),
    ID2 = base64:encode(<<"monster:2">>),
    ID1000 = base64:encode(<<"monster:1000">>),

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
             reason := not_found}]
     } = run(Config, <<"MultipleMonsters">>, #{ <<"ids">> => [ID1, ID2, ID1000] }),

    #{ data := #{
        <<"monsters">> := [
            #{ <<"id">> := ID1 }, null, #{ <<"id">> := ID2 }, null ]},
       errors := [
                  #{path := [<<"MultipleMonstersExprMissing">>, <<"monsters">>, 1],
                    reason := not_found},
                  #{path := [<<"MultipleMonstersExprMissing">>, <<"monsters">>, 3],
                    reason := not_found}]
     } = run(Config, <<"MultipleMonstersExprMissing">>, #{}),

     Room1 = base64:encode(<<"room:1">>),

     #{ data := #{
        <<"rooms">> := [#{<<"id">> := Room1}]}
      } = run(Config, <<"MultipleRooms">>, #{ <<"ids">> => [Room1]}),

     #{ data := #{
        <<"rooms">> := null
         },
        errors := [
                   #{path := [<<"MultipleRooms">>, <<"rooms">>, 1], reason := null_value},
                   #{path := [<<"MultipleRooms">>, <<"rooms">>, 1], reason := not_found}]
      } = run(Config, <<"MultipleRooms">>, #{ <<"ids">> => [Room1, base64:encode(<<"room:2">>)]}),

     ok.

inline_fragment(Config) ->
    ID = base64:encode(<<"monster:1">>),
    Expected = #{ data => #{
        <<"thing">> => #{
            <<"id">> => ID,
            <<"hitpoints">> => 10 }
    }},
    Expected = run(Config, <<"InlineFragmentTest">>, #{ <<"id">> => ID }),
    ok.

fragment_over_union_interface(Config) ->
    ID = base64:encode(<<"monster:1">>),
    Expected = #{ data => #{
    	<<"monster">> => #{
    		<<"id">> => ID }}},
    Expected = run(Config, <<"FragmentOverUnion1">>, #{ <<"id">> => ID }),
    ct:log("Same as before, but on a named fragment instead"),
    Expected = run(Config, <<"FragmentOverUnion2">>, #{ <<"id">> => ID }),
    ct:log("Same as before, but via an interface type"),
    Expected = run(Config, <<"FragmentOverUnion3">>, #{ <<"id">> => ID }),
    ok.

simple_field_merge(Config) ->
    ID = base64:encode(<<"monster:1">>),
    Expected = #{ data => #{
    	<<"monster">> => #{
    		<<"id">> => ID,
    		<<"hitpoints">> => 10 }}},
    Expected = run(Config, <<"TestFieldMerge">>, #{ <<"id">> => ID }),
    ok.

nested_field_merge(Config) ->
    ID = base64:encode(<<"monster:1">>),
    #{ data := #{
      <<"monster">> := #{
          <<"id">> := ID,
          <<"hitpoints">> := 10,
          <<"stats">> := [#{
              <<"attack">> := 3,
              <<"shellScripting">> := 3,
              <<"yell">> := <<"HELO">> }]
    }}} = Res = run(Config, <<"TestNestedFieldMerge">>, #{ <<"id">> => ID }),
    ok.

unknown_variable(Config) ->
    ID = base64:encode(<<"monster:1">>),

    #{errors :=
          #{key := {unbound_variable,<<"i">>},
            path := [<<"document">>,
                     <<"GoblinQuery">>,
                     <<"monster">>]}} =
        run(Config,
            "unknown_variable.graphql",
            <<"TestFieldMerge">>,
            #{ <<"id">> => ID }),
    ok.

missing_fragment(Config) ->
    ID = base64:encode(<<"monster:1">>),

    #{errors :=
          #{key := {unknown_fragment,<<"GoblinFragment">>},
            path := [<<"document">>,
                     <<"GoblinQuery">>,
                     <<"monster">>]}} =
        run(Config,
            "missing_fragment.graphql",
            <<"GoblinQuery">>,
            #{ <<"id">> => ID }),
    ok.

quoted_input_error(Config) ->
    {error, {_Line, graphql_parser, _}} =
        run(Config, "quoted_input.graphql", <<"IMonster">>, #{}).

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
            path := [<<"document">>,<<"IMonster">>,<<"introduceMonster">>, <<"mood">>]}} =
                run(Config, "invalid_enum_1.graphql", <<"IMonster">>, #{}),
    #{
        errors := #{
            key := {enum_not_found,<<"Mood">>,<<>>},
            message := <<"The value <<>> is not a valid enum value for type Mood">>,
            path := [<<"document">>,<<"IMonster">>,<<"introduceMonster">>, <<"mood">>]}} =
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
