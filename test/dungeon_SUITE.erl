-module(dungeon_SUITE).
-include_lib("common_test/include/ct.hrl").
-compile(export_all).

suite() ->
    [{timetrap,
      {seconds, 10}}].

init_per_suite(Config) ->
    application:ensure_all_started(graphql),
    Config.

end_per_suite(_Config) ->
    application:stop(graphql),
    ok.

init_per_group(dungeon, Config) ->
    {ok, Doc} = read_doc(Config, "dungeon.graphql"),

    ok = dungeon:inject(),
    ok = dungeon:start(),
    ok = graphql:validate_schema(),
    [{document, Doc} | Config];
init_per_group(_Group, Config) ->
    Config.

end_per_group(dungeon, _Config) ->
    dungeon:stop(),
    graphql_schema:reset(),
    ok;
end_per_group(_Group, _Config) ->
    graphql_schema:reset(),
    ok.

groups() ->
    Dungeon = {dungeon, [],
               [ unions,
                 union_errors,
                 scalar_output_coercion,
                 populate,
                 direct_input,
                 inline_fragment,
                 fragment_over_union_interface,
                 simple_field_merge ]},

    [Dungeon].

all() -> [
    {group, dungeon} ].
    
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
    Expected = run(Config, <<"IntroduceMonster">>, #{ <<"input">> => Input }),

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
    HobgoblinExpected =
        run(Config,
            <<"IntroduceMonster">>,
            #{ <<"input">> => HobgoblinInput }),
    
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
    Expected = #{ data => #{
        <<"introduceMonster">> => #{
            <<"clientMutationId">> => null,
            <<"monster">> => #{
                <<"id">> => base64:encode(<<"monster:4">>),
                <<"name">> => <<"Albino Hobgoblin">>,
                <<"color">> => <<"#FFFFFF">>,
                <<"hitpoints">> => 5,
                <<"mood">> => <<"AGGRESSIVE">>}
        }}},
    Input = #{
      <<"name">> => <<"Albino Hobgoblin">>,
      <<"color">> => <<"#ffffff">>,
      <<"hitpoints">> => 5,
      <<"mood">> => <<"AGGRESSIVE">>
    },
    Expected = run(Config, <<"IntroduceMonster">>, #{ <<"input">> => Input}),
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
