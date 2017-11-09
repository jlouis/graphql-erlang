-module(validation_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([suite/0, all/0, groups/0, init_per_suite/1, end_per_suite/1,
         init_per_group/2, end_per_group/2,
         init_per_testcase/2, end_per_testcase/2]).

-export([v_5_1_1_1/1,
         v_5_1_2_1/1,
         v_5_2_1/1,
         v_5_2_3/1,
         v_5_3_1/1,
         v_5_3_2/1,
         v_5_4_1_1/1,
         v_5_4_1_2/1,
         v_5_4_1_3/1,
         v_5_4_2_1/1,
         v_5_4_2_2/1,
         v_5_4_2_3_1/1,
         v_5_4_2_3_2/1,
         v_5_4_2_3_3/1,
         v_5_4_2_3_4/1,
         v_5_5_1/1,
         v_5_6_1/1,
         v_5_6_2/1,
         v_5_6_3/1,
         v_5_7_1/1,
         v_5_7_3/1
        ]).

suite() ->
    [{timetrap, {seconds, 30}}].

init_per_group(_Group, Config) ->
    Config.

end_per_group(_Group, _Config) ->
    ok.

init_per_suite(Config) ->
    application:ensure_all_started(graphql),
    ok = schema_pet:inject(),
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
    Validation =
        {validation, [shuffle, parallel],
         [ v_5_1_1_1,
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
           v_5_4_2_3_1,
           v_5_4_2_3_2,
           v_5_4_2_3_3,
           v_5_4_2_3_4,
           v_5_5_1,
           v_5_6_1,
           v_5_6_2,
           v_5_6_3,
           v_5_7_1,
           v_5_7_3
         ]},
    [Validation].

all() -> [{group, validation}].

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

    true = th:v(
      "{ dog { ...F } } fragment F on Dog { isHouseTrained(atOtherHomes: true) @include(if: true) }"),

    false = th:v(
      "{ dog { ...F } } fragment F on Dog { doesKnowCommand(command: CLEAN_UP_HOUSE) }"),

    true = th:v(
      "{ arguments { ...F } } fragment F on Arguments { multipleReqs(x: 1, y: 2) }"),

    true = th:v(
      "{ arguments { ...F } } fragment F on Arguments { multipleReqs(y:1, x: 2) }"),

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

v_5_4_2_3_2(_Config) ->
    true = th:v(
             "{ dog { ...petFrag } } fragment petFrag on Pet { name }"),
    true = th:v(
      "{ dog { ...unionWithObjectFragment } } "
      "fragment catOrDogNameFragment on CatOrDog { ... on Cat { meowVolume } } "
      "fragment unionWithObjectFragment on Dog { ...catOrDogNameFragment }"),
    ok.

v_5_4_2_3_3(_Config) ->
    true = th:v(
             "{ dog { ...catOrDogFragment } } "
             "fragment petFragment on Pet { name ... on Dog { barkVolume}} "
             "fragment catOrDogFragment on CatOrDog { ... on Cat { meowVolume }}"),
    false = th:v(
              "{ human { ...humanOrAlienFragment }} "
              "fragment sentientFragment on Sentient {... on Dog {barkVolume}} "
              "fragment humanOrAlienFragment on HumanOrAlien {... on Cat {meowVolume}}"),
    ok.

v_5_4_2_3_4(_Config) ->
    true = th:v(
       "{ dog { ...unionWithInterface } } "
       "fragment unionWithInterface on Pet {...dogOrHumanFragment} "
       "fragment dogOrHumanFragment on DogOrHuman {... on Dog {barkVolume}}"),
    false = th:v(
       "{ human { ...nonIntersectingInterfaces } } "
       "fragment nonIntersectingInterfaces on Pet {...sentientFragment} "
       "fragment sentientFragment on Sentient {name}"),
    ok.

v_5_5_1(_Config) ->
    false = th:v("{ field(arg: { field: true, field: false })}"),
    ok.

v_5_6_1(_Config) ->
    false = th:v("query Q { dog { name @invalidDirective }}"),
    true  = th:v("query Q { dog { name @include(if: true) }}"),
    ok.

v_5_6_2(_Config) ->
    false = th:v("query Q @skip(if: $foo) { dog { name }}"),
    ok.

v_5_6_3(_Config) ->
    false = th:v("query Q { dog { name @skip(if: true) @skip(if: false) }}"),
    ok.

v_5_7_1(_Config) ->
    false = th:v("query houseTrainedQuery("
                 " $atOtherHomes: Boolean,"
                 " $atOtherHomes: Boolean) { "
                 "dog { isHouseTrained(atOtherHomes: $atOtherHomes) } }"),
    Q = "query A($atOtherHomes: Boolean) {...HouseTrainedFragment } "
        "query B($atOtherHomes: Boolean) {...HouseTrainedFragment } "
        " fragment HouseTrainedFragment { "
        "    dog {isHouseTrained(atOtherHomes: $atOtherHomes)}}",
    
    %% Since we have not yet solved fragment expansion correctly, this
    %% query doesn't work. But once fragment expansion is solved, then
    %% we should expect a true return here over a false one.
    false = th:v(Q),
    ok.

v_5_7_3(_Config) ->
    false = th:v("query Q($var : Pet) { dog { name } }"),
    false = th:v("query Q($var : Dog) { dog { name } }"),
    true  = th:v(
        "query Q($command : DogCommand!) {"
        " dog { doesKnowCommand(dogCommand : $command) } }"),
    ok.
