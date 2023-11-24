-module(dungeon).
-include_lib("common_test/include/ct.hrl").
-include_lib("graphql/include/graphql.hrl").
-include_lib("stdlib/include/qlc.hrl").

-include("dungeon.hrl").

-export([dirty_load/1, load/1, load_txn/1, wrap/1, unwrap/1, insert/1]).
-export([inject/1, start/0, stop/0]).

-export([insert/2, reserve_number/1, create/1, create/2, batch_create/1, opaque_id/1, next_id/1,
        subscribe/1, unsubscribe/1]).

start() ->
    application:load(mnesia),
    ok = mnesia:create_schema([node()]),
    application:ensure_all_started(mnesia),
    {atomic, ok} = mnesia:create_table(
                     sequence,
                     [{attributes, record_info(fields, sequence)}]),
    {atomic, ok} = mnesia:create_table(
                     monster,
                     [{attributes, record_info(fields, monster)}]),
    {atomic, ok} = mnesia:create_table(
                     item,
                     [{attributes, record_info(fields, item)}]),
    {atomic, ok} = mnesia:create_table(
                     room,
                     [{attributes, record_info(fields, room)}]),
    populate(),
    ok.

stop() ->
    application:stop(mnesia).

set_id(#monster{} = M, ID) -> M#monster { id = ID };
set_id(#room{} = R, ID) -> R#room { id = ID };
set_id(#item{} = I, ID) -> I#item { id = ID }.

insert(Input) ->
    Fun = fun() ->
                  Tab = element(1, Input),
                  Id = mnesia:dirty_update_counter({sequence, Tab}, 1),
                  Record = set_id(Input, Id),
                  ok = mnesia:write(Record),
                  Record
          end,
    mnesia:transaction(Fun).

insert(Input, {Type, Id}) when element(1, Input) =:= Type ->
    Fun = fun() ->
                  Record = set_id(Input, Id),
                  ok = mnesia:write(Record),
                  Record
          end,
    mnesia:transaction(Fun).

next_id(Tab) ->
    Fun = fun() ->
                  [{sequence, Tab, Id}] = mnesia:dirty_read(sequence, Tab),
                  {ok, Wrapped} = wrap({Tab, Id + 1}),
                  Wrapped
          end,
    {atomic, Id} = mnesia:transaction(Fun),
    Id.

reserve_number(Input) ->
    Tab = element(1, Input),
    Fun =
        fun() ->
            mnesia:dirty_update_counter({sequence, Tab}, 1)
        end,
    {atomic, Id} = mnesia:transaction(Fun),
    {Tab, Id}.


create(monster, Opts) ->
    Name = proplists:get_value(name, Opts, <<"goblin">>),
    Mood = proplists:get_value(mood, Opts, 'DODGY'),
    true = is_atom(Mood),
    Color = proplists:get_value(color, Opts, #{ r => 65, g => 146, b => 75}),
    HitPoints = proplists:get_value(hitpoints, Opts, 10),
    #monster { name = Name
             , color = Color
             , hitpoints = HitPoints
             , mood = Mood
             , stats = [#stats{}] };
create(room, Opts) ->
    Desc = proplists:get_value(desc, Opts, <<"hallway">>),
    #room { description = Desc
          }.

subscribe(Table) ->
    {ok, _} = mnesia:subscribe({table, Table, simple}).

unsubscribe(Table) ->
    {ok, _} = mnesia:unsubscribe({table, Table, simple}).

create(Type) -> create(Type, []).

batch_create(Spec) when is_map(Spec) ->
    maps:fold(
      fun(K, V, Acc) when is_list(V) ->
              Acc#{K => [do_create(Item) || Item <- V]}
      end, #{}, Spec);
batch_create(Spec) when is_list(Spec) ->
    [do_create(Item) || Item <- Spec].


do_create({Data, reserve}) ->
    Id = reserve_number(Data),
    {Id, reserved};
do_create({Data, insert}) ->
    Id = reserve_number(Data),
    {atomic, Entry} = insert(Data, Id),
    {Id, Entry}.

opaque_id({Type, Id}) ->
    BinId = integer_to_binary(Id),
    BinType = atom_to_binary(Type, utf8),
    base64:encode(<<BinType/binary, ":", BinId/binary>>).

%update(Record) ->
%    mnesia:transaction(
%      fun() ->
%        ok = mnesia:write(Record),
%        Record
%      end).

load({room, ID}) ->
    load_txn(qlc:q([R || R <- mnesia:table(room), R#room.id == ID]));
load({monster, ID}) ->
    load_txn(qlc:q([M || M <- mnesia:table(monster), M#monster.id == ID])).

dirty_load(OID) ->
    case mnesia:dirty_read(OID) of
        [] -> {error, not_found};
        [Obj] -> {ok, Obj}
    end.

load_txn(Q) ->
    F = fun() -> qlc:e(Q) end,
    case mnesia:transaction(F) of
        {atomic, [X]} -> {ok, X};
        {atomic, L} when is_list(L) -> {ok, L}
    end.

populate() ->
    Fun =
        fun() ->
                mnesia:write(#sequence { id = monster, count = 0 }),
                mnesia:write(#sequence { id = room, count = 0 })
        end,
    {atomic, _} = mnesia:transaction(Fun),
    Spec = #{
       rooms =>
          [ {create(room, [{desc, <<"This is the dungeon entrance">>}]), insert }
          , {create(room, [{desc, <<"Reserved room">>}]), reserve } ]
     , monsters =>
          [ {create(monster, [ {name, <<"goblin">>}
                             , {color, #{ r => 65, g => 146, b => 75}}
                             , {hitpoints, 10}
                             , {mood, 'DODGY'}
                             ]), insert}
          , {create(monster, [ {name, <<"orc">>}
                             , {color, #{ r => 65, g => 146, b => 75}}
                             , {hitpoints, 30}
                             , {mood, 'AGGRESSIVE'}
                             ]), insert}
          , {create(monster, [ {name, <<"reserved goblin">>}
                             , {color, #{ r => 65, g => 146, b => 75}}
                             , {hitpoints, 5}
                             , {mood, 'DODGY'}
                             ]), reserve}
          ]
     },
    batch_create(Spec),
    {atomic, _} = mnesia:transaction(fun() ->
        mnesia:write(#sequence { id = monster, count = 1000 }),
        mnesia:write(#sequence { id = room, count = 1000 })
    end),
   ok.

mapping_rules() ->
    #{
       scalars => #{ default => dungeon_scalar },
       enums => #{ 'Mood' => dungeon_enum, default => graphql_enum_coerce },
       interfaces => #{ default => dungeon_type },
       unions => #{ default => dungeon_type },
       objects => #{
         'Monster' => dungeon_monster,
         'Item' => dungeon_item,
         'Room' => dungeon_room,
         'Stats' => dungeon_stats,
         'Dice' => dungeon_dice,
         'QueryRoot' => dungeon_query,
         'MutationRoot' => dungeon_mutation,
         'SubscriptionRoot' => dungeon_subscription,
         default => dungeon_object }
     }.

inject(Config) ->
    DataDir = ?config(data_dir, Config),
    SchemaFile = filename:join([DataDir, "dungeon_schema.graphql"]),
    {ok, SchemaData} = file:read_file(SchemaFile),
    Mapping = mapping_rules(),
    ok = graphql:load_schema(Mapping, SchemaData),
    ok.

unwrap(X) ->
    try
        [Type, ID] = binary:split(base64:decode(X), <<":">>),
        I = binary_to_integer(ID),
           case Type of
               <<"item">> -> {item, I};
               <<"monster">> -> {monster, I};
               <<"room">> -> {room, I};
               %% Invalid type
               <<"kraken">> -> {kraken, I};
               _ -> undefined
           end
    catch
        _:_ -> undefined
    end.

wrap({Type, ID}) ->
    TBin = atom_to_binary(Type, utf8),
    IDBin = integer_to_binary(ID),
    {ok, base64:encode(<<TBin/binary, ":", IDBin/binary>>)}.
