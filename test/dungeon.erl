-module(dungeon).
-include_lib("common_test/include/ct.hrl").
-include_lib("graphql/include/graphql.hrl").
-include_lib("stdlib/include/qlc.hrl").

-include("dungeon.hrl").

-export([dirty_load/1, load/1, wrap/1, unwrap/1, insert/1]).
-export([inject/1, start/0, stop/0]).

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
set_id(#room{} = R, ID) -> R#room { id = ID }.

insert(Input) ->
    Fun = fun() ->
                  Tab = element(1, Input),
                  ID = mnesia:dirty_update_counter({sequence, Tab}, 1),
                  Record = set_id(Input, ID),
                  ok = mnesia:write(Record),
                  Record
          end,
    mnesia:transaction(Fun).

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
        {atomic, [X]} -> {ok, X}
    end.

populate() ->
    Fun =
        fun() ->
                mnesia:write(#sequence { id = monster, count = 0 }),
                mnesia:write(#sequence { id = room, count = 0 })
        end,
    {atomic, _} = mnesia:transaction(Fun),
    {atomic, _} = insert(#monster {
        name = <<"goblin">>,
        color = #{ r => 65, g => 146, b => 75},
        hitpoints = 10,
        mood = <<"DODGY">>,
        stats = [#stats{}] }),
   ok.

mapping_rules() ->
    #{
       scalars => #{ default => dungeon_scalar },
       interfaces => #{ default => dungeon_type },
       unions => #{ default => dungeon_type },
       objects => #{
         'Monster' => dungeon_monster,
         'Item' => dungeon_item,
         'Room' => dungeon_room,
         'Stats' => dungeon_stats,

         'QueryRoot' => dungeon_query,
         'MutationRoot' => dungeon_mutation,
         default => dungeon_object }
     }.
    
inject(Config) ->
    DataDir = ?config(data_dir, Config),
    SchemaFile = filename:join([DataDir, "dungeon_schema.graphql"]),
    {ok, SchemaData} = file:read_file(SchemaFile),
    Mapping = mapping_rules(),
    ok = graphql:load_schema(Mapping, SchemaData),
    
    Schema = {root, #{
                query => 'QueryRoot',
                mutation => 'MutationRoot',
                interfaces => ['Node'] }},
    ok = graphql:insert_schema_definition(Schema),
    ok.

unwrap(X) ->
    [Type, ID] = binary:split(base64:decode(X), <<":">>),
    try binary_to_integer(ID) of
        I ->
           case Type of
               <<"item">> -> {item, I};
               <<"monster">> -> {monster, I};
               <<"room">> -> {room, I};
               _ -> undefined
           end
    catch
        _:_ -> undefined
    end.

wrap({Type, ID}) ->
    TBin = atom_to_binary(Type, utf8),
    IDBin = integer_to_binary(ID),
    {ok, base64:encode(<<TBin/binary, ":", IDBin/binary>>)}.

