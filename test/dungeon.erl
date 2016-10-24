-module(dungeon).
-include_lib("graphql/include/graphql.hrl").
-include_lib("stdlib/include/qlc.hrl").

-export([inject/0, start/0, stop/0]).

-record(sequence, {id, count}).
-record(stats, {
          shell_scripting = 3,
          attack = 3,
          yell = <<"HELO">>
}).

-record(monster, {
          id,
          name,
          color,
          inventory = ordsets:new(),
          stats = #stats{},
          hitpoints,
          plush_factor = 0.0,
          properties = [],
          mood}).
-record(item, {
	id,
	name,
	description,
	weight,
	contents }).
-record(room, {
	id,
	description,
	contents = ordsets:new()
}).

start() ->
    application:load(mnesia),
    ok = mnesia:create_schema([node()]),
    application:ensure_all_started(mnesia),
    {atomic, ok} = mnesia:create_table(sequence, [{attributes, record_info(fields, sequence)}]),
    {atomic, ok} = mnesia:create_table(monster,[{attributes, record_info(fields, monster)}]),
    {atomic, ok} = mnesia:create_table(item, [{attributes, record_info(fields, item)}]),
    {atomic, ok} = mnesia:create_table(room, [{attributes, record_info(fields, room)}]),
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

update(Record) ->
    mnesia:transaction(
      fun() ->
        ok = mnesia:write(Record),
        Record
      end).
 
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
    {atomic, _} = insert(#monster { name = <<"goblin">>, color = {65, 146, 75}, hitpoints = 10, mood = <<"DODGY">> }),
   ok.

inject_error() ->
    Error = {object, #{
        id => 'Error',
        description => "Error Objects",
        fields => #{
        	message => #{
        		type => 'string',
        		description => "The error message"
        	}
       }}},
    ok = graphql:insert_schema_definition(Error),
    ok.

inject_color() ->
    ColorType = {scalar, #{
    	id => 'ColorType',
    	description => "How to represent colors in the output",
    	input_coerce => fun(Bin) ->
    	    V = list_to_binary(
    	        string:to_lower(
    	            binary_to_list(Bin))),
    	    {ok, V}
    	end,
    	output_coerce => fun(X) -> {ok, X} end
    }},
    ok = graphql:insert_schema_definition(ColorType),

    Color = {scalar, #{
    	id => 'Color',
    	description => "Represents a color in the system",
    	input_coerce => fun (<<"#", R1, R2, G1, G2, B1, B2>> = X) ->
                                ct:pal("Input coercer called~n"),
                                Red = binary_to_integer(<<R1, R2>>, 16),
                                Green = binary_to_integer(<<G1, G2>>, 16),
                                Blue = binary_to_integer(<<B1, B2>>, 16),
                                {ok, {Red, Green, Blue}};
                            (<<"#">>) ->
                                exit(argh);
                            (X) ->
                                {error, {invalid_color, X}}
                        end,
    	output_coerce => fun ({R,G,B}) ->
                                 ct:pal("Output coercer called~n"),
                                 R1 = integer_to_binary(R, 16),
                                 G1 = integer_to_binary(G, 16),
                                 B1 = integer_to_binary(B, 16),
                                 {ok, <<"#", R1/binary, G1/binary, B1/binary>>}
                         end
    }},
    ok = graphql:insert_schema_definition(Color),
    ok.

ok(Data) -> ok(Data, []).

ok([], Acc) -> {ok, lists:reverse(Acc)};
ok([{ok, R} | Next], Acc) -> ok(Next, [R | Acc]).

inject_monster() ->
    StatsInput = {input_object, #{
               id => 'StatsInput',
               description => "The stats of the monster in question",
               fields => #{
                 attack => #{
                   type => 'int!',
                   description => "The attack value of the monster"
                  },
                 yell => #{
                   type => 'string',
                   description => "The yell of the monster",
                   default => <<"YAAAAAAAAAH MAN!</rastafari>">>
                  },
                 shellScripting => #{
                   type => 'int',
                   description => "The sh(1) scripting capability of the entity",
                   default => 3
                  }
                }
              }},
    ok = graphql:insert_schema_definition(StatsInput),

    Stats = {object, #{
               id => 'Stats',
               description => "The stats of the monster in question",
               fields => #{
                 attack => #{
                   type => 'int!',
                   description => "The attack value of the monster",
                   resolve => fun(_, #stats { attack = A }, _) -> {ok, A} end
                  },
                 yell => #{
                   type => 'string',
                   description => "The yell of the monster",
                   resolve => fun(_, #stats { yell = Y }, _) -> {ok, Y} end
                  },
                 shellScripting => #{
                   type => 'int',
                   description => "The sh(1) scripting capability of the entity",
                   resolve => fun(_, #stats { shell_scripting = SS }, _) -> {ok, SS} end
                  }
                }
              }},
    ok = graphql:insert_schema_definition(Stats),
                 
    Mood = {enum, #{
        id => 'Mood',
        description => "The mood of a monster",
        repr => binary,
        values => #{
            'TRANQUIL' => #{value => 0, description => "The monster is tranquil and will not attack, unless attacked"},
            'DODGY' => #{value => 1, description => "The monter might or might not attack"},
            'AGGRESSIVE' => #{value => 2, description => "The monster is looking for a fight"}
        }
    }},
    ok = graphql:insert_schema_definition(Mood),
    Property = {enum, #{
        id => 'Property',
        description => "Monster properties",
        repr => binary,
        values => #{
        	   'BEAST' => #{ value => 0, description => "The monster is a BEAST!" },
        	   'MECH' => #{ value => 1, description => "The monster is a mech" },
        	   'DRAGON' => #{ value => 2, description => "The monster is a dragon" },
        	   'PIRATE' => #{ value => 3, description => "The monstARRRRRR is a PirARRRRte" },
        	   'MURLOC' => #{ value => 4, description => "MRRGLGLGLGLGLGL" },
        	   'TOTEM' => #{ value => 5, description => "The monster is a totem" },
        	   'DEMON' => #{ value => 6, description => "The monster is a demon" }
        }}},
    ok = graphql:insert_schema_definition(Property),
    Monster = {object, #{
    	id => 'Monster',
    	description => "Represents a monster in the dungeon",
    	interfaces => ['Node'],
    	fields => #{
          id => #{
            type => 'id!',
            resolve => fun(Ctx, #monster{ id = ID }, _) -> wrap({monster, ID}) end,
            description => "Unique identifiers of monsters" },
          name => #{
            type => 'string!',
            resolve => fun(_, #monster{ name = N}, _) -> {ok, N} end,
            description => "The name of the monster" },
          color => #{
            type => 'Color!',
            resolve => fun(_, #monster{ color = {R,G,B} = C }, #{ <<"colorType">> := CType }) ->
                                  case CType of
                                      <<"rgb">> -> {ok, C};
                                      <<"gray">> ->
                                          V = 0.30*R + 0.59*G + 0.11*B,
                                          {ok, {V, V, V}}
                                  end
                       end,
            description => "The color of the monster",
            args => #{
              colorType => #{
                type => 'ColorType',
                default => <<"rgb">>,
                description => "How to process the color of the monster" }}},
          hitpoints => #{
            type => 'int!',
            resolve => fun(_, #monster { hitpoints = HP}, _) -> {ok, HP} end,
            description => "How many hitpoints the monster has" },
          inventory => #{
            type => ['Thing'],
            resolve =>
                fun(_, #monster { inventory = INV }, _) ->
                        ok([load(OID) || OID <- INV])
                end,
            description => "The monsters inventory" },
          hp => #{
            type => 'int!',
            resolve => fun(_, #monster { hitpoints = HP}, _) -> {ok, HP} end,
            description => "How many hitpoints the monster has",
            deprecation => "Use `hitpoints` instead" },
          mood => #{
            type => 'Mood',
            resolve => fun(_, #monster { mood = M}, _) -> {ok, M} end,
            description => "The mood of the monster"
           },
          plushFactor => #{
            type => 'float!',
            resolve => fun(_, #monster { plush_factor = F }, _) -> {ok, F} end,
            description => "How much of a plush animal the monster is (cuteness)" },
          stats => #{
            type => 'Stats',
            resolve => fun(_, #monster { stats = S}, _) -> {ok, S} end,
            description => "The stats of the monster"
           },
          properties => #{
            type => ['Property'],
            resolve => fun(_, #monster { properties = Props }, _) -> {ok, Props} end,
            description => "Monster properies"
          }
    	}}},
    ok = graphql:insert_schema_definition(Monster),

    ok = graphql_relay:input('IntroduceMonster', #{
                               name => #{
                                 type => 'string!',
                                 description => "The defining name of the monster" },
                               color => #{
                                 type => 'Color!',
                                 description => "The color of the monster" },
                               hitpoints => #{
                                 type => 'int',
                                 default => 15,
                                 description => "The number of hitpoints of a monster" },
                               plushFactor => #{
                                  type => 'float',
                                  default => 0.01,
                                  description => "The plush-factor of the monster" },
                               mood => #{
                                 type => 'Mood',
                                 default => <<"DODGY">>,
                                 description => "The mood of the monster" },
                               stats => #{
                                 type => 'StatsInput',
                                 description => "The monster stats"
                                }
                              },
                             #{
                                monster => #{
                                  type => 'Monster',
                                  description => "The introduced monster" }
            }).
 
inject_room() ->
    	Room = {object, #{
    		id => 'Room',
    		description => "Rooms in the dungeon",
    		interfaces => ['Node'],
    		fields => #{
    			id => #{
    				type => 'id!',
    				resolve => fun(_, #room { id = ID }, _) -> wrap({room, ID}) end,
    				description => "The unique ID of a room" },
    			description => #{
    				type => 'string!',
    				resolve => fun(_, #room { description = D }, _) -> {ok, D} end,
    				description => "What the room looks like" },
    			contents => #{
    				type => ['Thing'],
    				resolve =>
    				  fun(_, #room { contents = Contents }, _) ->
    				    ok([load(OID) || OID <- Contents])
    				  end,
    				description => "Things in the room" }
    	}}},
    	ok = graphql:insert_schema_definition(Room),
    ok = graphql_relay:input('IntroduceRoom',
        #{
    	    description => #{
    		type => 'string!',
    		description => "Description of what the new room looks like" }
        },
        #{
             room => #{
               type => 'Room',
               description => "The introduced room" }
        }),

    ok = graphql_relay:input('SpawnMinion',
	#{
    	    monsterId => #{
    		type => 'id!',
    		description => "The monster to add to the room" },
    	    roomId => #{
    	    	type => 'id!',
    	    	description => "The room in which the monster should be added" }
	},
	#{
             room => #{
               type => 'Room',
               description => "The room in which the monster was added" },
             monster => #{
               type => 'Monster',
               description => "The monster which was added to the room" }
	}),
    ok.
    
mutations() ->
    {object, #{
       id => 'MutationRoot',
       description => "Top level mutations in the schema",
       fields => #{
         introduceRoom => #{
           type => 'IntroduceRoomPayload',
           description => "Introduce a new room into the dungeon",
           resolve => fun mut_int_room/3,
           args => #{
           	input => #{
           		type => 'IntroduceRoomInput',
           		description => "The room to create" } }},
         introduceMonster => #{
           type => 'IntroduceMonsterPayload',
           description => "Introduce a new monster into the dungeon",
           resolve => fun mut_int_monster/3,
           args => #{
            input => #{
             type => 'IntroduceMonsterInput',
             description => "The monster to create" } }},
        introduceItem => #{
        	   type => 'IntroduceItemPayload',
        	   description => "Introduce a new item into the dungeon",
        	   resolve => fun mut_int_item/3,
        	   args => #{
        	      input => #{
        	         type => 'IntroduceItemInput',
        	         description => "The input for the mutation" } }},
        spawnMinion => #{
          type => 'SpawnMinionPayload',
          description => "Spawn a minion in a room in the dungeon",
          resolve=> fun mut_spawn_minion/3,
          args => #{
            input => #{
              type => 'SpawnMinionInput',
              description => "The monster and room to link" }}}
          
        }}}.

inject_item() ->
    Item = {object, #{
    		id => 'Item',
    		description => "Items in the dungeon",
    		interfaces => ['Node'],
    		fields => #{
    			id => #{
    				type => 'id!',
    				resolve => fun(_, #item { id = ID }, _) -> wrap({item, ID}) end,
    				description => "Unique identifier for Items" },
    			name => #{
    				type => 'string!',
    				resolve => fun(_, #item { name = N}, _) -> N end,
    				description => "Name of the item" },
    			description => #{
    				type => 'string!',
    				resolve => fun(_, #item { description = D}, _) -> D end,
    				description => "The description of the item" },
    			weight => #{
    				type => 'string!',
    				resolve => fun(_, #item {} = I, _) -> weight(I) end,
    				description => "Weight of the item itself, not its contents" },
    			weightSum => #{
    				type => 'string!',
    				resolve => fun(_, #item {} = I, _) -> weight_sum(I) end,
    				description => "The weight of the item and all its contents" },
    			contents => #{
    				type => ['Thing'],
    				resolve => fun(_, #item { contents = Cts }, _) ->
    				    ok([load(OID) || OID <- Cts]) end,
    				description => "The Items inside this item, if any" }
    	}}},
    ok = graphql:insert_schema_definition(Item),
    ok = graphql_relay:input('IntroduceItem',
      #{
        name => #{
        		type => 'string!',
        		description => "The name of the item to introduce" },
        	description => #{
        		type => 'string!',
        		description => "The description of the item to introduce" },
        	weight => #{
        		type => 'float!',
        		description => "The weight of the item itself. Not it's contents" }
      },
      #{
         item => #{
             type => 'Item',
             description => "The introduced item" }
      }),
    ok.

inject() ->
    inject_error(),
    inject_color(),
    inject_monster(),
    inject_room(),
    inject_item(),

    Thing = {union, #{
    		id => 'Thing',
    		description => "Things in the dungeon",
    		resolve_type => fun resolve_item/1,
    		types => ['Item', 'Monster']
    }},
    ok = graphql:insert_schema_definition(Thing),

    Node = {interface, #{
    	id => 'Node',
    	description => "The Relay specifications Node interface",
    	resolve_type => fun resolve_node/1,
    	fields => #{
    		id => #{
    			type => 'id!',
    			description => "The unique ID of an object" }
    	}
    }},
    ok = graphql:insert_schema_definition(Node),

    MutationRoot = mutations(),
    ok = graphql:insert_schema_definition(MutationRoot),

    QueryRoot = {object, #{
    		id => 'QueryRoot',
    		description => "Queries possible on the dungeon",
    		fields => #{
    			monster => #{
    				type => 'Monster',
    				description => "Request a thing",
    				resolve =>
    				    fun(Ctx, none, #{ <<"id">> := InputID }) ->
    				        case unwrap(InputID) of
    				            {monster, ID} = OID -> dirty_load(OID)
    				        end
    				    end,
    				args => #{
    					id => #{ type => 'id!', description => "The Monster ID to retrieve" }
    				}},
    			thing => #{
    				type => 'Thing',
    				description => "Request a thing",
    				resolve =>
    				    fun(Ctx, none, #{ <<"id">> := InputID }) ->
    				        case unwrap(InputID) of
    				            {monster, ID} = OID -> dirty_load(OID);
    				            {item, ID} = OID -> dirty_load(OID)
    				        end
    				    end,
    				args => #{
    					id => #{ type => 'id!', description => "The Thing ID to retrieve" }
    				}},
    			room => #{
    				type => 'Room',
    				description => "Request a particular room",
    				resolve =>
    				  fun(Ctx, none, #{ <<"id">> := InputID }) ->
    				    case unwrap(InputID) of
    				        {room, ID} = OID -> dirty_load(OID);
    				        _ -> null
    				    end
    				  end,
    				args => #{
    					id => #{ type => 'id!', description => "The Room ID to retrieve" }
    				}}
    	}}},
    	ok = graphql:insert_schema_definition(QueryRoot),
    	
    	Schema = {root, #{
    		query => 'QueryRoot',
    		mutation => 'MutationRoot',
    		interfaces => ['Node'] }},
    	ok = graphql:insert_schema_definition(Schema),
    	ok.

resolve_node(#monster{}) -> {ok, 'Monster'};
resolve_node(X) ->
    case unwrap(X) of
        {Ty, _ID} -> {ok, object_type(Ty)}
    end.
    
object_type(room) -> 'Room';
object_type(monster) -> 'Monster';
object_type(item) -> 'Item'.

resolve_item(#monster{}) -> {ok, 'Monster'};
resolve_item(X) ->
    case unwrap(X) of
        {room, _} -> {error, room};
        {item, _} -> {ok, 'Item'};
        {monster, _} -> {ok, 'Monster'}
    end.

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

weight(#item { weight = W }) -> W;
weight(_) -> 0.0.

weight_sum(#item { weight = W, contents = C }) ->
    W + lists:sum([load(OID) || OID <- C]);
weight_sum(_) -> 0.0.

mut_int_room(_Ctx, none, #{ <<"input">> := Input }) ->
    #{ <<"clientMutationId">> := MID,
       <<"description">> := D } = Input,
    {atomic, Room} = insert(#room { description = D }),
    {ok, #{
        <<"clientMutationId">> => MID,
        <<"room">> => Room } }.

mut_int_monster(_Ctx, none, #{ <<"input">> := Input }) ->
    #{ <<"clientMutationId">> := MID,
       <<"name">> := N,
       <<"color">> := {_,_,_} = C,
       <<"hitpoints">> := HP,
       <<"stats">> := Stats,
       <<"mood">> := M,
       <<"plushFactor">> := PF
    } = Input,
    S = case Stats of
            null -> #stats{};
            #{ <<"attack">> := Attack,
               <<"shellScripting">> := SHScript,
               <<"yell">> := Yell } ->
                #stats{
                   attack = Attack,
                   shell_scripting = SHScript,
                   yell = Yell
                  }
        end,
    {atomic, Monster} = insert(#monster { plush_factor = PF, stats = S, name = N, color = C, hitpoints = HP, mood = M }),
    {ok, #{
        <<"clientMutationId">> => MID,
        <<"monster">> => Monster } }.

mut_int_item(_Ctx, none, #{ <<"input">> := Input }) ->
    #{ <<"clientMutationId">> := MID,
       <<"name">> := N,
       <<"description">> := D,
       <<"weigth">> := W } = Input,
    {atomic, Item} = insert(#item { name = N, description = D, weight = W }),
    {ok, #{
        <<"clientMutationId">> => MID,
        <<"item">> => Item } }.

mut_spawn_minion(_Ctx, none, #{ <<"input">> := Input }) ->
    #{ <<"clientMutationId">> := MID,
       <<"monsterId">> := MonsterID,
       <<"roomId">> := RoomID } = Input,
    Txn = fun() ->
        [Room = #room{ contents = Contents }] = mnesia:read(unwrap(RoomID)),
        [Monster] = mnesia:read(unwrap(MonsterID)),
        NewRoom =
            Room#room {
                contents = ordsets:add_element({monster, Monster#monster.id}, Contents) },
        ok = mnesia:write(NewRoom),
        #{ <<"monster">> => Monster, <<"room">> => NewRoom }
    end,
    {atomic, Result} = mnesia:transaction(Txn),
    {ok, Result#{ <<"clientMutationId">> => MID }}.
