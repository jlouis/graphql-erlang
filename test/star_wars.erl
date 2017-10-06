-module(star_wars).

-export([inject/0]).

inject() ->
    init_starwars(),
    Query = {object, #{ id => 'Query',
      description => "Simple top-level Query Schema.",
      fields =>
        #{
            hero => #{
            	type => 'Character',
            	args => #{
            		episode => #{
            			type => 'Episode',
            			description =>
            			    "Returns the hero of a particular episode "
            			    "or the whole saga if omitted" } },
            	resolve => fun(Ctx, _Cur, Args) -> get_hero(Ctx, Args) end },
            human => #{
            	type => 'Human',
            	args => #{
            		id => #{
            			type => 'string!',
            			description => "The ID of the human" } },
            	resolve => fun(Ctx, _Cur, Args) -> get_human(Ctx, Args) end },
            droid => #{
            	type => 'Droid',
            	args => #{
            		id => #{
            			type => 'string!',
            			description => "The ID of the droid" } },
            	resolve => fun(Ctx, _Cur, Args) -> get_droid(Ctx, Args) end }

         } } },
    ok = graphql:insert_schema_definition(Query),

    Root = {root, #{
    	query => 'Query',
    	interaces => []
    }},
    ok = graphql:insert_schema_definition(Root),
    ok.

init_starwars() ->
    Episodes = {enum, #{
         id => 'Episode',
         description => "One of the films in the Star Wars Trilogy",
         values => #{
             'NEWHOPE' => #{ value => 4, description => "Released in 1977." },
             'EMPIRE' => #{ value => 5, description => "Released in 1980." },
             'JEDI' => #{ value => 6, description => "Released in 1983." } }
    }},
    Character = {interface, #{
        id => 'Character',
        description => "A character in the Star Wars Trilogy",
        resolve_type => fun(X) ->
            case is_human(X) of
                true -> {ok, 'Human'};
                false -> {ok, 'Droid'}
            end
          end,
        fields => #{
            id => #{
                type => string,
                description => "The ID of a character" },
            name => #{
                type => string,
                description => "The name of the character" },
            friends => #{
                type => ['Character'],
                description => "The friends of the character, possibly empty if they have none" },
            appearsIn => #{
                type => ['Episode'],
                description => "Which movies they appear in." }
    }}},
    Human = {object, #{
    	id => 'Human',
    	description => "A humanoid creature in the Star Wars Universe.",
    	fields => #{
    	    id => #{
    	    	type => string,
    	    	description => "The ID of the human" },
    	    name => #{
    	    	type => string,
    	    	description => "The name of the human" },
    	    friends => #{
    	    	type => ['Character'],
    	    	description => "The friends of the human, possibly empty list if no friends",
    	    	resolve => fun(_, Human, _) -> get_friends(Human) end },
    	    appearsIn => #{
    	    	type => ['Episode'],
    	    	description => "Which movies they appear in." },
    	    homePlanet => #{
    	    	type => string,
    	    	description => "The home planet of the human, or null if unknown." }
    	    },
        interfaces => [ 'Character' ]
    }},
    Droid = {object, #{
    	id => 'Droid',
    	description => "A mechanical create in the Star Wars universe.",
    	fields => #{
    		id => #{
    			type => string,
    			description => "The ID of the droid." },
    		name => #{
    			type => string,
    			description => "The name of the droid." },
    		friends => #{
    			type => ['Character'],
    			description => "The friends of the droid.",
    			resolve => fun(_, Droid, _) -> get_friends(Droid) end },
    		appearsIn => #{
    			type => ['Episode'],
    			description => "Which movies they appear in." },
    		primaryFunction => #{
    			type => string,
    			description => "The primary function of the droid." }
    		},
    	interfaces => ['Character']
    }},
    ok = graphql:insert_schema_definition(Episodes),
    ok = graphql:insert_schema_definition(Character),
    ok = graphql:insert_schema_definition(Human),
    ok = graphql:insert_schema_definition(Droid),
    ok.

%% DATA
is_human(ID) ->
    {Humans, _} = star_wars(),
    maps:is_key(ID, Humans).

get_character(ID) ->
    {Humans, Droids} = star_wars(),
    case maps:get(ID, maps:merge(Humans, Droids), not_found) of
        not_found -> {ok, null};
        X -> {ok, X}
    end.

get_friends(#{ <<"friends">> := Friends }) ->
    {ok, [get_character(F) || F <- Friends]}.

get_hero(_Ctx, #{ <<"episode">> := {enum, <<"EMPIRE">>} }) ->
    {Humans, _} = star_wars(),
    {ok, maps:get(<<"1000">>, Humans)};
get_hero(_Ctx, _) ->
    {_, Droids} = star_wars(),
    {ok, maps:get(<<"2001">>, Droids)}.

get_human(_Ctx, #{ <<"id">> := ID }) ->
    {Humans, _} = star_wars(),
    {ok, maps:get(ID, Humans, null)}.

get_droid(_Ctx, #{ <<"id">> := ID }) ->
    {_, Droids} = star_wars(),
    {ok, maps:get(ID, Droids, null)}.

star_wars() ->
    Luke = #{
    	id => <<"1000">>,
    	name => <<"Luke Skywalker">>,
    	friends => [<<"1002">>,<< "1003">>, <<"2000">>, <<"2001">> ],
    	appearsIn => resolve_module([ 4, 5, 6 ]),
    	homePlanet => <<"Tatooine">> },
    Vader = #{
    	id => <<"1001">>,
    	name => <<"Darth Vader">>,
    	friends => [<<"1004">>],
    	appearsIn => resolve_module([ 4, 5, 6 ]),
    	homePlanet => <<"Tatooine">> },
    Han = #{
    	id => <<"1002">>,
    	name => <<"Han Solo">>,
    	friends => [<<"1000">>, <<"1003">>, <<"2001">>],
    	appearsIn => resolve_module([ 4, 5, 6])},
    Leia = #{
    	id => <<"1003">>,
    	name => <<"Leia Organa">>,
    	friends => [<<"1000">>, <<"1002">>, <<"2000">>, <<"2001">> ],
    	appearsIn => resolve_module([ 4, 5, 6]),
    	homePlanet => <<"Alderaan">> },
    Tarkin = #{
    	id => <<"1004">>,
    	name => <<"Wilhuff Tarkin">>,
    	friends => [ <<"1001">> ],
    	appearsIn => resolve_module([ 4 ]) },

    HumanData = #{
    	<<"1000">> => c(Luke),
    	<<"1001">> => c(Vader),
    	<<"1002">> => c(Han),
    	<<"1003">> => c(Leia),
    	<<"1004">> => c(Tarkin)
    },

    Threepio = #{
    	id => <<"2000">>,
    	name => <<"C-3PO">>,
    	friends => [ <<"1000">>, <<"1002">>, <<"1003">>, <<"2001">> ],
    	appearsIn => resolve_module([ 4, 5, 6 ]),
    	primaryFunction => <<"Protocol">>
    },

    Artoo = #{
    	id => <<"2001">>,
    	name => <<"R2-D2">>,
    	friends => [ <<"1000">>, <<"1002">>, <<"1003">> ],
    	appearsIn => resolve_module([ 4, 5, 6 ]),
    	primaryFunction => <<"AstroMech">>
    },

    DroidData = #{
    	<<"2000">> => c(Threepio),
    	<<"2001">> => c(Artoo)
    },

    {HumanData, DroidData}.

c(M) ->
    Unpacked = maps:to_list(M),
    maps:from_list(
    	[{atom_to_binary(K, utf8), V} || {K, V} <- Unpacked]).

resolve_arg(X) ->
    case X of
	<<"NEWHOPE">> ->
 	    4;
 	<<"EMPIRE">> ->
 	    5;
 	<<"JEDI">> ->
 	    6;
 	'_' ->
 	    {error, {invalid_episode_string, X}};
	4 ->
  	    <<"NEWHOPE">>;
 	5 ->
  	    <<"EMPIRE">>;
  	6 ->
  	    <<"JEDI">>;
  	_ ->
  	    {error, {invalid_episode_int, X}}
     end.

map(F, [H|T]) -> [F(H)|map(F, T)];
map(_F, []) -> [].

resolve_module(L) when is_list(L) ->
    map(fun(X) -> resolve_arg(X) end, L).
