-module(colors).

-export([inject/0]).

inject() ->
     Color = {enum, #{
     	id => 'Color',
     	description => "A test representation of color",
     	values => #{
     	    <<"RED">> => #{ value => 0, description => "The color red" },
     	    <<"GREEN">> => #{ value => 1, description => "The color green" },
     	    <<"BLUE">> => #{ value => 2, description => "The color blue" }
     	}
     }},
     true = gql_schema:insert_new(Color),

     Query = {object, #{
     	id => 'Query',
     	description => "Top level query object",
     	fields => #{
     		colorEnum => #{
     			type => 'Color',
     			description => "A color enumerated type",
     			args => #{
     				'fromEnum' =>
     				    #{ type => 'Color', description => "" },
				'fromInt' =>
				    #{ type => int, description => "" },
				'fromString' =>
				    #{ type => string, description => "" }
			},
			resolve => fun color_enum/3
		},
		colorInt => #{
			type => int,
			description => "Colors as integers",
			args => #{
				fromEnum => #{ type => 'Color', description => "" },
				fromInt => #{ type => int, description => "" }
			},
			resolve => fun
			    (_Ctx, _, #{ <<"fromInt">> := X}) -> {ok, color_from_int(X)};
			    (_Ctx, _, #{ <<"fromEnum">> := X}) -> {ok, color_from_enum(X)};
			    (_Ctx, _, #{}) -> {error, cannot_resolve_color}
			end
		}
	}
    }},
    true = gql_schema:insert_new(Query),

    Mutation = {object, #{
        id => 'Mutation',
        description => "Top level mutation query",
        fields => #{
        		favoriteEnum => #{
        			type => 'Color',
        			args => #{
        				color => #{ type => 'Color', description => "" }
        			},
        			resolve => fun
        			    (_, V, #{ <<"color">> := {enum, C} }) -> {ok, C};
        			    (_, _V, #{}) -> {error, cannot_resolve_color}
        			end
        		}
        	}
    }},
    true = gql_schema:insert_new(Mutation),
    
    Root = {root, #{
       query => 'Query',
       mutation => 'Mutation',
       interfaces => []
    }},
    true = gql_schema:insert_new(Root),
    ok.

-define(RED, 0).
-define(GREEN, 1).
-define(BLUE, 2).

color_from_int(0) -> 0;
color_from_int(1) -> 1;
color_from_int(2) -> 2.

color_from_enum({enum, En}) ->
    case En of
        <<"RED">> -> ?RED;
        <<"GREEN">> -> ?GREEN;
        <<"BLUE">> -> ?BLUE
    end.

color_from_string(X) -> color_from_enum({enum, X}).

color_enum(Ctx, _, #{ <<"fromEnum">> := X}) -> {ok, color_from_enum(X)};
color_enum(Ctx, _, #{ <<"fromInt">> := X}) -> {ok, color_from_int(X)};
color_enum(Ctx, _, #{ <<"fromString">> := X}) -> {ok, color_from_string(X)};
color_enum(Ctx, _, #{}) -> {ok, null}.
