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
     ok = graphql:insert_schema_definition(Color),

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
			resolve => fun color_int/3
		}
	}
    }},
    ok = graphql:insert_schema_definition(Query),

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
        			    (_, _V, #{ <<"color">> := {enum, C} }) -> {ok, C};
        			    (_, _V, #{}) -> {error, cannot_resolve_color}
        			end
        		}
        	}
    }},
    ok = graphql:insert_schema_definition(Mutation),
    
    Root = {root, #{
       query => 'Query',
       mutation => 'Mutation',
       interfaces => []
    }},
    ok = graphql:insert_schema_definition(Root),
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

color_enum(_Ctx, _, #{
    <<"fromEnum">> := null,
    <<"fromInt">> := null,
    <<"fromString">> := null}) -> {ok, null};
color_enum(_Ctx, _, #{
    <<"fromEnum">> := X,
    <<"fromInt">> := null,
    <<"fromString">> := null}) -> {ok, color_from_enum(X)};
color_enum(_Ctx, _, #{
    <<"fromEnum">> := null,
    <<"fromInt">> := X,
    <<"fromString">> := null}) -> {ok, color_from_int(X)};
color_enum(_Ctx, _, #{
    <<"fromEnum">> := null,
    <<"fromInt">> := null,
    <<"fromString">> := X}) -> {ok, color_from_string(X)}.

color_int(_Ctx, _, #{
    <<"fromEnum">> := null,
    <<"fromInt">> := null }) -> {ok, null};
color_int(_Ctx, _, #{
    <<"fromEnum">> := null,
    <<"fromInt">> := X }) when X /= null -> {ok, color_from_int(X)};
color_int(_Ctx, _, #{
    <<"fromEnum">> := X,
    <<"fromInt">> := null }) when X /= null -> {ok, color_from_enum(X)}.
