-module(schema_colors).

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
				    #{ type => 'Int', description => "" },
				'fromString' =>
				    #{ type => 'String', description => "" }
			},
			resolve => fun color_enum/3
		},
		colorInt => #{
			type => 'Int',
			description => "Colors as integers",
			args => #{
				fromEnum => #{ type => 'Color', description => "" },
				fromInt => #{ type => 'Int', description => "" }
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
        			    (_, _V, #{ <<"color">> :=  C }) -> {ok, C};
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

%% Internal representation is a set of atoms
resolve_input(enum, {enum, X}) when is_binary(X) -> resolve_input(string, X);
resolve_input(int, 0) -> 'RED';
resolve_input(int, 1) -> 'GREEN';
resolve_input(int, 2) -> 'BLUE';
resolve_input(string, <<"YELLOW">>) -> 'YELLOW';
resolve_input(string, <<"RED">>) -> 'RED';
resolve_input(string, <<"GREEN">>) -> 'GREEN';
resolve_input(string, <<"BLUE">>) -> 'BLUE'.

resolve_output(enum, 'YELLOW') -> {ok, <<"YELLOW">>}; %% This one is invalid
resolve_output(enum, 'RED') -> {ok, <<"RED">>};
resolve_output(enum, 'GREEN') -> {ok, <<"GREEN">>};
resolve_output(enum, 'BLUE') -> {ok, <<"BLUE">>};
resolve_output(int, 'RED') -> {ok, 0};
resolve_output(int, 'GREEN') -> {ok, 1};
resolve_output(int, 'BLUE') -> {ok, 2}.
    
color_enum(_Ctx, _, #{
    <<"fromEnum">> := null,
    <<"fromInt">> := null,
    <<"fromString">> := null}) -> {ok, null};
color_enum(_Ctx, _, #{
    <<"fromEnum">> := X,
    <<"fromInt">> := null,
    <<"fromString">> := null}) -> resolve_output(enum,
                                    resolve_input(enum, X));
color_enum(_Ctx, _, #{
    <<"fromEnum">> := null,
    <<"fromInt">> := X,
    <<"fromString">> := null})
  when X >= 0 andalso X < 3 -> resolve_output(enum,
                                 resolve_input(int, X));
color_enum(_Ctx, _, #{
    <<"fromEnum">> := null,
    <<"fromInt">> := null,
    <<"fromString">> := X}) -> resolve_output(enum,
                                 resolve_input(string, X)).

color_int(_Ctx, _, #{
    <<"fromEnum">> := null,
    <<"fromInt">> := null }) -> {ok, null};
color_int(_Ctx, _, #{
    <<"fromEnum">> := null,
    <<"fromInt">> := X}) when X /= null -> resolve_output(int,
                                             resolve_input(int, X));
color_int(_Ctx, _, #{
    <<"fromEnum">> := X,
    <<"fromInt">> := null }) when X /= null -> resolve_output(int,
                                                 resolve_input(enum, X)).

