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


resolve_input({enum, X}) when is_binary(X) -> resolve_input(X);
resolve_input(<<"RED">>) -> 0;
resolve_input(<<"GREEN">>) -> 1;
resolve_input(<<"BLUE">>) -> 2.

resolve_output(0) -> <<"RED">>;
resolve_output(1) -> <<"GREEN">>;
resolve_output(2) -> <<"BLUE">>.

color_enum(_Ctx, _, #{
    <<"fromEnum">> := null,
    <<"fromInt">> := null,
    <<"fromString">> := null}) -> {ok, null};
color_enum(_Ctx, _, #{
    <<"fromEnum">> := X,
    <<"fromInt">> := null,
    <<"fromString">> := null}) -> {ok, resolve_output(
                                         resolve_input(X))};
color_enum(_Ctx, _, #{
    <<"fromEnum">> := null,
    <<"fromInt">> := X,
    <<"fromString">> := null})
  when X >= 0 andalso X < 3 -> {ok, resolve_output(X)};
color_enum(_Ctx, _, #{
    <<"fromEnum">> := null,
    <<"fromInt">> := null,
    <<"fromString">> := X}) -> {ok, resolve_output(
                                      resolve_input(X))}.

color_int(_Ctx, _, #{
    <<"fromEnum">> := null,
    <<"fromInt">> := null }) -> {ok, null};
color_int(_Ctx, _, #{
    <<"fromEnum">> := null,
    <<"fromInt">> := X}) when X /= null -> {ok, X};
color_int(_Ctx, _, #{
    <<"fromEnum">> := X,
    <<"fromInt">> := null }) when X /= null -> {ok, resolve_input(X)}.




