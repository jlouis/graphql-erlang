-module(basic).

-export([inject/0]).
-export([resolve/3]).

inject() ->
    User = {object, #{ id => 'User',
         description => "A test user object",
    	fields => #{
    		id => #{
    			type => string,
    			description => "The identity of a user" },
    		name => #{
    			type => string,
    			description => "The name of the user" }
    }}},
    true = graphql_schema:insert_new(User),
     
    Query = {object, #{ id => 'Query',
         description => "Hello World!",
         fields => #{
            user => #{
            	type => 'User',
            	args => #{ id => #{
            		type => string,
            		description => "User ID to fetch"
            	}},
            	resolve => fun ?MODULE:resolve/3 },
             hello => #{
         	type => string,
         	resolve => fun(_, _, _) -> {ok, <<"world">>} end } } } },
    true = graphql_schema:insert_new(Query),

    Root = {root, #{
        query => 'Query',
        interfaces => []
    }},
    true = graphql_schema:insert_new(Root),
    ok.

resolve(_, _, #{ <<"id">> := ID }) ->
    case maps:get(ID, data(), not_found) of
        not_found -> {error, not_found};
        X -> {ok, X}
    end.

data() ->
    #{
        <<"1">> => #{ <<"id">> => <<"1">>, <<"name">> => <<"Dan">> },
        <<"2">> => #{ <<"id">> => <<"2">>, <<"name">> => <<"Marie">> },
        <<"3">> => #{ <<"id">> => <<"3">>, <<"name">> => <<"Jessie">> }
    }.

