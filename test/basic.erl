-module(basic).

-export([inject/0]).
-export([execute/4]).

inject() ->
    User = {object, #{
              id => 'User',
              resolve_module => ?MODULE,
              description => "A test user object",
              fields => #{
                id => #{
                  type => 'String',
                  description => "The identity of a user" },
                name => #{
                  type => 'String',
                  description => "The name of the user" }
               }}},
    ok = graphql:insert_schema_definition(User),
     
    Query = {object, #{
               id => 'Query',
               resolve_module => ?MODULE,
               description => "Hello World!",
               fields => #{
                 user => #{
                   type => 'User',
                   args => #{ id => #{
                                type => 'String',
                                description => "User ID to fetch"
                               }}},
                 hello => #{
                   type => 'String',
                   description => "Hello World Test"}}}},
    ok = graphql:insert_schema_definition(Query),

    Root = {root, #{
        query => 'Query',
        interfaces => []
    }},
    ok = graphql:insert_schema_definition(Root),
    ok.

execute(_Ctx, _Object, <<"user">>, #{ <<"id">> := ID }) ->
    case maps:get(ID, data(), not_found) of
        not_found -> {error, not_found};
        X -> {ok, X}
    end;
execute(_Ctx, _, <<"hello">>, _) ->
    {ok, <<"world">>};
execute(_Ctx, #{ <<"id">> := ID }, <<"id">>, _) ->
    {ok, ID};
execute(_Ctx, #{ <<"name">> := Name }, <<"name">>, _) ->
    {ok, Name}.

data() ->
    #{
        <<"1">> => #{ <<"id">> => <<"1">>, <<"name">> => <<"Dan">> },
        <<"2">> => #{ <<"id">> => <<"2">>, <<"name">> => <<"Marie">> },
        <<"3">> => #{ <<"id">> => <<"3">>, <<"name">> => <<"Jessie">> }
    }.

