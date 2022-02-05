-module(blog).
-include_lib("graphql/include/graphql.hrl").

-export([inject/0, execute/4]).

-spec inject() -> ok.
inject() ->
    BlogImage = {object, #{
                   id => 'Image',
                   resolve_module => ?MODULE,
                   description => "Blog Images",
                   fields => #{
                     url => #{
                       type => 'String',
                       description => "The URL of an image" },
                     width => #{
                       type => 'Int',
                       description => "The width of the image in pixels" },
                     height => #{
                       type => 'Int',
                       description => "The height of the image in pixels" }
                    }}},
    	
    BlogAuthor = {object, #{
                    id => 'Author',
                    resolve_module => ?MODULE,
                    description => "Blog Authors",
                    fields => #{
                      id => #{
                        type => 'ID',
                        description => "The ID of the author" },
                      name => #{
                        type => 'String',
                        description => "The name of the author" },
                      pic => #{
                        type => 'Image',
                        args => #{
                          width => #{
                            type => 'Int', description => "Width request" },
                          height => #{
                            type => 'Int', description => "Height request" }
                         },
                        resolve => fun pic_resolve/3 },
                      recentArticle => #{
                        type => 'Article',
                        description => "The most recent article of the Author" }
                     }}},
    	
    BlogArticle = {object, #{
                     id => 'Article',
                     resolve_module => ?MODULE,
                     description => "Blog Articles",
                     fields => #{
                       id => #{
                         type => 'ID!',
                         description => "The ID of the article" },
                       isPublished => #{
                         type => 'Boolean',
                         description => "True is the article has been published"},
                       author => #{
                         type => 'Author',
                         description => "Author of the article" },
                       title => #{
                         type => 'String',
                         description => "The Title of the article" },
                       body => #{
                         type => 'String',
                         description => "The Article body itself" },
                       keywords => #{
                         type => ['String'],
                         description => "Keywords for the article" }
                      }}},
    	
    BlogQuery = {object, #{
                   id => 'Query',
                   resolve_module => ?MODULE,
                   description => "Blog Queries",
                   fields => #{
                     article => #{
                       type => 'Article',
                       description => "Query for a specific article",
                       args => #{
                         id => #{ type => 'Int',
                                  description => "ID of the article to request" }
                        },
                       resolve => fun query_id_resolve/3
                      },
                     feed => #{
                       type => ['Article'],
                       description => "A feed of articles",
                       resolve => fun query_feed_resolve/3
                      }
                    }}},
    	
    Schema = {root, #{ query => 'Query', interfaces => [] }},
    
    ok = graphql:insert_schema_definition(BlogImage),
    ok = graphql:insert_schema_definition(BlogAuthor),
    ok = graphql:insert_schema_definition(BlogArticle),
    ok = graphql:insert_schema_definition(BlogQuery),
    ok = graphql:insert_schema_definition(Schema),
    ok.
    
execute(_Ctx, null, _, _) ->
    {ok, null};
execute(_Ctx, Obj, FieldName, _Args) ->
    case maps:get(FieldName, Obj, not_found) of
        {'$lazy', F} when is_function(F, 0) -> F();
        not_found -> {error, not_found};
        Values when is_list(Values) -> {ok, [ {ok, R} || R <- Values ]};
        Value -> {ok, Value}
     end.

query_id_resolve(_Ctx, _, #{ <<"id">> := ID }) ->
    article(ID).

pic_resolve(_Ctx, #{ <<"id">> := ID }, #{ <<"width">> := W, <<"height">> := H }) ->
    {ok, get_pic(ID, W, H)}.

query_feed_resolve(_Ctx, _, _) ->
    {ok, [article(X) || X <- lists:seq(1,10)]}.

article(ID) ->
    IDBin = integer_to_binary(ID),
    {ok, #{
    	<<"id">> => IDBin,
    	<<"isPublished">> => true,
    	<<"author">> => ?LAZY(john_smith()),
    	<<"title">> => <<"My article number ", IDBin/binary>>,
    	<<"body">> => <<"This is a post">>,
    	<<"hidden">> => <<"This data is not exposed in the schema">>,
    	<<"keywords">> => [
    		<<"foo">>, <<"bar">>, 1, true, null ]
    }}.
    
john_smith() ->
    {ok, Recent} = article(1),
    {ok, #{
    	<<"id">> => <<"123">>,
    	<<"name">> => <<"John Smith">>,
    	<<"pic">> => fun(Width, Height) -> get_pic(123, Width, Height) end,
    	<<"recentArticle">> => Recent
    }}.
    
get_pic(UID, W, H) ->
   #{
   	<<"url">> => <<"cdn://", UID/binary>>,
   	<<"width">> => W,
   	<<"height">> => H
   }.
