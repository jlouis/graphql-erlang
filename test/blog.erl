-module(blog).
-include_lib("graphql/include/graphql.hrl").

-export([inject/0]).

-spec inject() -> ok.
inject() ->
    BlogImage = {object, #{
    	id => 'Image',
    	description => "Blog Images",
    	fields => #{
    		url => #{
    			type => string, description => "The URL of an image" },
    		width => #{
    			type => int, description => "The width of the image in pixels" },
    		height => #{
    			type => int, description => "The height of the image in pixels" }
    	}}},
    	
    BlogAuthor = {object, #{
    	id => 'Author',
    	description => "Blog Authors",
    	fields => #{
    		id => #{
    			type => id, description => "The ID of the author" },
    		name => #{
    			type => string, description => "The name of the author" },
    		pic => #{
    			type => 'Image',
    			args => #{
    				width => #{
    					type => int, description => "Width request" },
    				height => #{
    					type => int, description => "Height request" }
    			},
    			resolve => fun pic_resolve/3 },
    		recentArticle => #{
    			type => 'Article', description => "The most recent article of the Author" }
    	}}},
    	
    BlogArticle = {object, #{
    	id => 'Article',
    	description => "Blog Articles",
    	fields => #{
    		id => #{
    			type => 'id!', description => "The ID of the article" },
    		isPublished => #{
    			type => bool, description => "True is the article has been published"},
    		author => #{
    			type => 'Author', description => "Author of the article" },
    		title => #{
    			type => string, description => "The Title of the article" },
    		body => #{
    			type => string, description => "The Article body itself" },
    		keywords => #{
    			type => [string], description => "Keywords for the article" }
    	}}},
    	
    BlogQuery = {object, #{
    	id => 'Query',
    	description => "Blog Queries",
    	fields => #{
    		article => #{
    			type => 'Article',
    			description => "Query for a specific article",
    			args => #{
    				id => #{ type => int, description => "ID of the article to request" }
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
    
    graphql_schema:insert_new(BlogImage),
    graphql_schema:insert_new(BlogAuthor),
    graphql_schema:insert_new(BlogArticle),
    graphql_schema:insert_new(BlogQuery),
    graphql_schema:insert_new(Schema),
    ok.
    
query_id_resolve(_Ctx, _, #{ <<"id">> := ID }) ->
    article(ID).

pic_resolve(_Ctx, #{ <<"id">> := ID }, #{ <<"width">> := W, <<"height">> := H }) ->
    {ok, get_pic(ID, W, H)}.

query_feed_resolve(_Ctx, _, _) ->
    ok([article(X) || X <- lists:seq(1,10)]).

ok(Data) ->
    {ok, [X || {ok, X} <- Data]}.

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
