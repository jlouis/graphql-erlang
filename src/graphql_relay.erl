%%% Relay-specific conventions
%%
-module(graphql_relay).

-export([input/3, with_client_mutation/2]).
-export([pagination/1, pagination_fields/0, resolve_paginate/1, paginate/3]).

-define(DEFAULT_N, 10).

-spec with_client_mutation(M, F) -> Res
      when
        F :: fun((M) -> Res),
        Res :: {ok, M} | {error, any()}.
with_client_mutation(Input, Fun) ->
    with_mutation(Input, Fun, <<"clientMutationId">>).

-spec input(Name, InputFields, PayloadFields) -> ok
    when
        Name :: atom(),
        InputFields :: #{ atom() => any() },
        PayloadFields :: #{ atom() => any() }.

input(Ty, InputFields, PayloadFields) ->
    TyBin = atom_to_binary(Ty, utf8),
    
    Input =
        {input_object, #{
            id => <<TyBin/binary, "Input">>,
            description => ["Input object for ", TyBin],
            fields => maps:merge(InputFields,
                #{ clientMutationId => #{
                    	type => 'string',
                    	description => "Mutation ID for the client, if any" }})
        }},
    true = graphql_schema:insert_new(Input),
    
    Payload =
        {object, #{
            id => <<TyBin/binary, "Payload">>,
            description => ["Payload for ", TyBin],
            fields => maps:merge(PayloadFields,
                #{
                	clientMutationId => #{
                    	type => string,
                    	description => "Mutation ID for the client, if any" },
                  errors => #{
                  	type => ['Error'],
                  	description => "Errors from the mutation" }
                })
        }},
    true = graphql_schema:insert_new(Payload),
    ok.

paginate(Type, Description, AssocType) ->
    #{
        type => Type,
        description => Description,
        resolve => graphql_relay:resolve_paginate(AssocType),
        args => graphql_relay:pagination_fields()
     }.

-spec pagination(atom()) -> ok.
pagination(Type) ->
    TyB = atom_to_binary(Type, utf8),
    TyConn = binary_to_atom(<<TyB/binary, "Connection">>, utf8),
    TyEdge =  binary_to_atom(<<TyB/binary, "Edge">>, utf8),
    Edge = {object, #{
    	id => TyEdge,
    	description => ["Edges for the object ", TyB],
    	fields => #{
    		node => #{
    			type => Type,
    			description => "Underlying node for the edge" },
    		cursor => #{
    			type => 'Cursor',
    			description => "Cursor object" }
    }}},
    true = graphql_schema:insert_new(Edge),

    Connection = {object, #{
        id => TyConn,
        description => ["Connection for the type ", TyB],
        fields => #{
            edges => #{
            	type => [TyEdge],
            	description => "Edges in the connection" },
            pageInfo => #{
            	type => 'PageInfo',
            	description => "PageInfo for the connection" }
    }}},
    true = graphql_schema:insert_new(Connection),
    ok.

pagination_fields() -> pagination_fields(#{}).

pagination_fields(Additional) ->
    Base = #{ 
    	first => #{
    		type => int,
    		description => "Return the first K entries" },
    	'after' => #{
    		type => 'string',
    		description => "Retrieve Edges after this cursor entry" },
    	last => #{
    		type => int,
    		description => "Return the last K entries" },
    	before => #{
    		type => 'string',
    		description => "Retrieve Edges before this cursor entry" }
    },
    maps:merge(Base, Additional).

resolve_paginate(Type) ->
    fun (_Ctx, Src, Input) -> resolve_paginate(Type, Src, Input) end.

%% -- INTERNAL FUNCTIONS --------------------------------------

with_mutation(#{ <<"input">> := Input}, Fun, Key) ->
    {Mut, Rest} = take(Key, Input),
    case Fun(Rest) of
        {ok, null} -> {ok, #{ Key => Mut }};
        {ok, Result} -> {ok, Result#{ Key => Mut }};
        {error, Reason} -> {error, Reason}
    end.
    
take(K, Map) ->
    {maps:get(K, Map), maps:remove(K, Map)}.

resolve_paginate(Type, Source, Input) ->
    case gryphon_hydra:assoc_count(Source, Type) of
        {res, null, []} ->
            {error, invalid_type};
        {res, Count, []} ->
            Bounds = {0, Count},
            Sliced = cursors_to_edges(Input, Bounds),
            resolve_paginate_(Source, Type, Input, Sliced)
    end.

%% The spec says you can set both first and last, but the results are confusing
%% when doing so. Hence, we reject such attempts for the simplicity of the
%% implementation here.
resolve_paginate_(_Source, _Type, #{ <<"first">> := _, <<"last">> := _}, _Bounds) ->
    {error, first_last};
resolve_paginate_(_Source, _Type, #{ <<"first">> := N }, _Bounds) when N < 0 ->
    {error, negative_first};
resolve_paginate_(Source, Type, #{ <<"first">> := N }, {Lo, Hi}) ->
    Offset = Lo,
    Limit = min(Hi-Lo, N),
    {res, Edges, []} = gryphon_hydra:assoc_range(Source, Type, Offset, Limit),
    PageInfo = #{
        <<"hasNextPage">> => N < (Hi-Lo),
        <<"hasPreviousPage">> => false
    },
    {ok, #{
       <<"pageInfo">> => PageInfo,
       <<"edges">> => edges(Edges, Offset)
    }};
resolve_paginate_(_Source, _Type, #{ <<"last">> := N }, _Bounds) when N < 0 ->
    {error, negative_last};
resolve_paginate_(Source, Type, #{ <<"last">> := N }, {Lo, Hi}) ->
    Offset = max(Hi-N, 0),
    Limit = min(Hi-Lo, N),
    {res, Edges, []} = gryphon_hydra:assoc_range(Source, Type, Offset, Limit),
    PageInfo = #{
        <<"hasNextPage">> => false,
        <<"hasPreviousPage">> => N < (Hi-Lo)
    },
    {ok, #{
        <<"pageInfo">> => PageInfo,
        <<"edges">> => edges(Edges, Offset)
      }};
resolve_paginate_(Source, Type, #{}, Bound) ->
    resolve_paginate_(Source, Type, #{ <<"first">> => ?DEFAULT_N }, Bound).


cursors_to_edges(#{ <<"after">> := Cursor } = Input, {Lo, Hi}) ->
    K = unpack_cursor(Cursor),
    cursors_to_edges(maps:remove(<<"after">>, Input), slice({'after', K}, {Lo, Hi}));
cursors_to_edges(#{ <<"before">> := Cursor } = Input, {Lo, Hi}) ->
    K = unpack_cursor(Cursor),
    cursors_to_edges(maps:remove(<<"before">>, Input), slice({'before', K}, {Lo, Hi}));
cursors_to_edges(#{}, Bound) -> Bound.

%% Slice the structure into its pieces
slice({'after', K}, {Lo, Hi}) when K >= Lo andalso K < Hi ->
    {K+1, Hi};
slice({'before', K}, {Lo, Hi}) when K >= Lo andalso K < Hi ->
    {Lo, K};
slice(K, {Lo, Hi}) ->
    lager:warning("Slicing ~p in the range ~p", [K, {Lo, Hi}]),
    {Lo, Hi}.

unpack_cursor(Cur) ->
    Data = base64:decode(Cur),
    binary_to_integer(Data).

pack_cursor(K) ->
    base64:encode(integer_to_binary(K)).

edges([], _K) -> [];
edges([Obj | Next], K) ->
    Cursor = pack_cursor(K),
    Edge = #{ <<"node">> => Obj, <<"cursor">> => Cursor },
    [Edge | edges(Next, K+1)].

