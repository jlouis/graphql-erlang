%%% Relay-specific conventions
%%
-module(graphql_relay).

-export([input/3, with_client_mutation/2]).
-export([pagination/1, pagination_fields/0, resolve_paginate/2, paginate/3, paginate/4]).

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
    ok = graphql:insert_schema_definition(Input),

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
    ok = graphql:insert_schema_definition(Payload),
    ok.

paginate(Type, Description, AssocType) ->
    paginate(Type, Description, AssocType, #{pagination_type => sequential}).

paginate(Type, Description, AssocType, PaginationInfo) ->
    #{
        type => Type,
        description => Description,
        resolve => graphql_relay:resolve_paginate(PaginationInfo, AssocType),
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
    ok = graphql:insert_schema_definition(Edge),

    Connection = {object, #{
        id => TyConn,
        description => ["Connection for the type ", TyB],
        fields => #{
            edges => #{
                type => [TyEdge],
                description => "Edges in the connection" },
            pageInfo => #{
                type => 'PageInfo',
                description => "PageInfo for the connection" },
            count => #{
                type => int,
                description => "Count of edges in the connection" }
    }}},
    ok = graphql:insert_schema_definition(Connection),
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

resolve_paginate(PaginationInfo, Type) ->
    fun (_Ctx, Src, Input) -> resolve_paginate(PaginationInfo, Type, Src, Input) end.

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

resolve_paginate(#{ pagination_type := sequential }, Type, Source, Input) ->
    case gryphon_hydra:assoc_count(Source, Type) of
        {res, null, []} ->
            {error, invalid_type};
        {res, Count, []} ->
            Bounds = {0, Count},
            Sliced = cursors_to_edges(Input, Bounds),
            resolve_paginate_sequential(Source, Type, Input, Sliced, Count)
    end;
resolve_paginate(#{ pagination_type := sequential_assoc, target_field := TargetField }, Type, Source, Input) ->
    case gryphon_hydra:assoc_count(Source, Type) of
        {res, null, []} ->
            {error, invalid_type};
        {res, Count, []} ->
            Bounds = {0, Count},
            Sliced = cursors_to_edges(Input, Bounds),
            resolve_paginate_sequential_assoc(Source, Type, Input, Sliced, Count, TargetField)
    end;
resolve_paginate(#{ pagination_type := ordered }, Type, Source, Input) ->
    Bounds = {0, 9000000000000000000},
    Sliced = cursors_to_edges(Input, Bounds),
    resolve_paginate_ordered(Source, Type, Input, Sliced).

%% The spec says you can set both first and last, but the results are confusing
%% when doing so. Hence, we reject such attempts for the simplicity of the
%% implementation here.
resolve_paginate_sequential(_Source, _Type, #{ <<"first">> := F, <<"last">> := L}, _Bounds, _Count) when F /= null, L /= null ->
    {error, first_last};
resolve_paginate_sequential(_Source, _Type, #{ <<"first">> := N }, _Bounds, _Count) when N /= null, N < 0 ->
    {error, negative_first};
resolve_paginate_sequential(Source, Type, #{ <<"first">> := N }, {Lo, Hi}, Count) when N /= null ->
    Offset = Lo,
    Limit = min(Hi-Lo, N),
    {res, Edges, []} = gryphon_hydra:assoc_range(Source, Type, Offset, Limit),
    PageInfo = #{
        <<"hasNextPage">> => N < (Hi-Lo),
        <<"hasPreviousPage">> => false
    },
    {ok, #{
       <<"pageInfo">> => PageInfo,
       <<"edges">> => edges_sequential(Edges, Offset),
       <<"count">> => Count
    }};
resolve_paginate_sequential(_Source, _Type, #{ <<"last">> := N }, _Bounds, _Count) when N /= null, N < 0 ->
    {error, negative_last};
resolve_paginate_sequential(Source, Type, #{ <<"last">> := N }, {Lo, Hi}, Count) when N /= null ->
    Offset = max(Hi-N, 0),
    Limit = min(Hi-Lo, N),
    {res, Edges, []} = gryphon_hydra:assoc_range(Source, Type, Offset, Limit),
    PageInfo = #{
        <<"hasNextPage">> => false,
        <<"hasPreviousPage">> => N < (Hi-Lo)
    },
    {ok, #{
        <<"pageInfo">> => PageInfo,
        <<"edges">> => edges_sequential(Edges, Offset),
        <<"count">> => Count
      }};
resolve_paginate_sequential(Source, Type, #{}, Bound, Count) ->
    resolve_paginate_sequential(Source, Type, #{ <<"first">> => ?DEFAULT_N }, Bound, Count).

resolve_paginate_sequential_assoc(_Source, _Type, #{ <<"first">> := F, <<"last">> := L}, _Bounds, _Count, _TargetField) when F /= null, L /= null ->
    {error, first_last};
resolve_paginate_sequential_assoc(_Source, _Type, #{ <<"first">> := N }, _Bounds, _Count, _TargetField) when N /= null, N < 0 ->
    {error, negative_first};
resolve_paginate_sequential_assoc(Source, Type, #{ <<"first">> := N }, {Lo, Hi}, Count, TargetField) when N /= null ->
    Offset = Lo,
    Limit = min(Hi-Lo, N),
    {res, Res, []} = gryphon_hydra:assoc_range_data(Source, Type, Offset, Limit),
    Edges = lists:map(
        fun({Target, Data}) ->
            maps:merge(Data, #{TargetField => Target})
        end,
        Res
    ),
    PageInfo = #{
        <<"hasNextPage">> => N < (Hi-Lo),
        <<"hasPreviousPage">> => false
    },
    {ok, #{
        <<"pageInfo">> => PageInfo,
        <<"edges">> => edges_sequential(Edges, Offset),
        <<"count">> => Count
    }};
resolve_paginate_sequential_assoc(_Source, _Type, #{ <<"last">> := N }, _Bounds, _Count, _TargetField) when N /= null, N < 0 ->
    {error, negative_last};
resolve_paginate_sequential_assoc(Source, Type, #{ <<"last">> := N }, {Lo, Hi}, Count, TargetField) when N /= null ->
    Offset = max(Hi-N, 0),
    Limit = min(Hi-Lo, N),
    {res, Res, []} = gryphon_hydra:assoc_range_data(Source, Type, Offset, Limit),
    Edges = lists:map(
        fun({Target, Data}) ->
            maps:merge(Data, #{TargetField => Target})
        end,
        Res
    ),
    PageInfo = #{
        <<"hasNextPage">> => false,
        <<"hasPreviousPage">> => N < (Hi-Lo)
    },
    {ok, #{
        <<"pageInfo">> => PageInfo,
        <<"edges">> => edges_sequential(Edges, Offset),
        <<"count">> => Count
    }};
resolve_paginate_sequential_assoc(Source, Type, #{}, Bound, Count, TargetField) ->
    resolve_paginate_sequential_assoc(Source, Type, #{ <<"first">> => ?DEFAULT_N }, Bound, Count, TargetField).

resolve_paginate_ordered(_Source, _Type, #{ <<"first">> := F, <<"last">> := L}, _Bounds) when F /= null, L /= null ->
    {error, first_last};
resolve_paginate_ordered(_Source, _Type, #{ <<"first">> := N }, _Bounds) when N /= null, N < 0 ->
    {error, negative_first};
resolve_paginate_ordered(Source, Type, #{ <<"first">> := N }, {Lo, Hi}) when N /= null ->
    Options = [
        {high_low, #{high => Hi, low => Lo}},
        {limit, N + 1},
        {order_by, client_timestamp},
        {order, asc}
    ],
    {res, Edges, []} = gryphon_hydra:assoc_range(Source, Type, Options),
    Count = length(Edges),
    PageInfo = #{
        <<"hasNextPage">> => Count > N,
        <<"hasPreviousPage">> => false
    },
    {ok, #{
        <<"pageInfo">> => PageInfo,
        <<"edges">> => edges_ordered(Edges, N),
        <<"count">> => Count
    }};
resolve_paginate_ordered(_Source, _Type, #{ <<"last">> := N }, _Bounds) when N /= null, N < 0 ->
    {error, negative_last};
resolve_paginate_ordered(_Source, _Type, #{ <<"last">> := N }, {_Lo, _Hi}) when N /= null ->
    {error, not_implemented};
%%    {res, Edges, []} = gryphon_hydra:assoc_ordered_range(Source, Type, Hi, Lo, N + 1),
%%    Count = length(Edges),
%%    PageInfo = #{
%%        <<"hasNextPage">> => false,
%%        <<"hasPreviousPage">> => N < (Hi-Lo)
%%    },
%%    {ok, #{
%%        <<"pageInfo">> => PageInfo,
%%        <<"edges">> => edges_ordered(Edges),
%%        <<"count">> => Count
%%    }};
resolve_paginate_ordered(Source, Type, #{}, Bound) ->
    resolve_paginate_ordered(Source, Type, #{ <<"first">> => ?DEFAULT_N }, Bound).

cursors_to_edges(#{<<"after">> := null,<<"before">> := null}, Bound) -> Bound;
cursors_to_edges(#{ <<"after">> := Cursor } = Input, {Lo, Hi}) ->
    {_CursorType, K} = unpack_cursor(Cursor),
    cursors_to_edges(Input#{<<"after">> := null}, slice({'after', K}, {Lo, Hi}));
cursors_to_edges(#{ <<"before">> := Cursor } = Input, {Lo, Hi}) ->
    {_CursorType, K} = unpack_cursor(Cursor),
    cursors_to_edges(Input#{<<"before">> := null}, slice({'before', K}, {Lo, Hi})).

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
    binary_to_term(Data, [safe]).

pack_cursor(K) ->
    base64:encode(term_to_binary(K, [compressed])).

edges_sequential([], _K) -> [];
edges_sequential([Obj | Next], K) ->
    Cursor = pack_cursor({sequential, K}),
    Edge = #{ <<"node">> => Obj, <<"cursor">> => Cursor },
    [Edge | edges_sequential(Next, K+1)].

edges_ordered([], _) -> [];
edges_ordered(_, 0) -> [];
edges_ordered([{Obj, Timestamp} | Next], N) ->
    Cursor = pack_cursor({ordered, Timestamp}),
    Edge = #{ <<"node">> => Obj, <<"cursor">> => Cursor },
    [Edge | edges_ordered(Next, N-1)].



