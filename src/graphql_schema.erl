-module(graphql_schema).
-behaviour(gen_server).

-include_lib("stdlib/include/qlc.hrl").
-include("graphql_schema.hrl").
-include("graphql_internal.hrl").

-export([start_link/0, reset/0]).
-export([
         all/0,
         insert/1, insert/2,
         load/1,
         get/1,
         lookup/1,
         validate_enum/2,
         lookup_interface_implementors/1
        ]).
-export([resolve_root_type/2]).

-export([id/1]).

%% Callbacks
-export([init/1, handle_call/3, handle_cast/2, terminate/2, handle_info/2,
    code_change/3]).

-define(ENUMS, graphql_schema_enums).     % Table mapping enum values of type binary() to set of enum type ids
-define(OBJECTS, graphql_schema_objects). % Table of all types

-record(state, {}).

%% -- API ----------------------------
-spec start_link() -> any().
start_link() ->
    Res = gen_server:start_link({local, ?MODULE}, ?MODULE, [], []),
    reset(),
    Res.

-spec reset() -> ok.
reset() ->
    ok = gen_server:call(?MODULE, reset),
    ok = graphql_introspection:inject(),
    ok = graphql_builtins:standard_types_inject(),
    ok.

-spec insert(any()) -> ok.
insert(S) ->
    insert(S, #{ canonicalize => true }).

-spec insert(any(), any()) ->
  ok | {error, Reason :: term()}.
insert(S, #{ canonicalize := true }) ->
    try graphql_schema_canonicalize:x(S) of
        Rec ->
            case gen_server:call(?MODULE, {insert, Rec}) of
                true -> ok;
                false ->
                    Identify = fun({_, #{ id := ID }}) -> ID end,
                    {error, {already_exists, Identify(S)}}
            end
    catch
        Class:Reason ->
            error_logger:error_msg(
              "Schema canonicalization error: ~p stacktrace: ~p~n",
              [{Class,Reason}, erlang:get_stacktrace()]),
            {error, {schema_canonicalize, {Class, Reason}}}
    end;
insert(S, #{}) ->
    gen_server:call(?MODULE, {insert, S}).


-spec load(any()) -> ok | {error, Reason}
  when Reason :: term().
load(S) ->
    try graphql_schema_canonicalize:x(S) of
        #root_schema { query = Q } = Rec ->
            ok = graphql_introspection:augment_root(Q),
            insert_new_(Rec);
        Rec ->
            insert_new_(Rec)
    catch
        Class:Reason ->
            {error, {schema_canonicalize, {Class, Reason}}}
    end.

insert_new_(Rec) ->
    case gen_server:call(?MODULE, {insert_new, Rec}) of
        true -> ok;
        false -> {error, {already_exists, id(Rec)}}
    end.

-spec all() -> [any()].
all() ->
    ets:match_object(?OBJECTS, '_').

-spec get(binary() | 'ROOT') -> schema_object().
get(ID) ->
    case ets:lookup(?OBJECTS, ID) of
       [S] -> S;
       _ -> exit(schema_not_found)
    end.

%% Check if given enum value matches the given type id, other enums, or nothing at all.
-spec validate_enum(binary(), binary()) -> ok | not_found | {other_enums, [#enum_type{}]}.
validate_enum(EnumID, EnumValue) ->
    try ets:lookup_element(?ENUMS, EnumValue, 2) of
        #{EnumID := _} -> ok;
        EnumIDsMap ->
            EnumIDs = maps:keys(EnumIDsMap),
            OtherEnums = [?MODULE:get(ID) || ID <- EnumIDs],
            {other_enums, OtherEnums}
    catch
        error:badarg ->
            not_found
    end.

%% Find the implementors of a given interface. If this proves to be
%% too slow in practice, one can build an index in the schema over these
%% and use an index lookup instead. It should be fairly simple to do.
%%
%% However, in the spirit of getting something up and running, we start
%% with QLC in order to make a working system.
-spec lookup_interface_implementors(binary()) -> [binary()].
lookup_interface_implementors(IFaceID) ->
    QH = qlc:q([Obj#object_type.id
                || Obj <- ets:table(?OBJECTS),
                   element(1, Obj) == object_type,
                   lists:member(IFaceID, Obj#object_type.interfaces)]),
    qlc:e(QH).

-spec lookup(binary() | 'ROOT') -> schema_object() | not_found.
lookup(ID) ->
    case ets:lookup(?OBJECTS, ID) of
       [S] -> S;
       _ -> not_found
    end.

-spec resolve_root_type(undefined | operation_type(), root_schema()) -> undefined | binary().
resolve_root_type(undefined, #root_schema { query = Q }) -> Q;
resolve_root_type({query, _}, #root_schema { query = Q }) -> Q;
resolve_root_type({mutation, _}, #root_schema { mutation = M }) -> M;
resolve_root_type({subscription, _}, #root_schema { subscription = S }) -> S.

id(#root_schema{}) -> 'ROOT';
id(#scalar_type{ id = ID }) -> ID;
id(#object_type{ id = ID}) -> ID;
id(#enum_type{ id = ID}) -> ID;
id(#interface_type{ id = ID}) -> ID;
id(#union_type{ id = ID}) -> ID;
id(#input_object_type{ id = ID }) -> ID.

%% -- CALLBACKS

-spec init([]) -> {ok, #state{}}.
init([]) ->
    _Tab1 = ets:new(?ENUMS,
         [named_table, protected, {read_concurrency, true}, set,
           {keypos, 1}]),
    _Tab = ets:new(?OBJECTS,
        [named_table, protected, {read_concurrency, true}, set,
         {keypos, #object_type.id}]),
    {ok, #state{}}.

-spec handle_cast(any(), S) -> {noreply, S}
  when S :: #state{}.
handle_cast(_Msg, State) -> {noreply, State}.

-spec handle_call(M, any(), S) -> {reply, term(), S}
  when
    S :: #state{},
    M :: term().
handle_call({insert, X}, _From, State) ->
    case determine_table(X) of
        {error, unknown} ->
            {reply, {error, {schema, X}}, State};
        {enum, Tab, Enum} ->
            ets:insert(Tab, X),
            insert_enum(Enum, X),
            {reply, ok, State};
        Tab ->
            true = ets:insert(Tab, X),
            {reply, ok, State}
    end;
handle_call({insert_new, X}, _From, State) ->
    case determine_table(X) of
        {error, unknown} ->
            {reply, {error, {schema, X}}, State};
        {enum, Tab, Enum} ->
            case ets:insert_new(Tab, X) of
                false ->
                   {reply, false, State};
                true ->
                   insert_enum(Enum, X),
                   {reply, true, State}
            end;
        Tab ->
            {reply, ets:insert_new(Tab, X), State}
    end;
handle_call(reset, _From, State) ->
    true = ets:delete_all_objects(?OBJECTS),
    {reply, ok, State};
handle_call(_Msg, _From, State) ->
    {reply, {error, unknown_call}, State}.

-spec handle_info(term(), S) -> {noreply, S}
  when S :: #state{}.
handle_info(_Msg, State) -> {noreply, State}.

-spec terminate(any(), any()) -> any().
terminate(_, _) -> ok.

-spec code_change(term(), S, term()) -> {ok, S}
  when S :: #state{}.
code_change(_OldVsn, State, _Aux) -> {ok, State}.

%% -- INTERNAL FUNCTIONS -------------------------

%% determine_table/1 figures out the table an object belongs to
determine_table(#root_schema{}) -> ?OBJECTS;
determine_table(#object_type{}) -> ?OBJECTS;
determine_table(#enum_type{}) -> {enum, ?OBJECTS, ?ENUMS};
determine_table(#interface_type{}) -> ?OBJECTS;
determine_table(#scalar_type{}) -> ?OBJECTS;
determine_table(#input_object_type{}) -> ?OBJECTS;
determine_table(#union_type{}) -> ?OBJECTS;
determine_table(_) -> {error, unknown}.

%% insert enum values
insert_enum(Tab, #enum_type { id = ID, values = VMap }) ->
    Vals = maps:values(VMap),
    [ append_enum_id(Tab, Key, ID)
      || #enum_value { val = Key } <- Vals],
    ok.

append_enum_id(Tab, Key, ID) ->
    CurrentIDs = try ets:lookup_element(?ENUMS, Key, 2) of
        EnumIDsMap -> EnumIDsMap
    catch
        error:badarg ->
            #{}
    end,
    NewIDs = CurrentIDs#{ID => undefined},
    ets:insert(Tab, {Key, NewIDs}),
    ok.

