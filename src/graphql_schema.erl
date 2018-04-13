-module(graphql_schema).
-behaviour(gen_server).

-include_lib("stdlib/include/qlc.hrl").
-include("graphql_schema.hrl").
-include("graphql_internal.hrl").

-export([start_link/1, reset/0, reset/1]).
-export([
         get_endpoint/1,
         all/1,
         insert/2, insert/3,
         load/2,
         get/2,
         lookup/2,
         lookup_enum_type/2,
         lookup_interface_implementors/2
        ]).
-export([resolve_root_type/2]).

-export([id/1]).

%% Callbacks
-export([init/1, handle_call/3, handle_cast/2, terminate/2, handle_info/2,
    code_change/3]).

%% Miscellaneous
-record(endpoint_context, {
    pid       :: pid(),
    enumTab   :: ets:tid(),
    objectTab :: ets:tid()
}).


-record(state, {
    endpoint_context :: #endpoint_context{}
}).

%% -- API ----------------------------

-type server_ref() :: pid() | atom().
-type endpoint_context() :: #endpoint_context{}.


-spec start_link(atom()) -> any().
start_link(EPName) ->
    Res = {ok, Pid} =
          case EPName of
              undefined -> gen_server:start_link(?MODULE, [], []);
              _Other -> gen_server:start_link({local, EPName}, ?MODULE, [], [])
          end,
    EP = get_endpoint(Pid),
    reset(EP),
    Res.

-spec reset() -> ok.
reset() -> reset(graphql:default_endpoint()).

-spec reset(endpoint_context()) -> ok.
reset(EP = #endpoint_context{pid=Pid}) ->
    ok = gen_server:call(Pid, reset),
    ok = graphql_introspection:inject(EP),
    ok = graphql_builtins:standard_types_inject(EP),
    ok.



-spec get_endpoint(server_ref()) -> endpoint_context().
get_endpoint(P) ->
    gen_server:call(P, get_endpoint_context).

-spec insert(endpoint_context(), any()) -> true.
insert(EP, S) -> insert(EP, S, #{ canonicalize => true }).

-spec insert(endpoint_context(), any(), any()) -> true | false.
insert(_EP = #endpoint_context{pid=Pid}, S, #{ canonicalize := true }) ->
    try graphql_schema_canonicalize:x(S) of
        Rec ->
            case gen_server:call(Pid, {insert, Rec}) of
                true -> ok;
                false ->
                    Identify = fun({_, #{ id := ID }}) -> ID end,
                    {error, already_exists, Identify(S)}
            end
    catch
        Class:Reason ->
            error_logger:error_msg(
              "Schema canonicalization error: ~p stacktrace: ~p~n",
              [{Class,Reason}, erlang:get_stacktrace()]),
            {error, {schema_canonicalize, {Class, Reason}}}
    end;
insert(_EP = #endpoint_context{pid=Pid}, S, #{}) ->
    gen_server:call(Pid, {insert, S}).


-spec load(endpoint_context(), any()) -> ok | {error, Reason}
  when Reason :: term().
load(EP = #endpoint_context{pid=Pid}, S) ->
    try graphql_schema_canonicalize:x(S) of
        #root_schema { query = Q } = Rec ->
            ok = graphql_introspection:augment_root(EP, Q),
            insert_new_(Pid, Rec);
        Rec ->
            insert_new_(Pid, Rec)
    catch
        Class:Reason ->
            {error, {schema_canonicalize, {Class, Reason}}}
    end.

insert_new_(Pid, Rec) ->
    case gen_server:call(Pid, {insert_new, Rec}) of
        true -> ok;
        false -> {error, already_exists, id(Rec)}
    end.

-spec all(endpoint_context()) -> [any()].
all(#endpoint_context{objectTab = Tab}) ->
    ets:match_object(Tab, '_').

-spec get(endpoint_context(), binary() | 'ROOT') -> schema_object().
get(#endpoint_context{objectTab = Tab}, ID) ->
    case ets:lookup(Tab, ID) of
       [S] -> S;
       _ -> exit(schema_not_found)
    end.

-spec lookup_enum_type(endpoint_context(), binary()) -> binary() | not_found.
lookup_enum_type(EP=#endpoint_context{enumTab = Tab}, EnumValue) ->
    try ets:lookup_element(Tab, EnumValue, 3) of
        Ty -> ?MODULE:get(EP, Ty)
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
-spec lookup_interface_implementors(endpoint_context(), binary()) -> [binary()].
lookup_interface_implementors(#endpoint_context{objectTab = Tab}, IFaceID) ->
    QH = qlc:q([Obj#object_type.id
                || Obj <- ets:table(Tab),
                   element(1, Obj) == object_type,
                   lists:member(IFaceID, Obj#object_type.interfaces)]),
    qlc:e(QH).

-spec lookup(endpoint_context(), binary() | 'ROOT') -> schema_object() | not_found.
lookup(#endpoint_context{objectTab = Tab}, ID) ->
    case ets:lookup(Tab, ID) of
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
    EndpointContext = #endpoint_context{
        pid = self(),
        enumTab = ets:new(graphql_enum_table,
                          [protected, {read_concurrency, true}, set,
                           {keypos, 1}]),

        objectTab = ets:new(graphql_object_table,
                            [protected, {read_concurrency, true}, set,
                             {keypos, #object_type.id}])
    },
    {ok, #state{endpoint_context = EndpointContext}}.

-spec handle_cast(any(), S) -> {noreply, S}
  when S :: #state{}.
handle_cast(_Msg, State) -> {noreply, State}.

-spec handle_call(M, any(), S) -> {reply, term(), S}
  when
    S :: #state{},
    M :: term().
handle_call(get_endpoint_context, _From, State=#state{endpoint_context = EP}) ->
    {reply, EP, State};
handle_call({insert, X}, _From, State=#state{endpoint_context = EP}) ->
    case determine_table(EP, X) of
        {error, unknown} ->
            {reply, {error, {schema, X}}, State};
        {enum, Tab, Enum} ->
            ets:insert(Tab, X),
            insert_enum(Enum, X),
            {reply, true, State};
        Tab ->
            {reply, ets:insert(Tab, X), State}
    end;
handle_call({insert_new, X}, _From, State=#state{endpoint_context = EP}) ->
    case determine_table(EP, X) of
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
handle_call(reset, _From, State = #state{endpoint_context = #endpoint_context{objectTab = Tab}}) ->
    true = ets:delete_all_objects(Tab),
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
determine_table(#endpoint_context{objectTab = Tab}, #root_schema{}) -> Tab;
determine_table(#endpoint_context{objectTab = Tab}, #object_type{}) -> Tab;
determine_table(#endpoint_context{objectTab = ObjectTab, enumTab = EnumTab}, #enum_type{}) -> {enum, ObjectTab, EnumTab};
determine_table(#endpoint_context{objectTab = Tab}, #interface_type{}) -> Tab;
determine_table(#endpoint_context{objectTab = Tab}, #scalar_type{}) -> Tab;
determine_table(#endpoint_context{objectTab = Tab}, #input_object_type{}) -> Tab;
determine_table(#endpoint_context{objectTab = Tab}, #union_type{}) -> Tab;
determine_table(_, _) -> {error, unknown}.

%% insert enum values
insert_enum(Tab, #enum_type { id = ID, values = VMap }) ->
    Vals = maps:to_list(VMap),
    [begin
        ets:insert(Tab, {Key, Value, ID})
      end || {Value, #enum_value { val = Key }} <- Vals],
    ok.
