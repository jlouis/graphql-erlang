-module(graphql_schema).
-behaviour(gen_server).

-include_lib("stdlib/include/qlc.hrl").
-include("graphql_schema.hrl").
-include("graphql_internal.hrl").

-export([start_link/0, start_link/1, reset/0, reset/1, get_endpoint_ctx/1, get_endpoint_ctx/0]).
-export([
         all/1,
         insert/3,
         load/2,
         load_schema/2,
         get/2,
         lookup/2,
         validate_enum/3,
         lookup_interface_implementors/2
        ]).
-export([resolve_root_type/2]).

-export([id/1]).

%% Callbacks
-export([init/1, handle_call/3, handle_cast/2, terminate/2, handle_info/2,
    code_change/3]).

-define(DEFAULT_ENDPOINT, default).

-type endpoint() :: atom().
-record(state, {context :: endpoint_context()}).

%% -- API ----------------------------

-spec get_endpoint_ctx() -> endpoint_context().
get_endpoint_ctx() ->
    {ok, Ep} = gen_server:call(?DEFAULT_ENDPOINT, get_endpoint_ctx),
    Ep.

-spec get_endpoint_ctx(endpoint()) -> endpoint_context().
get_endpoint_ctx(Name) ->
    {ok, Ep} = gen_server:call(Name, get_endpoint_ctx),
    Ep.

-spec start_link() -> any().
start_link() ->
    start_link(?DEFAULT_ENDPOINT).

-spec start_link(endpoint()) -> any().
start_link(Name) ->
    Res = gen_server:start_link({local, Name}, ?MODULE, [Name], []),
    Ep = get_endpoint_ctx(Name),
    reset(Ep),
    Res.

-spec reset() -> ok.
reset() ->
    Ep = get_endpoint_ctx(?DEFAULT_ENDPOINT),
    reset(Ep).

-spec reset(endpoint_context()) -> ok.
reset(Ep) ->
    ok = gen_server:call(Ep#endpoint_context.pid, reset),
    ok = graphql_introspection:inject(Ep),
    ok = graphql_builtins:standard_types_inject(Ep),
    ok = graphql_builtins:standard_directives_inject(Ep),
    ok.

-spec insert(endpoint_context(), any(), any()) ->
  ok | {error, Reason :: term()}.
insert(Ep, S, #{ canonicalize := true }) ->
    try graphql_schema_canonicalize:x(S) of
        Rec ->
            case gen_server:call(Ep#endpoint_context.pid, {insert, Rec}) of
                true -> ok;
                false ->
                    Identify = fun({_, #{ id := ID }}) -> ID end,
                    {error, {already_exists, Identify(S)}}
            end
    catch
        ?EXCEPTION(Class, Reason, Stacktrace) ->
            error_logger:error_msg(
              "Schema canonicalization error: ~p stacktrace: ~p~n",
              [{Class,Reason}, ?GET_STACK(Stacktrace)]),
            {error, {schema_canonicalize, {Class, Reason}}}
    end;
insert(Ep, S, #{}) ->
    gen_server:call(Ep#endpoint_context.pid, {insert, S}).

-spec load_schema(endpoint_context(), #root_schema{}) -> ok.
load_schema(Ep, #root_schema{ query = Q } = Root) ->
    ok = graphql_introspection:augment_root(Ep, Q),
    insert_new_(Ep, Root).

-spec load(endpoint_context(), any()) -> ok | {error, Reason}
  when Reason :: term().
load(Ep, S) ->
    try graphql_schema_canonicalize:x(S) of
        #root_schema { query = Q } = Rec ->
            ok = graphql_introspection:augment_root(Ep, Q),
            insert_new_(Ep, Rec);
        Rec ->
            insert_new_(Ep, Rec)
    catch
        Class:Reason ->
            {error, {schema_canonicalize, {Class, Reason}}}
    end.

insert_new_(#endpoint_context{pid = Pid}, Rec) ->
    case gen_server:call(Pid, {insert_new, Rec}) of
        true -> ok;
        false -> {error, {already_exists, id(Rec)}}
    end.

-spec all(endpoint_context()) -> [any()].
all(#endpoint_context{objects_tab = ObjectsTab}) ->
    ets:match_object(ObjectsTab, '_').

-spec get(endpoint_context(), binary() | 'ROOT') -> schema_object().
get(#endpoint_context{objects_tab = ObjectsTab}, ID) ->
    case ets:lookup(ObjectsTab, ID) of
       [S] -> S;
       _ -> exit(schema_not_found)
    end.

%% Check if given enum value matches the given type id, other enums,
%% or nothing at all.

-spec validate_enum(endpoint_context(), binary(), binary()) -> ok | not_found | {other_enums, [#enum_type{}]}.
validate_enum(Ep=#endpoint_context{enums_tab = EnumsTab}, EnumID, EnumValue) ->
    try ets:lookup_element(EnumsTab, EnumValue, 2) of
        #{EnumID := _} -> ok;
        EnumIDsMap ->
            EnumIDs = maps:keys(EnumIDsMap),
            OtherEnums = [get(Ep, ID) || ID <- EnumIDs],
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

-spec lookup_interface_implementors(endpoint_context(), binary()) -> [binary()].
lookup_interface_implementors(#endpoint_context{objects_tab = ObjectsTab}, IFaceID) ->
    QH = qlc:q([Obj#object_type.id
                || Obj <- ets:table(ObjectsTab),
                   element(1, Obj) == object_type,
                   lists:member(IFaceID, Obj#object_type.interfaces)]),
    qlc:e(QH).


-spec lookup(endpoint_context(), binary() | 'ROOT') -> schema_object() | not_found.
lookup(#endpoint_context{objects_tab = ObjectsTab}, ID) ->
    case ets:lookup(ObjectsTab, ID) of
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
id(#input_object_type{ id = ID }) -> ID;
id(#directive_type{ id = ID }) -> ID.

%% -- CALLBACKS

-spec init([atom()]) -> {ok, #state{}}.
init([Name]) ->
    Ep = register_schema(Name),
    {ok, #state{context = Ep}}.

-spec register_schema(endpoint()) -> endpoint_context().
register_schema(Name) ->
    EnumsTab = ets:new(graphql_objects_table,
         [protected, {read_concurrency, true}, set,
           {keypos, 1}]),
    ObjectsTab = ets:new(graphql_enums_table,
        [protected, {read_concurrency, true}, set,
         {keypos, #object_type.id}]),

    #endpoint_context{name = Name,
                      pid = self(),
                      enums_tab = EnumsTab,
                      objects_tab = ObjectsTab}.


-spec handle_cast(any(), S) -> {noreply, S}
  when S :: #state{}.
handle_cast(_Msg, State) -> {noreply, State}.

-spec handle_call(M, any(), S) -> {reply, term(), S}
  when
    S :: #state{},
    M :: term().
handle_call({insert, X}, _From, #state{context=Ep}=State) ->
    case determine_table(Ep, X) of
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
handle_call({insert_new, X}, _From, #state{context=Ep}=State) ->
    case determine_table(Ep, X) of
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
handle_call(reset, _From,
            #state{context = #endpoint_context{objects_tab = Tab}} = State) ->
    true = ets:delete_all_objects(Tab),
    {reply, ok, State};
handle_call(get_endpoint_ctx, _From, State) ->
    {reply, {ok, State#state.context}, State};
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
determine_table(#endpoint_context{objects_tab = Objs},
                #root_schema{}) -> Objs;
determine_table(#endpoint_context{objects_tab = Objs},
                #object_type{}) -> Objs;
determine_table(#endpoint_context{objects_tab = Objs, enums_tab = Enums},
                #enum_type{}) -> {enum, Objs, Enums};
determine_table(#endpoint_context{objects_tab = Objs},
                #interface_type{}) -> Objs;
determine_table(#endpoint_context{objects_tab = Objs},
                #scalar_type{}) -> Objs;
determine_table(#endpoint_context{objects_tab = Objs},
                #input_object_type{}) -> Objs;
determine_table(#endpoint_context{objects_tab = Objs},
                #union_type{}) -> Objs;
determine_table(#endpoint_context{objects_tab = Objs},
                #directive_type{}) -> Objs;
determine_table(_, _) -> {error, unknown}.

%% insert enum values
insert_enum(Tab, #enum_type { id = ID, values = VMap }) ->
    Vals = maps:values(VMap),
    [ append_enum_id(Tab, Key, ID)
      || #enum_value { val = Key } <- Vals],
    ok.

append_enum_id(Tab, Key, ID) ->
    CurrentIDs = try ets:lookup_element(Tab, Key, 2) of
        EnumIDsMap -> EnumIDsMap
    catch
        error:badarg ->
            #{}
    end,
    NewIDs = CurrentIDs#{ID => undefined},
    ets:insert(Tab, {Key, NewIDs}),
    ok.

