-module(graphql_sup).
-behaviour(supervisor).

%% API.
-export([start_link/0]).

%% Supervisor callbacks.
-export([init/1]).

%% From supervisor.
-type start_link_err() :: {already_started, pid()} | shutdown | term().
-type start_link_ret() :: {ok, pid()} | ignore | {error, start_link_err()}.

-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

-spec start_link() -> start_link_ret().
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @private
init([]) ->
    SchemaMgr = ?CHILD(graphql_schema, worker),
    {ok, {{one_for_all, 5, 3600}, [SchemaMgr]}}.
