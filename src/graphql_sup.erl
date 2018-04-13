-module(graphql_sup).
-behaviour(supervisor).

%% API.
-export([start_link/0]).

%% Supervisor callbacks.
-export([init/1]).

%% From supervisor.
-type start_link_err() :: {already_started, pid()} | shutdown | term().
-type start_link_ret() :: {ok, pid()} | ignore | {error, start_link_err()}.

-spec start_link() -> start_link_ret().
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @private
init([]) ->
    DefaultEndpoint = graphql_default_endpoint,
    SchemaMgr = #{id => graphql_schema,
                  start => {graphql_schema, start_link, [DefaultEndpoint]}
    },
    {ok, {{one_for_all, 5, 3600}, [SchemaMgr]}}.
