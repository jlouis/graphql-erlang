%% Type Definition Language
-record(directive,
        { id :: graphql:name(),
          args = [] :: #{ binary() => term() } | [{any(), any()}],
          schema :: any(),
          resolve_module :: atom()
        }).

-define(LAZY(X), {'$lazy', fun() -> X end}).

%% Only for not found behaviours
-define(RETURN_NULL, {ok, null}).
-define(RETURN_EMPTY_LIST, {ok, []}).
