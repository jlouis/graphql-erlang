%% Type Definition Language
-type name() :: {name, pos_integer(), binary()} | binary().

-record(directive,
        { id :: name(),
          args = [] :: [any()],
          schema :: any()
        }).
-type directive() :: #directive{}.

-define(LAZY(X), {'$lazy', fun() -> X end}).

%% Only for not found behaviours
-define(RETURN_NULL, {ok, null}).
-define(RETURN_EMPTY_LIST, {ok, []}).
