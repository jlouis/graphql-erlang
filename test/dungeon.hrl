
-record(sequence, {
          id :: atom(),
          count :: pos_integer()
         }).

-record(stats, {
          shell_scripting = 3 :: pos_integer(),
          attack = 3 :: pos_integer(),
          yell = <<"HELO">> :: binary()
         }).

-record(monster, {
          id :: pos_integer(),
          name :: binary(),
          color :: unknown:unknown(),
          inventory = ordsets:new() :: ordsets:ordset(pos_integer()),
          stats = [] :: [#stats{}],
          hitpoints :: integer(),
          plush_factor = 0.0 :: float(),
          properties = [] :: [binary()],
          mood :: binary()
         }).

-record(item, {
          id :: pos_integer(),
          name :: binary(),
          description :: binary(),
          weight :: float(),
          contents :: [pos_integer()]
         }).

-record(room, {
          id :: pos_integer(),
          description :: binary(),
          contents = ordsets:new() :: ordsets:ordset(pos_integer())
         }).
