-module(dungeon_object).
-export([execute/4]).

execute(_Ctx, Obj, Field, _) when is_map(Obj) ->
    {ok, maps:get(Field, Obj, owl)};
execute(_Ctx, Obj, _, _) ->
    {error, {not_map_object, Obj}}.
