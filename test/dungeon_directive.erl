-module (dungeon_directive).

-include_lib("graphql/include/graphql.hrl").

-export ([execute/6]).

execute(Ctx, Obj, Field, Args, #directive{id = <<"alwaysError">>, args = #{<<"reason">> := Reason}}, Resolver) ->
    ct:pal("alwaysError directive executed on field ~p at object ~p ", [Field, Obj]),
    {error, {always_error_directive, Reason}};
execute(Ctx, Obj, Field, Args, _Directive, Resolver) ->
    Resolver(Ctx, Obj, Field, Args).