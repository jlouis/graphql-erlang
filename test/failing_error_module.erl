-module(failing_error_module).

-export([crash/2, err/2]).

crash(_Ctx, x) ->
    #{ message => "OK" }.

err(_Ctx, x) ->
    #{ mesge => "OK" }.

