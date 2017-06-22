%%% Test Helper module
-module(th).
-include_lib("common_test/include/ct.hrl").
-export([
         x/2, x/3, x/4,
         errors/1,
         no_errors/1,
         v/1
]).

x(Config, Input) -> x(Config, Input, #{}).

x(Config, Input, Params) -> x(Config, Input, undefined, Params).

x(Config, Input, OpName, Params) ->
    ct:log("Query: ~s", [Input]),
    Track0 = track_new(),
    case graphql:parse(Input) of
        {ok, AST} ->
           Track1 = track(parse, Track0),
           ct:log("AST: ~p", [AST]),
           ct:log("Params: ~p", [Params]),
           try
               Elaborated = graphql:elaborate(AST),
               ct:log("Elaborated: ~p", [Elaborated]),
               Track2 = track(elaborate, Track1),
               {ok, #{
                   ast := AST2,
                   fun_env := FunEnv }} = graphql:type_check(Elaborated),
               Track3 = track(type_check, Track2),
               CoercedParams = graphql:type_check_params(FunEnv, OpName, Params),
               Track4 = track(type_check_params, Track3),
               Ctx = #{ params => CoercedParams },
               ok = graphql:validate(AST2),
               Track5 = track(validate, Track4),
               Res = case OpName of
                         undefined -> graphql:execute(Ctx, AST2);
                         Op -> graphql:execute(Ctx#{ operation_name => Op }, AST2)
                     end,
               
               Track6 = track(execute, Track5),
               ct:log("Result: ~p", [Res]),
               track_report(Config, Track6),
               Res
            catch
                throw:{error, Error} -> #{ errors => Error }
            end;
         Err ->
             Err
    end.

errors(#{ errors := [] }) -> ct:fail(no_errors_present);
errors(#{ errors := _}) -> ok;
errors(#{ }) -> ct:fail(no_errors_present).

no_errors(#{ errors := [] }) -> ok;
no_errors(#{ errors := Es }) ->
    ct:log("Errors: ~p", [Es]),
    ct:fail(errors_present);
no_errors(#{}) -> ok.

%% v/1 predicates valid queries
v(Q) ->
    ct:log("Query: ~s", [Q]),
    case graphql:parse(Q) of
        {ok, AST} ->
            try
               Elab = graphql:elaborate(AST),
               {ok, #{
                   ast := _AST2,
                   fun_env := _FunEnv }} = graphql:type_check(Elab),
               ok = graphql:validate(AST),
               true
            catch
                Class:Err ->
                  ct:log("Error: ~p", [{Class, Err, erlang:get_stacktrace()}]),
                  false
            end;
        Err ->
            ct:log("Error: ~p", [Err]),
            false
    end.

%track_ets() ->
%    ets:new(graphql_SUITE_track, [named_table, public, {keypos, 1}]),
%    ok.

track_new() ->
    T = erlang:monotonic_time(),
    #{ '$last' => T }.
    
track(Event, #{ '$last' := Start } = M) ->
    End = erlang:monotonic_time(),
    Diff = erlang:convert_time_unit(End - Start, native, micro_seconds),
    M#{ '$last' := End, Event => Diff}.

track_report(Config, M) ->
    Name = proplists:get_value(name, ?config(tc_group_properties, Config)),
    ct:log("Timings ~p: ~p", [Name, maps:remove('$last', M)]).

