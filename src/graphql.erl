-module(graphql).

-include_lib("graphql/include/graphql.hrl").
-include("graphql_internal.hrl").
-include("graphql_schema.hrl").

-compile({no_auto_import, [monitor/2]}).

%% GraphQL Documents
-export([
         parse/1,
         type_check/1, type_check_params/3,
         insert_root/1,
         validate/1,
         execute/1, execute/2,
         handle_subscription_event/4
        ]).

-export([
         format_errors/2
        ]).

%% Early exit
-export([
         throw/1
        ]).

%% Deferred execution
-export([
         token/1, reply_cast/2,
         sync/3,
         monitor/2,
         map/2
         ]).

%% Schema Definitions
-export([
         load_schema/2,
         insert_schema_definition/1,
         validate_schema/0
]).

%% Internal
-export([token_ref/1]).

-type json() :: number() | binary() | true | false | null | #{ binary() | atom() => json() } | [json()] .
-type param_context() :: json().

-type error() :: #{message := binary(),
                   path => [binary()],
                   extensions => map()}.
-type execute_result() :: #{data => graphql:json(),
                            subscription => {subscription_value(), subscription_ctx()},
                            errors => [error()],
                            aux => [any()]}.

-type schema_definition() :: {atom(), #{ atom() => term() }}.

-export_type([json/0, param_context/0, execute_result/0, error/0]).

-type token() :: {'$graphql_token', pid(), reference(), reference()}.
-type defer_map() :: #{ worker => pid(),
                        timeout => non_neg_integer(),
                        apply => [fun()]}.
-type result() :: {ok, term()} | {error, term()} | {defer, token()} | {defer, token(), defer_map()}.
-type name() :: {name, pos_integer(), binary()} | binary().
-type document() :: #document{}.
-type directive() :: #directive{}.

-type subscription_value() :: any().
-type subscription_ctx() :: graphql_execute:subscription_ctx().

-export_type([directive/0,
              token/0,
              schema_field/0,
              subscription_value/0,
              subscription_ctx/0]).

-define(DEFAULT_TIMEOUT, 3000).

%% EARLY EXIT
%% --------------------------------------------------------------------------
throw(Msg) ->
    erlang:throw({'$graphql_throw', Msg}).


%% TOKENS
%% --------------------------------------------------------------------------
-spec token(#{ defer_process := pid(),
               defer_request_id := reference() }) ->
                   token().
token(#{ defer_process := Proc, defer_request_id := ReqId }) ->
    {'$graphql_token', Proc, ReqId, make_ref()}.

sync(#{ defer_process := Proc, defer_request_id := ReqId }, Pid, Msg) ->
    Proc ! {'$graphql_sync', ReqId, Pid, Msg}.

-spec monitor(pid(), result()) -> result().
monitor(_Worker, {ok, Value})                            -> {ok, Value};
monitor(_Worker, {error, Reason})                        -> {error, Reason};
monitor(Worker, {defer, Token})                          -> monitor(Worker, {defer, Token, #{}});
monitor(Worker, {defer, Token, Map}) when is_pid(Worker) -> {defer, Token, Map#{ worker => Worker}}.

map(F, {ok, Value})                          -> F({ok, Value});
map(F, {error, Reason})                      -> F({error, Reason});
map(F, {defer, Token})                       -> map(F, {defer, Token, #{}});
map(F, {defer, Token, #{ apply := App} = M}) -> {defer, Token, M#{ apply := queue:in(F, App)}};
map(F, {defer, Token, #{} = M})              -> {defer, Token, M#{ apply => queue:in(F, queue:new())}}.

%% @private
token_ref({'$graphql_token', _, _, Ref}) -> Ref.

-spec reply_cast(token(), term()) -> ok.
reply_cast({'$graphql_token', Target, Id, Ref}, Data) ->
    Target ! {'$graphql_reply', Id, Ref, Data},
    ok.

%% ERRORS
%% --------------------------------------------------------------------------
lift_list(L) when is_list(L) -> L;
lift_list(X) -> [X].

format_errors(Ctx, Errs) ->
    graphql_err:format_errors(Ctx, lift_list(Errs)).

%% --------------------------------------------------------------------------
-spec parse( binary() | string()) ->
                   {ok, document()} | {error, {scanner_error | parser_error, term()}}.
parse(Input) when is_binary(Input) -> parse(binary_to_list(Input));
parse(Input) when is_list(Input) ->
    case graphql_scanner:string(Input) of
        {ok, Tokens, _EndLine} ->
            case graphql_parser:parse(Tokens) of
                {ok, Res} ->
                    {ok, Res};
                {error, Err} ->
                    {error, {parser_error, Err}}
            end;
        {error, Err, _EndLine} ->
            {error, {scanner_error, Err}}
    end.

load_schema(Mapping, Input) when is_binary(Input) ->
    load_schema(Mapping, binary_to_list(Input));
load_schema(Mapping, Input) when is_list(Input) ->
    case graphql_scanner:string(Input) of
        {ok, Tokens, _EndLine} ->
            case graphql_parser:parse(Tokens) of
                {ok, _} = Result ->
                    graphql_schema_parse:inject(Mapping, Result);
                {error, Err} ->
                    {error, Err}
            end;
        {error, Err, _EndLine} ->
            {error, Err}
    end.

-spec validate(document()) -> ok | {error, term()}.
validate(AST) ->
    graphql_validate:x(AST).

-spec type_check(document()) -> {ok, #{ atom() => term() }}.
type_check(AST) ->
    graphql_check:check(AST).

-spec type_check_params(any(), any(), any()) -> param_context().
type_check_params(FunEnv, OpName, Vars) ->
    graphql_check:check_params(FunEnv, OpName, Vars).

-spec execute(document()) -> execute_result().
execute(AST) ->
    Ctx = #{ params => #{}, default_timeout => ?DEFAULT_TIMEOUT },
    execute(Ctx, AST).

-spec execute(context(), document()) -> execute_result().
execute(#{default_timeout := _DT } = Ctx, AST) ->
    graphql_execute:x(Ctx, AST);
execute(Ctx, AST) ->
    case graphql_execute:x(Ctx#{ default_timeout => ?DEFAULT_TIMEOUT}, AST) of
        #{ errors := Errs } = Result ->
            Result#{ errors := graphql_err:format_errors(Ctx, Errs) };
        Result -> Result
    end.

%% @doc Executes subscription MapSourceToResponseEvent step
%% `subscription_value()' and `subscription_ctx()' are the values returned as
%% `#{subscription := {SubValue, SubCtx}} = graphql:execute(...)'
%% A tuple `{Subscription, Event}' will be passed as 2nd argument to the `execute/4' callback
%%
%% @param ExtraCtx additional map fields that needs to be added to execution context
%% @param Event arbitrary event that was produced by the subscription
-spec handle_subscription_event(context(), subscription_value(), subscription_ctx(), any()) ->
          execute_result().
handle_subscription_event(ExtraCtx, Subscription, SubscriptionCtx, Event) ->
    graphql_execute:x_subscription_event(ExtraCtx, Subscription, SubscriptionCtx, Event).

%% @doc insert_schema_definition/1 loads a schema definition into the Graph Schema
%% @end
-spec insert_schema_definition(schema_definition()) -> ok | {error, Reason}
  when Reason :: term().
insert_schema_definition(Defn) ->
    graphql_schema:load(Defn).

-spec insert_root(schema_definition()) -> ok.
insert_root(Defn) ->
    Root = graphql_schema_canonicalize:x(Defn),
    Schema = graphql_schema_validate:root(Root),
    graphql_schema:load_schema(Schema).

%% STUB for now
-spec validate_schema() -> ok | {error, any()}.
validate_schema() ->
    graphql_schema_validate:x().
