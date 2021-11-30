-module(graphql).

-include_lib("graphql/include/graphql.hrl").
-include("graphql_internal.hrl").
-include("graphql_schema.hrl").

-compile({no_auto_import, [monitor/2]}).

%% GraphQL Documents
-export([
         parse/1,
         type_check/1, type_check/2,
         type_check_params/3, type_check_params/4,
         insert_root/1, insert_root/2,
         validate/1,
         execute/1, execute/2, execute/3,
         get_endpoint/1
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
         load_schema/2, load_schema/3,
         insert_schema_definition/1, insert_schema_definition/2,
         validate_schema/0, validate_schema/1
]).

%% Internal
-export([token_ref/1]).

-type json() :: number() | binary() | true | false | null | #{ binary() | atom() => json() } | [json()] .
-type param_context() :: json().

-type schema_definition() :: {atom(), #{ atom() => term() }}.

-export_type([json/0, param_context/0, endpoint_context/0]).

-type token() :: {'$graphql_token', pid(), reference(), reference()}.
-type defer_map() :: #{ worker => pid(),
                        timeout => non_neg_integer(),
                        apply => [fun()]}.
-type result() :: {ok, term()} | {error, term()} | {defer, token()} | {defer, token(), defer_map()}.
-type name() :: {name, pos_integer(), binary()} | binary().
-type document() :: #document{}.
-type directive() :: #directive{}.
-export_type([directive/0,

              token/0,
              schema_field/0]).

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

load_schema(Mapping, Input) ->
    Ep = graphql_schema:get_endpoint_ctx(),
    load_schema(Ep, Mapping, Input).

load_schema(Ep, Mapping, Input) when is_binary(Input) ->
    load_schema(Ep, Mapping, binary_to_list(Input));
load_schema(Ep, Mapping, Input) when is_list(Input) ->
    case graphql_scanner:string(Input) of
        {ok, Tokens, _EndLine} ->
            case graphql_parser:parse(Tokens) of
                {ok, _} = Result ->
                    graphql_schema_parse:inject(Ep, Mapping, Result);
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
    Ep = graphql_schema:get_endpoint_ctx(),
    type_check(Ep, AST).

-spec type_check(endpoint_context(), document()) -> {ok, #{ atom() => term() }}.
type_check(Ep, AST) ->
    graphql_check:check(Ep, AST).


-spec type_check_params(any(), any(), any()) -> param_context().
type_check_params(FunEnv, OpName, Vars) ->
    Ep = graphql_schema:get_endpoint_ctx(),
    type_check_params(Ep, FunEnv, OpName, Vars).

-spec type_check_params(endpoint_context(), any(), any(), any()) -> param_context().
type_check_params(Ep, FunEnv, OpName, Vars) ->
    graphql_check:check_params(Ep, FunEnv, OpName, Vars).

-spec execute(document()) -> #{ atom() => json() }.
execute(AST) ->
    Ctx = #{ params => #{}, default_timeout => ?DEFAULT_TIMEOUT },
    execute(Ctx, AST).

-spec execute(context(), document()) -> #{ atom() => json() }.
execute(Ctx, AST) ->
    Ep = graphql_schema:get_endpoint_ctx(),
    execute(Ep, Ctx, AST).

-spec execute(endpoint_context(), context(), document()) -> #{ atom() => json() }.
execute(Ep, #{default_timeout := _DT } = Ctx, AST) ->
    graphql_execute:x(Ep, Ctx, AST);
execute(Ep, Ctx, AST) ->
    case graphql_execute:x(Ep, Ctx#{ default_timeout => ?DEFAULT_TIMEOUT}, AST) of
        #{ errors := Errs } = Result ->
            Result#{ errors := graphql_err:format_errors(Ctx, Errs) };
        Result -> Result
    end.

-spec get_endpoint(atom()) -> endpoint_context().
get_endpoint(Name) ->
    graphql_schema:get_endpoint_ctx(Name).

%% @doc insert_schema_definition/1 loads a schema definition into the Graph Schema
%% @end
-spec insert_schema_definition(schema_definition()) -> ok | {error, Reason}
  when Reason :: term().
insert_schema_definition(Defn) ->
    Ep = graphql_schema:get_endpoint_ctx(),
    insert_schema_definition(Ep, Defn).

-spec insert_schema_definition(endpoint_context(), schema_definition()) -> ok | {error, Reason}
  when Reason :: term().
insert_schema_definition(Ep, Defn) ->
    graphql_schema:load(Ep, Defn).

-spec insert_root(schema_definition()) -> ok.
insert_root(Defn) ->
    Ep = graphql_schema:get_endpoint_ctx(),
    insert_root(Ep, Defn).

-spec insert_root(endpoint_context(), schema_definition()) -> ok.
insert_root(Ep, Defn) ->
    Root = graphql_schema_canonicalize:x(Defn),
    Schema = graphql_schema_validate:root(Ep, Root),
    graphql_schema:load_schema(Ep, Schema).

%% STUB for now
-spec validate_schema() -> ok | {error, any()}.
validate_schema() ->
    Ep = graphql_schema:get_endpoint_ctx(),
    graphql_schema_validate:x(Ep).

-spec validate_schema(endpoint_context()) -> ok | {error, any()}.
validate_schema(Ep) ->
    graphql_schema_validate:x(Ep).
