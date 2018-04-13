-module(graphql).

-include("graphql_internal.hrl").

%% GraphQL Documents
-export([
         parse/1,
         elaborate/1, p_elaborate/2, ep_elaborate/2,
         type_check/1, p_type_check/2, ep_type_check/2,
         type_check_params/3, p_type_check_params/4, ep_type_check_params/4,
         validate/1,
         execute/1, p_execute/2, ep_execute/2,
         execute/2, p_execute/3, ep_execute/3
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
         token/1, reply_cast/2
         ]).

%% Schema Definitions
-export([
         load_schema/2, p_load_schema/3, ep_load_schema/3,
         insert_schema_definition/1, p_insert_schema_definition/2, ep_insert_schema_definition/2,
         validate_schema/0, p_validate_schema/1, ep_validate_schema/1
]).

-export([
    get_endpoint/1,
    default_endpoint/0
]).

%% Internal
-export([token_ref/1]).

-type json() :: number() | binary() | true | false | null | #{ binary() | atom() => json() } | [json()] .
-type param_context() :: json().

-type schema_definition() :: {atom(), #{ atom() => term() }}.

-export_type([ast/0, json/0, param_context/0]).

-type token() :: {'$graphql_token', pid(), reference()}.
-export_type([token/0]).

-type schema_field() :: #{ atom() => any() }.
-export_type([schema_field/0]).

-define(DEFAULT_TIMEOUT, 750).


%% Endpoints
-type server_ref() :: pid() | atom().
-type endpoint_context() :: graphql_schema:endpoint_context().





%% EARLY EXIT
%% --------------------------------------------------------------------------
throw(Msg) ->
    erlang:throw({'$graphql_throw', Msg}).


%% TOKENS
%% --------------------------------------------------------------------------
token(#{ defer_process := Proc, defer_request_id := ReqId }) ->
    {'$graphql_token', Proc, ReqId, make_ref()}.

%% @private
token_ref({'$graphql_token', _, _, Ref}) -> Ref.

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
                   {ok, ast()} | {error, {scanner_error | parser_error, term()}}.
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



%% Endpoint API
%% All calls require an endpoint_context() which can be acquired with
%% get_endpoint(server_ref()) and default_endpoint().


ep_load_schema(EP, Mapping, Input) when is_binary(Input) ->
    ep_load_schema(EP, Mapping, binary_to_list(Input));
ep_load_schema(EP, Mapping, Input) when is_list(Input) ->
    case graphql_scanner:string(Input) of
        {ok, Tokens, _EndLine} ->
            case graphql_parser:parse(Tokens) of
                {ok, _} = Result ->
                    graphql_schema_parse:inject(EP, Mapping, Result);
                {error, Err} ->
                    {error, Err}
            end;
        {error, Err, _EndLine} ->
            {error, Err}
    end.

-spec ep_type_check(endpoint_context(), ast()) -> {ok, #{ atom() => term() }}.
ep_type_check(EP, AST) ->
    graphql_type_check:x(EP, AST).

-spec ep_elaborate(endpoint_context(), ast()) -> ast().
ep_elaborate(EP, AST) ->
    graphql_elaborate:x(EP, AST).

-spec ep_type_check_params(endpoint_context(), any(), any(), any()) -> param_context().
ep_type_check_params(EP, FunEnv, OpName, Vars) ->
    graphql_type_check:x_params(EP, FunEnv, OpName, Vars).

-spec ep_execute(endpoint_context(), ast()) -> #{ atom() => json() }.
ep_execute(EP, AST) ->
    Ctx = #{ params => #{}, default_timeout => ?DEFAULT_TIMEOUT },
    ep_execute(EP, Ctx, AST).

-spec ep_execute(endpoint_context(), context(), ast()) -> #{ atom() => json() }.
ep_execute(EP, Ctx, AST) ->
    case graphql_execute:x(EP, Ctx#{ default_timeout => ?DEFAULT_TIMEOUT}, AST) of
        #{ errors := Errs } = Result ->
            Result#{ errors := graphql_err:format_errors(Ctx, Errs) };
        Result -> Result
    end.

%% @doc insert_schema_definition/1 loads a schema definition into the Graph Schema
%% @end
-spec ep_insert_schema_definition(endpoint_context(), schema_definition()) -> ok | {error, Reason}
                                  when Reason :: term().
ep_insert_schema_definition(EP, Defn) ->
    graphql_schema:load(EP, Defn).

%% STUB for now
-spec ep_validate_schema(endpoint_context()) -> ok | {error, any()}.
ep_validate_schema(EP) ->
    graphql_schema_validate:x(EP).





%% Pid API
%% All calls require a pid()/atom() that refers to the schema gen_server.

-spec get_endpoint(server_ref()) -> endpoint_context().
get_endpoint(P) -> graphql_schema:get_endpoint(P).


p_load_schema(P, Mapping, Input) -> ep_load_schema(get_endpoint(P), Mapping, Input).

-spec p_type_check(server_ref(), ast()) -> {ok, #{ atom() => term() }}.
p_type_check(P, AST) -> ep_type_check(get_endpoint(P), AST).

-spec p_elaborate(server_ref(), ast()) -> ast().
p_elaborate(P, AST) -> ep_elaborate(get_endpoint(P), AST).

-spec p_type_check_params(server_ref(), any(), any(), any()) -> param_context().
p_type_check_params(P, FunEnv, OpName, Vars) -> ep_type_check_params(get_endpoint(P), FunEnv, OpName, Vars).

-spec p_execute(server_ref(), ast()) -> #{ atom() => json() }.
p_execute(P, AST) -> ep_execute(get_endpoint(P), AST).

-spec p_execute(server_ref(), context(), ast()) -> #{ atom() => json() }.
p_execute(P, Ctx, AST) -> ep_execute(get_endpoint(P), Ctx, AST).

%% @doc insert_schema_definition/1 loads a schema definition into the Graph Schema
%% @end
-spec p_insert_schema_definition(server_ref(), schema_definition()) -> ok | {error, Reason}
                                     when Reason :: term().
p_insert_schema_definition(P, Defn) -> ep_insert_schema_definition(get_endpoint(P), Defn).

%% STUB for now
-spec p_validate_schema(server_ref()) -> ok | {error, any()}.
p_validate_schema(P) -> ep_validate_schema(get_endpoint(P)).


%% Default Endpoint API
%% All calls implicitly refer to the application default endpoint.

-spec default_endpoint() -> endpoint_context().
default_endpoint() -> get_endpoint(graphql_default_endpoint).


load_schema(Mapping, Input) -> ep_load_schema(default_endpoint(), Mapping, Input).

-spec type_check(ast()) -> {ok, #{ atom() => term() }}.
type_check(AST) -> ep_type_check(default_endpoint(), AST).

-spec elaborate(ast()) -> ast().
elaborate(AST) -> ep_elaborate(default_endpoint(), AST).

-spec type_check_params(any(), any(), any()) -> param_context().
type_check_params(FunEnv, OpName, Vars) -> ep_type_check_params(default_endpoint(), FunEnv, OpName, Vars).

-spec execute(ast()) -> #{ atom() => json() }.
execute(AST) -> ep_execute(default_endpoint(), AST).

-spec execute(context(), ast()) -> #{ atom() => json() }.
execute(Ctx, AST) -> ep_execute(default_endpoint(), Ctx, AST).

%% @doc insert_schema_definition/1 loads a schema definition into the Graph Schema
%% @end
-spec insert_schema_definition(schema_definition()) -> ok | {error, Reason}
                                     when Reason :: term().
insert_schema_definition(Defn) -> ep_insert_schema_definition(default_endpoint(), Defn).

%% STUB for now
-spec validate_schema() -> ok | {error, any()}.
validate_schema() -> ep_validate_schema(default_endpoint()).



%% Schema Independent API
%% Use in conjunction with any of the above API variants.

-spec validate(ast()) -> ok | {error, term()}.
validate(AST) ->
    graphql_validate:x(AST).

