-module(graphql).


-include_lib("graphql/include/graphql.hrl").
-include("graphql_internal.hrl").
-include("graphql_schema.hrl").

%% GraphQL Documents
-export([
         parse/1,
         type_check/1, type_check_params/3,
         insert_root/1,
         validate/1,
         execute/1, execute/2
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
         sync/3
         ]).

%% Schema Definitions
-export([
         load_schema/2,
         insert_schema_definition/1,
         validate_schema/0,
         populate_persistent_table/0
]).

%% Internal
-export([token_ref/1]).

-type json() :: number() | binary() | true | false | null | #{ binary() | atom() => json() } | [json()] .
-type param_context() :: json().

-type schema_definition() :: {atom(), #{ atom() => term() }}.

-export_type([json/0, param_context/0]).

-type token() :: {'$graphql_token', pid(), reference(), reference()}.
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

-spec execute(document()) -> #{ atom() => json() }.
execute(AST) ->
    Ctx = #{ params => #{}, default_timeout => ?DEFAULT_TIMEOUT },
    execute(Ctx, AST).

-spec execute(context(), document()) -> #{ atom() => json() }.
execute(#{default_timeout := _DT } = Ctx, AST) ->
    graphql_execute:x(Ctx, AST);
execute(Ctx, AST) ->
    case graphql_execute:x(Ctx#{ default_timeout => ?DEFAULT_TIMEOUT}, AST) of
        #{ errors := Errs } = Result ->
            Result#{ errors := graphql_err:format_errors(Ctx, Errs) };
        Result -> Result
    end.

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
-ifdef(HAVE_PERSISTENT_TERM).
-spec validate_schema() -> ok | {error, any()}.
-else.
-spec validate_schema() -> ok.
-endif.

validate_schema() ->
    case graphql_schema:populate_persistent_table() of
        ok ->
            ignore;
        {error, Reason} ->
            error_logger:info_report([warning, {populate_persistent_table, Reason}])
    end,
    graphql_schema_validate:x().

populate_persistent_table() ->
    graphql_schema:populate_persistent_table().



