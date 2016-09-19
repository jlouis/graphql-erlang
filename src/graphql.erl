-module(graphql).

-include("graphql_internal.hrl").

-export([
    elaborate/1,
    execute/1, execute/2,
    parse/1,
    type_check/1, type_check_params/3,
    validate/1]).

-export([
    load/1,
    validate_schema/0
]).

-type json() :: number() | binary() | true | false | null | #{ binary() => json() }.
-type param_context() :: json().

-type schema_definition() :: {atom(), #{ binary() => term() }}.

-export_type([ast/0, json/0, param_context/0]).

-type schema_field() :: #{ atom() => any() }.
-export_type([schema_field/0]).

-spec parse( binary() | string()) -> {ok, ast()} | {error, term()}.
parse(Input) when is_binary(Input) -> parse(binary_to_list(Input));
parse(Input) when is_list(Input) ->
    case graphql_scanner:string(Input) of
        {ok, Tokens, _EndLine} ->
            graphql_parser:parse(Tokens);
        ErrorInfo ->
            ErrorInfo
    end.

-spec validate(ast()) -> ok.
validate(AST) ->
    graphql_validate:x(AST).
    
-spec type_check(ast()) -> {ok, #{ atom() => term() }}.
type_check(AST) ->
    graphql_type_check:x(AST).

-spec elaborate(ast()) -> ast().
elaborate(AST) ->
   graphql_elaborate:x(AST).

-spec type_check_params(any(), any(), any()) -> param_context().
type_check_params(FunEnv, OpName, Vars) ->
    graphql_type_check:x_params(FunEnv, OpName, Vars).

-spec execute(ast()) -> #{ atom() => json() }.
execute(AST) -> execute(#{ params => #{} }, AST).

-spec execute(context(), ast()) -> #{ atom() => json() }.
execute(Ctx, AST) ->
    graphql_execute:x(Ctx, AST).

%% @doc load/1 loads a schema definition into the Graph Schema
%% @end
-spec load(schema_definition()) -> 'ok' | {error, term()}.
load(Defn) ->
    graphql_schema:load(Defn).

%% STUB for now
-spec validate_schema() -> ok | {error, any()}.
validate_schema() ->
    graphql_schema_validate:x().

