%%% @doc Type checking of GraphQL query documents
%%%
%%% The type checker carries out three tasks:
%%%
%%% Make sure that types check. That is, the user supplies a well
%%% typed query document.
%%%
%%% Make sure that types are properly inferred. Some times, the types
%%% the user supply needs an inference pass in order to figure out
%%% what the user supplied. The type-checker also infers the possible
%%% types and makes sure the query document is well typed.
%%%
%%% Handle coercion for the constant fragment of a query document.
%%% Whenever a coercible constant value is encountered in a query
%%% document, or a coercible parameter occurs in a parameter, the type
%%% checker runs "input coercion" which is part canonicalization, part
%%% input validation on input data. The coerved value is expanded into
%%% the query document or parameter string, so the execution engine
%%% always works with coerced data. This choice is somewhat peculiar,
%%% but it serves an optimization purpose since we only have to carry
%%% out a coercion once for a query with constant values.
%%%
%%% Polarity:
%%%
%%% This type checker mentions polarity of types. There are 3 kinds of
%%% polarity: positive, negative and non-polar. The flows of these are
%%% that Client -> Server is positive and Server -> Client is
%%% negative. Non-polar types flows both ways. Since the server engine
%%% doesn't trust the client, type checking follows some polarity
%%% rules. If we check a positive polarity context, we don't trust the
%%% client and we use the schema data to verify that everything is
%%% covered by the client in a valid way. If we check in negative
%%% polarity context, we are the server and can trust things are
%%% correct. So we fold over the query document when considering if
%%% types are correct. Non-polar values fall naturally in both
%%% contexts.
%%%
%%% @end
-module(graphql_type_check).

-include_lib("graphql/include/graphql.hrl").
-include("graphql_internal.hrl").
-include("graphql_schema.hrl").

-export([x_params/3]).

%% -- TYPE CHECK OF PARAMETER ENVS ------------------

%% GraphQL queries are really given in two stages. One stage is the
%% query document containing (static) queries which take parameters.
%% These queries can be seen as functions (stored procedures) you can
%% call.
%%
%% If called, we get a function name, and a set of parameters for that
%% function. So we have to go through the concrete parameters and type
%% check them against the function environment type schema. If the
%% input parameters can not be coerced into the parameters expected by
%% the function scheme, and error occurs.

%% Determine the operation in the query which is the one to execute
get_operation(FunEnv, <<>>, Params) ->
    %% Supplying an empty string is the same as not supplying anything at all
    %% This should solve problems where we have empty requests
    get_operation(FunEnv, undefined, Params);
get_operation(FunEnv, undefined, Params) ->
    case maps:to_list(FunEnv) of
        [] when Params == #{} ->
            undefined;
        [] when Params /= #{} ->
            err([], unnamed_operation_params);
        [{_, VarEnv}] ->
            VarEnv;
        _ ->
            %% The error here should happen in the execute phase
            undefined
    end;
get_operation(FunEnv, OpName, _Params) ->
    maps:get(OpName, FunEnv, not_found).

%% This is the entry-point when checking parameters for an already parsed,
%% type checked and internalized query. It serves to verify that a requested
%% operation and its parameters matches the types in the operation referenced
-spec x_params(any(), any(), any()) -> graphql:param_context().
x_params(FunEnv, OpName, Params) ->
    case get_operation(FunEnv, OpName, Params) of
        undefined ->
            #{};
        not_found ->
            err([], {operation_not_found, OpName});
        VarEnv ->
            tc_params([OpName], VarEnv, Params)
    end.

%% Parameter checking has positive polarity, so we fold over
%% the type var environment from the schema and verify that each
%% type is valid
tc_params(Path, VarEnv, InitialParams) ->
    F =
      fun(K, V0, PS) ->
        case tc_param(Path, VarEnv, K, V0, maps:get(K, PS, not_found)) of
            V0 -> PS;
            V1 -> PS#{ K => V1 }
        end
      end,
    maps:fold(F, InitialParams, VarEnv).

%% When checking parameters, we must consider the case of default values.
%% If a given parameter is not given, and there is a default, we can supply
%% the default value in some cases. The spec requires special handling of
%% null values, which are handled here.
tc_param(Path, _, K, #vardef { ty = {non_null, _}, default = null }, not_found) ->
    err([K | Path], missing_non_null_param);
tc_param(Path, VarEnv, K, #vardef { default = Default,
                               ty = Ty }, not_found) ->
    coerce_default_param([K | Path], VarEnv, Ty, Default);
tc_param(Path, VarEnv, K, #vardef { ty = Ty }, Val) ->
    check_param([K | Path], VarEnv, Ty, Val).

%% When checking params, the top level has been elaborated by the
%% elaborator, but the levels under it has not. So we have a case where
%% we need to look up underlying types and check them.
%%
%% This function case-splits on different types of positive polarity and
%% calls out to the correct helper-function

%% The following expands un-elaborated (nested) types
check_param(Path, VarEnv, Ty, V) when is_binary(Ty) ->
    ResolvedType = case graphql_schema:lookup(Ty) of
                       #scalar_type {} = T -> T;
                       #input_object_type {} = T -> T;
                       #enum_type {} = T -> T;
                       _ ->
                           err(Path, {not_input_type, Ty, V})
                   end,
    check_param(Path, VarEnv, ResolvedType, V);
%% Handle the case of a variable
check_param(Path, _, {non_null, _}, null) ->
    err(Path, non_null);
check_param(Path, VarEnv, {non_null, Ty}, V) ->
    check_param(Path, VarEnv, Ty, V);
check_param(_Path, _, _Ty, null) ->
    null;
check_param(Path, VarEnv, {list, T}, L) when is_list(L) ->
    %% Build a dummy structure to match the recursor. Unwrap this
    %% structure before replacing the list parameter.
    [check_param(Path, VarEnv, T, X) || X <- L];
check_param(Path, _, #scalar_type{} = STy, V) ->
    non_polar_coerce(Path, STy, V);
check_param(Path, VarEnv, #enum_type{} = ETy, {enum, V}) when is_binary(V) ->
    check_param(Path, VarEnv, ETy, V);
check_param(Path, _, #enum_type { id = Ty } = ETy, V) when is_binary(V) ->
    %% Determine the type of any enum term, and then coerce it
    case graphql_schema:validate_enum(Ty, V) of
        ok ->
            non_polar_coerce(Path, ETy, V);
        not_found ->
            err(Path, {enum_not_found, Ty, V});
        {other_enums, OtherTys} ->
            err(Path, {param_mismatch, {enum, Ty, OtherTys}})
    end;
check_param(Path, VarEnv, #input_object_type{} = IOType, Obj) when is_map(Obj) ->
    %% When an object comes in through JSON for example, then the input object
    %% will be a map which is already unique in its fields. To handle this, turn
    %% the object into the same form as the one we use on query documents and pass
    %% it on. Note that the code will create a map later on once the input has been
    %% uniqueness-checked.
    check_param(Path, VarEnv, IOType, {input_object, maps:to_list(Obj)});
check_param(Path, VarEnv, #input_object_type{} = IOType, {input_object, KVPairs}) ->
    check_input_object(Path, VarEnv, IOType, {input_object, KVPairs});
%% Everything else are errors
check_param(Path, _, Ty, V) ->
    err(Path, {param_mismatch, Ty, V}).

coerce_default_param(Path, VarEnv, Ty, Default) ->
    try check_param(Path, VarEnv, Ty, Default) of
        Result -> Result
    catch
        Class:Err ->
            error_logger:error_report(
              [{path, graphql_err:path(lists:reverse(Path))},
               {default_value, Default},
               {type, graphql_err:format_ty(Ty)},
               {default_coercer_error, Class, Err}]),
            err(Path, non_coercible_default)
    end.

%% Input objects are first coerced. Then they are checked.
check_input_object(Path, VarEnv, #input_object_type{ fields = Fields }, Obj) ->
    Coerced = coerce_input_object(Path, Obj),
    check_input_object_fields(Path, VarEnv, maps:to_list(Fields), Coerced, #{}).

%% Input objects are in positive polarity, so the schema's fields are used
%% to verify that every field is present, and that there are no excess fields
%% As we process fields in the object, we remove them so we can check that
%% there are no more fields in the end.
check_input_object_fields(Path, _, [], Obj, Result) ->
    case maps:size(Obj) of
        0 -> Result;
        K when K > 0 -> err(Path, {excess_fields_in_object, Obj})
    end;
check_input_object_fields(Path,
                          VarEnv, 
                          [{Name, #schema_arg { ty = Ty,
                                                default = Default }} | Next],
                          Obj,
                          Result) ->
    CoercedVal = case maps:get(Name, Obj, not_found) of
                     not_found ->
                         case Ty of
                             {non_null, _} when Default == null ->
                                 err([Name | Path], missing_non_null_param);
                             _ ->
                                 coerce_default_param(Path, VarEnv, Ty, Default)
                         end;
                     V ->
                         check_param([Name | Path], VarEnv, Ty, V)
                 end,
    check_input_object_fields(Path,
                              VarEnv,
                              Next,
                              maps:remove(Name, Obj),
                              Result#{ Name => CoercedVal }).

coerce_name(B) when is_binary(B) -> B;
coerce_name(Name) -> graphql_ast:name(Name).

coerce_input_object(Path, {input_object, Elems}) ->
    AssocList = [begin
                     N = coerce_name(K),
                     {N, coerce_input_object([N | Path], V)}
                 end || {K, V} <- Elems],
    case graphql_ast:uniq(AssocList) of
        ok ->
            maps:from_list(AssocList);
        {not_unique, Key} ->
            err(Path, {input_object_not_unique, Key})
    end;
coerce_input_object(_Path, Value) -> Value.

%% -- Error handling -------------------------------------

-spec err([term()], term()) -> no_return().
err(Path, Msg) ->
    graphql_err:abort(Path, type_check, Msg).

