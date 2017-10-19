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

-include("graphql_internal.hrl").
-include("graphql_schema.hrl").

-export([x/1, x_params/3]).
-export([err_msg/1]).

%% -- TOP LEVEL TYPE CHECK CODE -------------------------------

%% Type checking proceeds by the ordinary way of writing a type
%% checker. First we split the input query document definitions, which
%% we call "Clauses", into its fragments and the remaining operations.
%% Next, we create an environment of fragments, so we can refer to
%% their types as we type check other things.
%%
%% Type checking then proceeds one Clause at a time.
-spec x(graphql:ast()) -> {ok, #{ atom() => any()}}.
x(Doc) ->
    x(#{}, Doc).

x(Ctx, {document, Clauses}) ->
   type_check(Ctx, [document], Clauses).

type_check(Ctx, Path, Clauses) ->
   {Fragments, _Rest} = lists:partition(
                          fun (#frag{}) -> true; (_) -> false end,
                          Clauses),

   FragCtx = Ctx#{ fragenv => mk_fragenv(Fragments) },
   NewClauses = clauses(FragCtx, Path, Clauses),
   {ok, #{
       fun_env => graphql_elaborate:mk_funenv(NewClauses),
       ast => {document, NewClauses}
   }}.

clauses(_Ctx, _Path, []) ->
    [];
clauses(Ctx, Path, [#frag{} = Frag | Next]) ->
    [frag(Ctx, Path, Frag) | clauses(Ctx, Path, Next)];
clauses(Ctx, Path, [#op{} = Op | Next]) ->
    [op(Ctx, Path, Op) | clauses(Ctx, Path, Next)].

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
get_operation(FunEnv, undefined, Params) ->
    case maps:to_list(FunEnv) of
        [] when Params == #{} ->
            undefined;
        [] when Params /= #{} ->
            err([], unnamed_operation_params);
        [{_, TyVarEnv}] ->
            TyVarEnv;
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
        TyVarEnv ->
            tc_params([OpName], TyVarEnv, Params)
    end.

%% Parameter checking has positive polarity, so we fold over
%% the type var environment from the schema and verify that each
%% type is valid
tc_params(Path, TyVarEnv, InitialParams) ->
    F =
      fun(K, V0, PS) ->
        case tc_param(Path, K, V0, maps:get(K, PS, not_found)) of
            V0 -> PS;
            V1 -> PS#{ K => V1 }
        end
      end,
    maps:fold(F, InitialParams, TyVarEnv).

%% When checking parameters, we must consider the case of default values.
%% If a given parameter is not given, and there is a default, we can supply
%% the default value in some cases. The spec requires special handling of
%% null values, which are handled here.
tc_param(Path, K, #vardef { ty = {non_null, _}, default = null }, not_found) ->
    err([K | Path], missing_non_null_param);
tc_param(_Path, _K, #vardef { default = Default }, not_found) ->
    Default;
tc_param(Path, K, #vardef { ty = Ty }, Val) ->
    check_param([K | Path], Ty, Val).

%% When checking params, the top level has been elaborated by the
%% elaborator, but the levels under it has not. So we have a case where
%% we need to look up underlying types and check them.
%%
%% This function case-splits on different types of positive polarity and
%% calls out to the correct helper-function
check_param(Path, {non_null, Ty}, V) -> check_param(Path, Ty, V);
check_param(Path, #scalar_type{} = STy, V) -> non_polar_coerce(Path, STy, V);
check_param(Path, #enum_type{} = ETy, {enum, V}) when is_binary(V) ->
    check_param(Path, ETy, V);
check_param(Path, #enum_type { id = Ty }, V) when is_binary(V) ->
    %% Determine the type of any enum term, and then coerce it
    case graphql_schema:lookup_enum_type(V) of
        #enum_type { id = Ty } = ETy ->
            non_polar_coerce(Path, ETy, V);
        not_found ->
            err(Path, {enum_not_found, Ty, V});
        OtherTy ->
            err(Path, {param_mismatch, {enum, Ty, OtherTy}})
    end;
check_param(Path, {list, T}, L) when is_list(L) ->
    %% Build a dummy structure to match the recursor. Unwrap this
    %% structure before replacing the list parameter.
    [check_param(Path, T, X) || X <- L];
check_param(Path, #input_object_type{} = IOType, Obj) when is_map(Obj) ->
    check_input_object(Path, IOType, Obj);
%% The following expands un-elaborated (nested) types
check_param(Path, Ty, V) when is_binary(Ty) ->
    case graphql_schema:lookup(Ty) of
        #scalar_type {} = ScalarTy -> non_polar_coerce(Path, ScalarTy, V);
        #input_object_type {} = IOType -> check_input_object(Path, IOType, V);
        #enum_type {} = Enum -> check_param(Path, Enum, V);
        _ ->
            err(Path, {not_input_type, Ty, V})
    end;
%% Everything else are errors
check_param(Path, Ty, V) ->
    err(Path, {param_mismatch, Ty, V}).

%% Input objects are first coerced. Then they are checked.
check_input_object(Path, #input_object_type{ fields = Fields }, Obj) ->
    Coerced = coerce_input_object(Obj),
    check_input_object_fields(Path, maps:to_list(Fields), Coerced, #{}).

%% Input objects are in positive polarity, so the schema's fields are used
%% to verify that every field is present, and that there are no excess fields
%% As we process fields in the object, we remove them so we can check that
%% there are no more fields in the end.
check_input_object_fields(Path, [], Obj, Result) ->
    case maps:size(Obj) of
        0 -> Result;
        K when K > 0 -> err(Path, {excess_fields_in_object, Obj})
    end;
check_input_object_fields(Path,
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
                                 Default
                         end;
                     V ->
                         check_param([Name | Path], Ty, V)
                 end,
    check_input_object_fields(Path,
                              Next,
                              maps:remove(Name, Obj),
                              Result#{ Name => CoercedVal }).

%% Handle non-polar inputs
non_polar_coerce(_Path,
                 #enum_type { resolve_module = undefined }, Value) ->
    {ok, Value};
non_polar_coerce(Path,
                 #enum_type { id = ID,
                              resolve_module = ResolveModule }, Value) ->
    resolve_input_coercion(Path, ID, ResolveModule, Value);
non_polar_coerce(Path,
                 #scalar_type { id = ID,
                                resolve_module = ResolveModule }, Value) ->
    resolve_input_coercion(Path, ID, ResolveModule, Value).

resolve_input_coercion(Path, ID, ResolveModule, Value) ->
    try ResolveModule:input(ID, Value) of
        {ok, NewVal} -> NewVal;
        {error, Reason} -> graphql_err:abort(Path, {input_coercion, ID, Value, Reason})
    catch
        Cl:Err ->
            error_report({input_coercer, ID, Value}, Cl, Err),
            err(Path, {input_coerce_abort, {Cl, Err}})
    end.

%% -- FRAGMENTS --------------------------------

%% Generate a map from fragment names to fragments for use as a
%% fragment environment.
mk_fragenv(Frags) ->
    maps:from_list(
      [{graphql_ast:name(ID), Frg}
       || #frag { id = ID} = Frg <- Frags]).

%% Type check a fragment
frag(Ctx, Path, #frag {selection_set = SSet} = Frag) ->
    Frag#frag { selection_set = sset(Ctx, Path, SSet) }.

%% -- OPERATIONS -------------------------------

%% Type check an operation.
op(Ctx, Path, #op { vardefs = VDefs, selection_set = SSet} = Op) ->
    VarEnv = graphql_elaborate:mk_varenv(VDefs),
    Op#op { selection_set = sset(Ctx#{ varenv => VarEnv }, [id(Op) | Path], SSet) }.

%% -- SELECTION SETS ------------------------------------

%% Type check a selection set by recursing into each field in the selection
sset(Ctx, Path, SSet) ->
    [field(Ctx, Path, S) || S <- SSet].

%% Fields are either fragment spreads, inline fragments, introspection
%% queries or true field entries. Split on the variant of field and
%% handle each accordingly.
%%
%% Handling fields themselves is a congruence over its
%% (sub-)selection-set.
%%
%% The key is to type-check directives and eventual arguments to
%% fields in this recursion.
%%
%% Since fields have negative polarity, we only consider type checking
%% of the fields which the client requested. Every other field is
%% ignored.
field(#{ fragenv := FE } = Ctx, Path, #frag_spread { id = ID, directives = Ds } = FSpread) ->
    Name = graphql_ast:name(ID),
    case maps:get(Name, FE, not_found) of
        not_found ->
            err([Name | Path], unknown_fragment);
        _FragTy ->
            %% You can always include a fragspread, as long as it exists
            %% It may be slightly illegal in a given context but this just
            %% means the system will ignore the fragment on execution
            FSpread#frag_spread { directives = directives(Ctx, Path, Ds) }
    end;
field(Ctx, Path, #frag { id = '...', selection_set = SSet, directives = Ds} = InlineFrag) ->

    InlineFrag#frag {
        directives = directives(Ctx, [InlineFrag | Path], Ds),
        selection_set = sset(Ctx, [InlineFrag | Path], SSet)
    };
field(Ctx, Path, #field { schema = {introspection, typename}, directives = Ds } = F) ->
    F#field { directives = directives(Ctx, [F | Path], Ds)};
field(Ctx, Path, #field { args = Args,
                          selection_set = SSet,
                          directives = Ds,
                          schema = #schema_field { args = SArgs }} = F) ->
    F#field { args = args(Ctx, [F | Path], Args, SArgs),
              directives = directives(Ctx, [F | Path], Ds),
              selection_set = sset(Ctx, [F | Path], SSet) }.

%% -- DIRECTIVES --------------------------------

%% Type check directives. These can take arguments, so type checking
%% these amounts to type checking the arguments inside the directive.
directives(Ctx, Path, Ds) ->
    [directive(Ctx, Path, D) || D <- Ds].

directive(Ctx, Path,
          #directive { args = Args,
                       schema = #directive_type { args = SArgs }} = D) ->
    D#directive { args = args(Ctx, [D | Path], Args, SArgs) }.

%% -- ARGS -------------------------------------

%% Type checking of arguments to fields, directives, ...
%%
%% Arguments must be unique.
%%
%% Since arguments have positive polarity, they are checked according
%% to the schema arguments.
args(Ctx, Path, Args, Schema) ->
    NamedArgs = [{graphql_ast:name(K), V} || {K, V} <- Args],
    case uniq(lists:sort(NamedArgs)) of
        ok ->
          SchemaArgs = maps:to_list(Schema),
          args(Ctx, Path, NamedArgs, SchemaArgs, []);
        {not_unique, X} ->
            err(Path, {not_unique, X})
    end.

%% The meat of the argument checker. Walk over each schema arg and
%% verify it type checks according to the type checking rules.
args(_Ctx, _Path, [], [], Acc) -> Acc;
args(_Ctx, Path, [_|_] = Args, [], _Acc) ->
    err(Path, {excess_args, Args});
args(Ctx, Path, Args, [{Name, #schema_arg { ty = STy }} = SArg | Next], Acc) ->
    case take_arg(Args, SArg) of
        {error, Reason} ->
            err([Name | Path], Reason);
        {ok, {_, #{ type := Ty, value := Val}} = A, NextArgs} ->
            ValueType = value_type(Ctx, Path, Ty, Val),
            SchemaType =
                case graphql_elaborate:type(STy) of
                    {error, Reason} -> exit(Reason);
                    {_Polarity, SchemaTypeRsult} -> SchemaTypeRsult
                end,
            case refl(Path, ValueType, SchemaType) of
                {error, Expected} ->
                    err(Path, {type_mismatch,
                               #{ id => Name, schema => Expected }});
                {error, Got, Expected} ->
                    err(Path, {type_mismatch,
                               #{ id => Name, document => Got, schema => Expected }});
                ok ->
                    args(Ctx, Path, NextArgs, Next, [A | Acc]);
                Val ->
                    args(Ctx, Path, NextArgs, Next, [A | Acc]);
                RVal ->
                    args(Ctx, Path, NextArgs, Next, [{Name, {Ty, RVal}} | Acc])
            end
    end.

%% Search a list of arguments for the next argument. Handle non-null
%% values correctly as we are conducting the search. Return both the
%% arg found and the remaining set of arguments so we can eventually
%% check if we exhausted the full set.
take_arg(Args, {Key, #schema_arg { ty = {non_null, _}, default = null}}) ->
    case lists:keytake(Key, 1, Args) of
        false ->
            {error, missing_non_null_param};
        {value, Arg, NextArgs} ->
            {ok, Arg, NextArgs}
    end;
take_arg(Args, {Key, #schema_arg { ty = Ty, default = Default }}) ->
    case lists:keytake(Key, 1, Args) of
        false ->
            {ok, {Key, #{ type => Ty, value => Default }}, Args};
        {value, Arg, NextArgs} ->
            {ok, Arg, NextArgs}
    end.

%% Determine if an association list has unique keys
-spec uniq([{term(), term()}]) -> ok | {not_unique, term()}.
uniq([]) -> ok;
uniq([_]) -> ok;
uniq([{X, _}, {X, _} | _]) -> {not_unique, X};
uniq([_ | Next]) -> uniq(Next).

value_type(Ctx, Path, {non_null, Ty}, V) ->
    {non_null, value_type(Ctx, Path, Ty, V)};
value_type(Ctx, Path, {list, Ty}, Vs) when is_list(Vs) ->
    {list, [{value_type(Ctx, Path, Ty, V), V} || V <- Vs]};
value_type(#{ varenv := VE }, Path, _, {var, ID}) ->
    Name = graphql_ast:name(ID),
    case maps:get(Name, VE, not_found) of
        not_found -> err(Path, {unbound_variable, Name});
        #vardef { ty = Ty } -> Ty
    end;
value_type(_Ctx, _Path, _, null) -> null;
value_type(_Ctx, _Path, #enum_type{} = Ty, {enum, _N}) ->
    Ty;
value_type(_Ctx, Path, _, {enum, N}) ->
    case graphql_schema:lookup_enum_type(N) of
        not_found -> err(Path, {unknown_enum, N});
        #enum_type {} = Enum -> Enum
    end;
value_type(_Ctx, Path, {scalar, Tag}, V) ->
    case valid_scalar_value(V) of
        true -> {scalar, Tag, V};
        false ->
            err(Path, {invalid_scalar_value, V})
    end;
value_type(_Ctx, _Path, _, {name, N, _}) -> N;
value_type(_Ctx, _Path, _, S) when is_binary(S) -> {scalar, string, S};
value_type(_Ctx, _Path, _, I) when is_integer(I) -> {scalar, int, I};
value_type(_Ctx, _Path, _, F) when is_float(F) -> {scalar, float, F};
value_type(_Ctx, _Path, _, true) -> {scalar, bool, true};
value_type(_Ctx, _Path, _, false) -> {scalar, bool, false};
value_type(_Ctx, _Path, _, Obj) when is_map(Obj) -> coerce_input_object(Obj);
value_type(_Ctx, Path, Ty, Val) ->
    err(Path, {invalid_value_type_coercion, Ty, Val}).

refl_list(_Path, [], _T, Result) ->
    lists:reverse(Result);
refl_list(Path, [{A, V}|As], T, Acc) ->
    case refl(Path, A, T) of
        {error, _Reason} ->
            {error, {list, T}};
        ok ->
            refl_list(Path, As, T, [V|Acc]);
        V ->
            refl_list(Path, As, T, [V|Acc]);
        RV ->
            refl_list(Path, As, T, [RV|Acc])
   end.

%% EQ Match:
refl(_Path, X, X) -> ok;
%% Compound:
refl(Path, {list, As}, {list, T}) ->
    refl_list(Path, As, T, []);
refl(Path, {non_null, ValueType}, {non_null, SchemaType}) ->
    refl(Path, ValueType, SchemaType);
refl(_Path, kull, {non_null, _}) -> {error, non_null};
refl(_Path, null, _T) -> ok;
refl(Path, {non_null, A}, T) -> refl(Path, A, T);
refl(Path, A, {non_null, T}) -> refl(Path, A, T);
%% Ground:
refl(Path, {scalar, Tag, V}, #scalar_type { id = ID } = SType) ->
    case Tag of
        string -> non_polar_coerce(Path, SType, V);
        int -> non_polar_coerce(Path, SType, V);
        float -> non_polar_coerce(Path, SType, V);
        bool -> non_polar_coerce(Path, SType, V);
        ID -> non_polar_coerce(Path, SType, V)
    end;
refl(_Path, #input_object_type { id = ID },
            {input_object, #input_object_type { id = ID }}) ->
    ok;
refl(Path, Obj, #input_object_type{} = Ty) when is_map(Obj) ->
    check_input_object(Path, Ty, Obj);
%% Failure:
refl(_Path, A, T) -> {error, A, T}.

coerce_input_object(Obj) when is_map(Obj) ->
    coerce_object_(Obj).

coerce_object_(Obj) when is_map(Obj) ->
    Elems = maps:to_list(Obj),
    maps:from_list([{coerce_name(K), coerce_object_(V)} || {K, V} <- Elems]);
coerce_object_(Other) -> Other.

coerce_name(B) when is_binary(B) -> B;
coerce_name(Name) -> graphql_ast:name(Name).

%% -- AST MANIPULATION -------------------------

%% True if input is a scalar value
valid_scalar_value(S) when is_binary(S) -> true;
valid_scalar_value(F) when is_float(F) -> true;
valid_scalar_value(I) when is_integer(I) -> true;
valid_scalar_value(true) -> true;
valid_scalar_value(false) -> true;
valid_scalar_value(_) -> false.

id(#op { id = ID }) -> ID.

%% -- Error handling -------------------------------------

-spec err([term()], term()) -> no_return().
err(Path, Msg) ->
    graphql_err:abort(Path, type_check, Msg).

err_msg(unnamed_operation_params) ->
    ["Cannot supply parameter lists to unnamed (anonymous) queries"];
err_msg({operation_not_found, Op}) ->
    ["Expected an operation ", Op, " but no such operation was found"];
err_msg(missing_non_null_param) ->
    ["The parameter is non-null, but was undefined in parameter list"];
err_msg({enum_not_found, Ty, Val}) ->
    X = io_lib:format("The value ~p is not a valid enum value for type ", [Val]),
    [X, graphql_err:format_ty(Ty)];
err_msg({param_mismatch, {enum, Ty, OtherTy}}) ->
    ["The enum value is of type ", graphql_err:format_ty(OtherTy),
     " but used in a context where an enum value"
     " of type ", graphql_err:format_ty(Ty), " was expected"];
err_msg({param_mismatch, Ty, V}) ->
    io_lib:format("The parameter value ~p is not of type ~p", [V, graphql_err:format_ty(Ty)]);
err_msg({not_input_type, Ty, _}) ->
    ["The type ", graphql_err:format_ty(Ty), " is a valid input type"];
err_msg({excess_fields_in_object, Fields}) ->
    io_lib:format("The object contains unknown fields and values: ~p", [Fields]);
err_msg({excess_args, Args}) ->
    io_lib:format("The argument list contains unknown arguments ~p", [Args]);
err_msg({type_mismatch, #{ id := ID, document := Doc, schema := Sch }}) ->
    ["Type mismatch on (", ID, "). The query document has a value/variable of type (",
      graphql_err:format_ty(Doc), ") but the schema expects type (", graphql_err:format_ty(Sch), ")"];
err_msg({type_mismatch, #{ schema := Sch }}) ->
    ["Type mismatch, expected (", graphql_err:format_ty(Sch), ")"];
err_msg({input_coercion, Type, Value, Reason}) ->
    io_lib:format("Input coercion failed for type ~s with value ~p. The reason it failed is: ~p", [Type, Value, Reason]);
err_msg({input_coerce_abort, _}) ->
    ["Input coercer failed due to an internal server error"];
err_msg(unknown_fragment) ->
    ["The referenced fragment name is not present in the query document"];
err_msg({not_unique, X}) ->
    ["The name ", X, " occurs more than once"];
err_msg({unbound_variable, Var}) ->
    ["The document refers to a variable ", Var,
     " but no such var exists. Perhaps the variable is a typo?"];
err_msg({unknown_enum, E}) ->
    ["The enum name ", E, " is not present in the schema"];
err_msg({invalid_scalar_value, V}) ->
    io_lib:format("The value ~p is not a valid scalar value", [V]);
err_msg({invalid_value_type_coercion, Ty, Val}) ->
    io_lib:format(
      "The value ~p cannot be coerced into the type ~p",
      [Val, Ty]).

%% Tell the error logger that something is off
error_report(Term, Cl, Err) ->
  error_logger:error_report(
    [
     Term,
     {error, Cl, Err},
     {stack, erlang:get_stacktrace()}
    ]).
