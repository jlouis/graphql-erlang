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

-export([x/2, x_params/4]).
%% -- TOP LEVEL TYPE CHECK CODE -------------------------------

%% Type checking proceeds by the ordinary way of writing a type
%% checker. First we split the input query document definitions, which
%% we call "Clauses", into its fragments and the remaining operations.
%% Next, we create an environment of fragments, so we can refer to
%% their types as we type check other things.
%%
%% Type checking then proceeds one Clause at a time.
-spec x(graphql_schema:endpoint_context(), graphql:ast()) -> {ok, #{ atom() => any()}}.
x(EP, Doc) ->
    x(EP, #{}, Doc).

x(EP, Ctx, {document, Clauses}) ->
   EPCtx = Ctx#{endpoint_context => EP}, 
   type_check(EPCtx, [document], Clauses).

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
-spec x_params(graphql_schema:endpoint_context(), any(), any(), any()) -> graphql:param_context().
x_params(EP, FunEnv, OpName, Params) ->
    case get_operation(FunEnv, OpName, Params) of
        undefined ->
            #{};
        not_found ->
            err([], {operation_not_found, OpName});
        VarEnv ->
            tc_params(EP, [OpName], VarEnv, Params)
    end.

%% Parameter checking has positive polarity, so we fold over
%% the type var environment from the schema and verify that each
%% type is valid
tc_params(EP, Path, VarEnv, InitialParams) ->
    F =
      fun(K, V0, PS) ->
        case tc_param(EP, Path, VarEnv, K, V0, maps:get(K, PS, not_found)) of
            V0 -> PS;
            V1 -> PS#{ K => V1 }
        end
      end,
    maps:fold(F, InitialParams, VarEnv).

%% When checking parameters, we must consider the case of default values.
%% If a given parameter is not given, and there is a default, we can supply
%% the default value in some cases. The spec requires special handling of
%% null values, which are handled here.
tc_param(_EP, Path, _, K, #vardef { ty = {non_null, _}, default = null }, not_found) ->
    err([K | Path], missing_non_null_param);
tc_param(EP, Path, VarEnv, K, #vardef { default = Default,
                               ty = Ty }, not_found) ->
    coerce_default_param(EP, [K | Path], VarEnv, Ty, Default);
tc_param(EP, Path, VarEnv, K, #vardef { ty = Ty }, Val) ->
    check_param(EP, [K | Path], VarEnv, Ty, Val).

%% When checking params, the top level has been elaborated by the
%% elaborator, but the levels under it has not. So we have a case where
%% we need to look up underlying types and check them.
%%
%% This function case-splits on different types of positive polarity and
%% calls out to the correct helper-function

%% The following expands un-elaborated (nested) types
check_param(EP, Path, VarEnv, Ty, V) when is_binary(Ty) ->
    ResolvedType = case graphql_schema:lookup(EP, Ty) of
                       #scalar_type {} = T -> T;
                       #input_object_type {} = T -> T;
                       #enum_type {} = T -> T;
                       _ ->
                           err(Path, {not_input_type, Ty, V})
                   end,
    check_param(EP, Path, VarEnv, ResolvedType, V);
%% Handle the case of a variable
check_param(_EP, Path, VarEnv, SType, {var, ID}) ->
    Var = graphql_ast:name(ID),
    case maps:get(Var, VarEnv, not_found) of
        not_found ->
            err(Path, {unknown_var, Var});
        #vardef { ty = DType } ->
            case type_embed(DType, SType) of
                yes ->
                    %% Use the schema type here
                    {var, ID, DType};
                no ->
                    err(Path, {type_mismatch,
                               #{ document => {var, Var, DType},
                                  schema => SType }})
            end
    end;
check_param(_EP, Path, _, {non_null, _}, null) ->
    err(Path, non_null);
check_param(EP, Path, VarEnv, {non_null, Ty}, V) ->
    check_param(EP, Path, VarEnv, Ty, V);
check_param(_EP, _Path, _, _Ty, null) ->
    null;
check_param(EP, Path, VarEnv, {list, T}, L) when is_list(L) ->
    %% Build a dummy structure to match the recursor. Unwrap this
    %% structure before replacing the list parameter.
    [check_param(EP, Path, VarEnv, T, X) || X <- L];
check_param(_EP, Path, _, #scalar_type{} = STy, V) ->
    non_polar_coerce(Path, STy, V);
check_param(EP, Path, VarEnv, #enum_type{} = ETy, {enum, V}) when is_binary(V) ->
    check_param(EP, Path, VarEnv, ETy, V);
check_param(EP, Path, _, #enum_type { id = Ty }, V) when is_binary(V) ->
    %% Determine the type of any enum term, and then coerce it
    case graphql_schema:lookup_enum_type(EP, V) of
        #enum_type { id = Ty } = ETy ->
            non_polar_coerce(Path, ETy, V);
        not_found ->
            err(Path, {enum_not_found, Ty, V});
        OtherTy ->
            err(Path, {param_mismatch, {enum, Ty, OtherTy}})
    end;
check_param(EP, Path, VarEnv, #input_object_type{} = IOType, Obj) when is_map(Obj) ->
    %% When an object comes in through JSON for example, then the input object
    %% will be a map which is already unique in its fields. To handle this, turn
    %% the object into the same form as the one we use on query documents and pass
    %% it on. Note that the code will create a map later on once the input has been
    %% uniqueness-checked.
    check_param(EP, Path, VarEnv, IOType, {input_object, maps:to_list(Obj)});
check_param(EP, Path, VarEnv, #input_object_type{} = IOType, {input_object, KVPairs}) ->
    check_input_object(EP, Path, VarEnv, IOType, {input_object, KVPairs});
%% Everything else are errors
check_param(_EP, Path, _, Ty, V) ->
    err(Path, {param_mismatch, Ty, V}).

coerce_default_param(EP, Path, VarEnv, Ty, Default) ->
    try check_param(EP, Path, VarEnv, Ty, Default) of
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
check_input_object(EP, Path, VarEnv,#input_object_type{ fields = Fields }, Obj) ->
    Coerced = coerce_input_object(Path, Obj),
    check_input_object_fields(EP, Path, VarEnv, maps:to_list(Fields), Coerced, #{}).

%% Input objects are in positive polarity, so the schema's fields are used
%% to verify that every field is present, and that there are no excess fields
%% As we process fields in the object, we remove them so we can check that
%% there are no more fields in the end.
check_input_object_fields(_EP, Path, _, [], Obj, Result) ->
    case maps:size(Obj) of
        0 -> Result;
        K when K > 0 -> err(Path, {excess_fields_in_object, Obj})
    end;
check_input_object_fields(EP, Path,
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
                                 coerce_default_param(EP, Path, VarEnv, Ty, Default)
                         end;
                     V ->
                         check_param(EP, [Name | Path], VarEnv, Ty, V)
                 end,
    check_input_object_fields(EP,
                              Path,
                              VarEnv,
                              Next,
                              maps:remove(Name, Obj),
                              Result#{ Name => CoercedVal }).

%% Handle non-polar inputs
non_polar_coerce(_Path,
                 #enum_type { resolve_module = undefined }, Value) ->
    Value;
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
        {error, Reason} -> graphql_err:abort(Path,
                                             {input_coercion, ID, Value, Reason})
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
       || #frag { id = ID } = Frg <- Frags]).

%% Type check a fragment
frag(Ctx, Path, #frag { schema = ScopeTy,
                        selection_set = SSet} = Frag) ->
    Frag#frag { selection_set = sset(Ctx, Path, ScopeTy, SSet) }.

%% -- OPERATIONS -------------------------------

%% Check that the variable environment of an operation is unique
op_unique_varenv(VDefs) ->
    NamedVars = [{graphql_ast:name(K), V}
                 || #vardef { id = K } = V <- VDefs],
    graphql_ast:uniq(NamedVars).

%% Type check an operation.
op(Ctx, Path, #op { id = ID,
                    schema = ScopeTy,
                    vardefs = VDefs,
                    selection_set = SSet} = Op) ->
    case op_unique_varenv(VDefs) of
        ok ->
            VarEnv = graphql_elaborate:mk_varenv(VDefs),
            CheckedSSet = sset(Ctx#{ varenv => VarEnv },
                               [ID | Path], ScopeTy, SSet),
            Op#op { selection_set = CheckedSSet };
        {not_unique, Var} ->
            err([ID | Path], {param_not_unique, Var})
    end.

%% -- SELECTION SETS ------------------------------------

%% Type check a selection set by recursing into each field in the selection
sset(Ctx, Path, Scope, SSet) ->
    [field(Ctx, Path, Scope, S) || S <- SSet].

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
field(#{ fragenv := FE, endpoint_context := EP } = Ctx, Path, ScopeTy, #frag_spread { id = ID, directives = Ds } = FSpread) ->
    Name = graphql_ast:name(ID),
    case maps:get(Name, FE, not_found) of
        not_found ->
            err([Name | Path], unknown_fragment);
        #frag { schema = FragTy } ->
            ok = fragment_embed(EP, [Name | Path], FragTy, ScopeTy),
            %% You can always include a fragspread, as long as it exists
            %% It may be slightly illegal in a given context but this just
            %% means the system will ignore the fragment on execution
            FSpread#frag_spread { directives = directives(Ctx, Path, Ds) }
    end;
field(Ctx=#{endpoint_context:=EP}, Path, Scope, #frag { id = '...',
                                schema = InnerScope,
                                selection_set = SSet,
                                directives = Ds} = InlineFrag) ->
    ok = fragment_embed(EP, [<<"...">> | Path], InnerScope, Scope),
    InlineFrag#frag {
        directives = directives(Ctx, [<<"...">> | Path], Ds),
        selection_set = sset(Ctx, [InlineFrag | Path], InnerScope, SSet)
    };
field(Ctx, Path, _Scope, #field { id = ID, schema = {introspection, typename}, directives = Ds } = F) ->
    Component = graphql_ast:name(ID),
    F#field { directives = directives(Ctx, [Component | Path], Ds)};
field(Ctx, Path, _Scope,
      #field {
         id = ID,
         args = Args,
         selection_set = SSet,
         directives = Ds,
         schema = #schema_field { args = SArgs, ty = InnerScope }} = F) ->
    Component = graphql_ast:name(ID),
    F#field { args = args(Ctx, [Component | Path], Args, SArgs),
              directives = directives(Ctx, [Component | Path], Ds),
              selection_set = sset(Ctx, [Component | Path], InnerScope, SSet) }.

%% -- DIRECTIVES --------------------------------

%% Type check directives. These can take arguments, so type checking
%% these amounts to type checking the arguments inside the directive.
directives(Ctx, Path, Ds) ->
    [directive(Ctx, Path, D) || D <- Ds].

directive(Ctx, Path,
          #directive { id = ID, args = Args,
                       schema = #directive_type { args = SArgs }} = D) ->
    D#directive { args = args(Ctx, [graphql_ast:name(ID) | Path], Args, SArgs) }.

%% -- ARGS -------------------------------------

%% Type checking of arguments to fields, directives, ...
%%
%% Arguments must be unique.
%%
%% Since arguments have positive polarity, they are checked according
%% to the schema arguments.
args(Ctx, Path, Args, Schema) ->
    NamedArgs = [{graphql_ast:name(K), V} || {K, V} <- Args],
    case graphql_ast:uniq(NamedArgs) of
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
args(Ctx=#{endpoint_context:=EP}, Path, Args, [{Name, #schema_arg { ty = STy }} = SArg | Next], Acc) ->
    case take_arg(Args, SArg) of
        {error, Reason} ->
            err([Name | Path], Reason);
        {ok, {_, #{ type := Ty, value := Val}} = A, NextArgs} ->
            SchemaType =
                case graphql_elaborate:type(EP, STy) of
                    {error, Reason} -> exit(Reason);
                    {_Polarity, SchemaTypeRsult} -> SchemaTypeRsult
                end,
            case judge(Ctx, [Name | Path], Val, SchemaType) of
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

%% Decide is a fragment can be embedded in a given scope
%% We proceed by computing the valid set of the Scope and also the
%% Valid set of the fragment. The intersection type between these two,
%% Scope and Spread, must not be the empty set. Otherwise it is a failure.
%%
%% The implementation here works by case splitting the different possible
%% output types one at a time and then handling them systematically rather
%% than running an intersection computation. This trades off computation
%% for code size when you have a match that can be optimized in any way.
%%

%% First a series of congruence checks
fragment_embed(EP, Path, SpreadType, {list, ScopeType}) ->
    %% Check congruences of lists
    fragment_embed(EP, Path, SpreadType, ScopeType);
fragment_embed(EP, Path, SpreadType, {non_null, ScopeType}) ->
    %% Check congruences of non-null values
    fragment_embed(EP, Path, SpreadType, ScopeType);
fragment_embed(EP, Path, {non_null, SpreadType}, ScopeType) ->
    %% Check congruences of non-null values
    fragment_embed(EP, Path, SpreadType, ScopeType);
%% Real concrete output objects/negative polarity checks
fragment_embed(_EP, _Path, #object_type { id = Ty },
                      #object_type { id = Ty  }) ->
    %% Object spread in Object scope requires a perfect match
    ok;
fragment_embed(_EP, Path, #object_type { id = SpreadTy },
                     #object_type { id = ScopeTy  }) ->
    %% Object spread in Object scope requires a perfect match
    err(Path, {fragment_spread, SpreadTy, ScopeTy});
fragment_embed(_EP, Path, #object_type { id = ID },
                     #union_type { id = UID,
                                   types = ScopeTypes }) ->
    case lists:member(ID, ScopeTypes) of
        true -> ok;
        false -> err(Path, {not_union_member, ID, UID})
    end;
fragment_embed(_EP, Path, #object_type { id = ID,
                                    interfaces = IFaces },
                     #interface_type { id = IID }) ->
    case lists:member(IID, IFaces) of
        true -> ok;
        false -> err(Path, {not_interface_member, ID, IID})
    end;
fragment_embed(_EP, Path, #interface_type { id = IID },
                     #object_type { id = OID, interfaces = IFaces }) ->
    case lists:member(IID, IFaces) of
        true -> ok;
        false -> err(Path, {not_interface_embedder, IID, OID})
    end;
fragment_embed(EP, Path, #interface_type { id = SpreadID },
                     #interface_type { id = ScopeID }) ->
    SpreadTypes = graphql_schema:lookup_interface_implementors(EP, SpreadID),
    ScopeTypes = graphql_schema:lookup_interface_implementors(EP, ScopeID),
    case ordsets:intersection(
           ordsets:from_list(SpreadTypes),
           ordsets:from_list(ScopeTypes)) of
        [_|_] ->
            ok;
        [] ->
            err(Path, {no_common_object, SpreadID, ScopeID})
    end;
fragment_embed(EP, Path, #interface_type { id = SpreadID },
                     #union_type{ id = ScopeID, types = ScopeMembers }) ->
    SpreadTypes = graphql_schema:lookup_interface_implementors(EP, SpreadID),
    case ordsets:intersection(
           ordsets:from_list(SpreadTypes),
           ordsets:from_list(ScopeMembers)) of
        [_|_] ->
            ok;
        [] ->
            err(Path, {no_common_object, SpreadID, ScopeID})
    end;
fragment_embed(_EP, Path, #union_type { id = UID, types = UMembers },
                     #object_type { id = OID }) ->
    case lists:member(OID, UMembers) of
        true -> ok;
        false -> err(Path, {not_union_embedder, UID, OID})
    end;
fragment_embed(EP, Path, #union_type { id = SpreadID, types = SpreadMembers },
                     #interface_type { id = ScopeID }) ->
    ScopeTypes = graphql_schema:lookup_interface_implementors(EP, ScopeID),
    case ordsets:intersection(
           ordsets:from_list(SpreadMembers),
           ordsets:from_list(ScopeTypes)) of
        [_|_] ->
            ok;
        [] ->
            err(Path, {no_common_object, SpreadID, ScopeID})
    end;
fragment_embed(_EP, Path, #union_type { id = SpreadID, types = SpreadMembers },
                     #union_type { id = ScopeID,  types = ScopeMembers }) ->
    case ordsets:intersection(
           ordsets:from_list(SpreadMembers),
           ordsets:from_list(ScopeMembers)) of
        [_|_] ->
            ok;
        [] ->
            err(Path, {no_common_object, SpreadID, ScopeID})
    end.

%% Decide if a type is an valid embedding in another type. We assume
%% that the first parameter is the 'D' type and the second parameter
%% is the 'S' type. These are the document and schema types
%% respectively. In some situations, it is allowed to have a more
%% strict type in the document then in the schema, but not vice versa.
%% This is what leads to a type embedding.
%%
%% Some of the cases are reflexivity. Some of the cases are congruences.
%% And some are special handling explicitly.
%%
type_embed(#scalar_type { id = ID }, #scalar_type { id = ID }) -> yes;
type_embed(#enum_type { id = ID }, #enum_type { id = ID }) -> yes;
type_embed(#input_object_type { id = ID }, #input_object_type { id = ID }) -> yes;
type_embed({non_null, DTy}, {non_null, STy}) ->
    type_embed(DTy, STy);
type_embed({non_null, DTy}, STy) ->
    %% A more strict document type of non-null is always allowed since
    %% it can't be null in the schema then
    type_embed(DTy, STy);
type_embed(_DTy, {non_null, _STy}) ->
    %% If the schema requires a non-null type but the document doesn't
    %% supply that, it is an error
    no;
type_embed({list, DTy}, {list, STy}) ->
    %% Lists are decided by means of a congruence
    type_embed(DTy, STy);
type_embed(DTy, {list, STy}) ->
    %% A singleton type is allowed to be embedded in a list according to the
    %% specification (Oct 2016)
    type_embed(DTy, STy);
type_embed(_DTy, _STy) ->
    %% Any other type combination are invalid
    no.

%% Judge a list of values with the same type.
judge_list(_Ctx, _Path, [], _Type, _K) ->
    [];
judge_list(Ctx, Path, [V|Vs], Type, K) ->
    R = judge(Ctx, [K|Path], V, Type),
    [R | judge_list(Ctx, Path, Vs, Type, K+1)].

%% Judge a type and a value. Used to verify a type judgement of the
%% form 'a : T' for a value 'a' and a type 'T'. Analysis has shown that
%% it is most efficient to make the inversion analysis work on the value
%% from the document first and then make the inversion analysis on the schema-type.
judge(Ctx, Path, {name, _, N}, SType) ->
    judge(Ctx, Path, N, SType);
judge(#{ varenv := VE }, Path, {var, ID}, SType) ->
    Var = graphql_ast:name(ID),
    case maps:get(Var, VE, not_found) of
        not_found -> err(Path, {unbound_variable, Var});
        #vardef { ty = DType } ->
            case type_embed(DType, SType) of
                yes ->
                    {var, ID, DType};
                no ->
                    err(Path, {type_mismatch,
                               #{ document => {var, Var, DType},
                                  schema => SType }})
            end
    end;
judge(Ctx, Path, Value, {non_null, InnerSType} = SType) ->
    case Value of
        null ->
            err(Path, {type_mismatch,
                       #{ document => Value, schema => SType }});
        _Valid ->
            judge(Ctx, Path, Value, InnerSType)
    end;
judge(_Ctx, _Path, null, _SType) ->
    %% If a value is null, and we don't have a non-null case,
    %% then the value is valid
    null;
judge(Ctx, Path, Values, SType) when is_list(Values) ->
    case SType of
        {list, InnerType} ->
            judge_list(Ctx, Path, Values, InnerType, 0)
    end;
judge(Ctx, Path, Value, {list, _} = SType) ->
    %% If the value is not of list-type, but we expect a list,
    %% then hoist the value into a singleton list and recurse
    judge(Ctx, Path, [Value], SType);
judge(_Ctx=#{endpoint_context:=EP}, Path, {enum, N}, SType) ->
    case graphql_schema:lookup_enum_type(EP, N) of
        not_found ->
            err(Path, {unknown_enum, N});
        SType ->
            non_polar_coerce(Path, SType, N);
        Other ->
            err(Path, {type_mismatch,
                       #{ document => Other,
                          schema => SType }})
    end;
judge(#{ varenv := VarEnv, endpoint_context := EP }, Path, {input_object, _} = InputObj, SType) ->
    case SType of
        #input_object_type{} = IOType ->
            Coerced = coerce_input_object(Path, InputObj),
            check_input_object(EP, Path, VarEnv, IOType, Coerced);
        _OtherType ->
            err(Path, {type_mismatch, #{ document => InputObj, schema => SType }})
    end;
judge(_Ctx, Path, Value, #scalar_type{} = SType) ->
    non_polar_coerce(Path, SType, Value);
judge(_Ctx, Path, String, #enum_type{}) when is_binary(String) ->
    %% The spec (Oct 2016, section 3.1.5) says that this is not allowed, unless
    %% given as a parameter. In this case, it is not given as a parameter, but
    %% is expanded in as a string in a query document. Reject.
    err(Path, enum_string_literal);
judge(_Ctx, Path, Value, #enum_type{} = EType) ->
    non_polar_coerce(Path, EType, Value);
judge(_Ctx, Path, Value, Unknown) ->
    err(Path, {type_mismatch,
               #{ document => Value, schema => Unknown }}).

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

%% Tell the error logger that something is off
error_report(Term, Cl, Err) ->
  error_logger:error_report(
    [
     Term,
     {error, Cl, Err},
     {stack, erlang:get_stacktrace()}
    ]).
