-module(graphql_execute).

-include("graphql_internal.hrl").
-include("graphql_schema.hrl").

-export([x/1, x/2]).
-export([default_resolver/3]).

-spec x(graphql:ast()) -> #{ atom() => graphql:json() }.
x(X) -> x(#{ params => #{} }, X).

-spec x(term(), graphql:ast()) -> #{ atom() => graphql:json() }.
x(Ctx, X) ->
    Canon = canon_context(Ctx),
    document(Canon, X).

document(Ctx, {document, Operations}) ->
    {Fragments, Ops} = lists:partition(fun (#frag {}) -> true;(_) -> false end, Operations),
    operations(Ctx#{ fragments => fragments(Fragments) }, Ops).

%% -- FRAGMENTS -------------------------------
fragments(Frags) ->
    lists:foldl(fun(#frag { id = ID } = Frag, St) ->
        St#{ name(ID) => Frag }
    end,
    #{},
    Frags).

%% -- TOP LEVEL HANDLING ------------------------------
operations(#{ operation_name := undefined } = Ctx, [Op]) ->
    return(Ctx, root([undefined], Ctx, Op));
operations(#{ operation_name := undefined }, _) ->
    throw(more_than_one_operation);
operations(#{ operation_name := OpName } = Ctx, Ops) ->
    case find_operation(OpName, Ops) of
        not_found ->
           throw({operation_not_found, OpName});
        Op ->
           return(Ctx, root([OpName], Ctx, Op))
    end;
operations(Ctx, [Op]) ->
    %% No operation name given and there is only a single operation in the
    %% document. Run that operation.
    return(Ctx, root([Op], Ctx, Op)).

find_operation(_N, []) -> not_found;
find_operation(N, [#op { id = ID } = O | Next]) ->
    case name(ID) of
        N -> O;
        _ -> find_operation(N, Next)
    end.

root(Path, Ctx, #op { selection_set = SSet, schema = Schema } = Op) ->
    handle([Op | Path], Ctx, none, Schema, SSet).

%% -- RETURNING RESULTS -------------------------------

return(_Ctx, {Res, Errs}) when is_map(Res) ->
    Map = case Errs of
        [] -> #{};
        Errs -> #{ errors => Errs }
    end,
    Map#{ data => Res }.

%% -- GENERIC HANDLING --------------------------------

%% -- Product objects / vector types
handle(Path, Ctx, Cur, #object_type {} = Obj, SSet) ->
    resolve_obj([Obj | Path], Ctx, Cur, SSet, Obj, #{});
handle(Path, Ctx, Cur, #interface_type { id = IFaceID, resolve_type = R }, SSet) ->
    handle_abstract_type(Path, Ctx, Cur, IFaceID, R, SSet);
handle(Path, Ctx, Cur, #union_type { id = UnionID, resolve_type = R}, SSet) ->
    handle_abstract_type(Path, Ctx, Cur, UnionID, R, SSet);
%% -- Scalars / scalar types
handle(Path, _Ctx, Cur, #scalar_type { id = ID, output_coerce = Coerce }, []) ->
    try Coerce(Cur) of
        {ok, Result} -> {Result, []};
        {error, Reason} -> err(Path, {output_coerce, ID, Cur, Reason})
    catch
        Class:Error ->
            err(Path, {coerce, ID, Cur, {Class,Error}})
    end;
handle(_Path, Ctx, Cur, #enum_type {} = Enum, SSet) ->
    resolve_enum(Ctx, Cur, SSet, Enum).

%% Helper for calling the type resolver in an interface or union
handle_abstract_type(Path, Ctx, Cur, ID, R, SSet) ->
    try R(Cur) of
        {ok, Ty} ->
          #object_type{} = Obj = graphql_schema:get(binarize(Ty)),
          resolve_obj([Obj | Path], Ctx, Cur, SSet, Obj, #{});
        {error, Reason} ->
            throw(err(Path, {unresolved_type, ID, Reason}))
    catch
        Class:Err ->
            throw(err(Path, {resolver_crash, ID, Cur, {Class, Err}}))
    end.

%% -- ENUM HANDLING ------------------------------------

resolve_enum(_Ctx, Val, [], #enum_type {}) when is_binary(Val) ->
    {Val, []};
resolve_enum(Ctx, {enum, Val}, [], EnumType) when is_binary(Val) ->
    resolve_enum(Ctx, Val, [], EnumType);
resolve_enum(_Ctx, Val, [], #enum_type { values = Vals }) when is_integer(Val) ->
    #enum_value { val = Res } = maps:get(Val, Vals),
    {Res, []}.

%% -- FRAGMENT HANDLING -----------------------------------

%% fragment_match/3 will try to match a fragment against a type
%%
%%
fragment_match(_Ctx,
	#frag { schema = SchemaTy } = Frag,
	#object_type { id = ID, interfaces = Implements }) ->
    case SchemaTy of
        #object_type{ id = OID } ->
            %% Objects need a perfect match
            case OID =:= ID of
                true -> {match, Frag};
                false -> nomatch
            end;
        #interface_type { id = IFaceID } ->
            %% Interfaces must be implemented by the concrete object
            case lists:member(IFaceID, Implements) of
                true -> {match, Frag};
                false -> nomatch
            end;
        #union_type{ types = Types } ->
            %% Unions match if they match one of the implementing types
            case lists:member(ID, Types) of
                true -> {match, Frag};
                false -> nomatch
            end
    end;
fragment_match(#{ fragments := Fragments } = Ctx, #frag_spread { id = FragID }, Obj) ->
    %% Matching a fragment requires an environment lookup, otherwise it's
    %% the same as for inline fragments.
    Frag = maps:get(name(FragID), Fragments),
    fragment_match(Ctx, Frag, Obj).

resolve_frag(Ctx, Frag, SObj) ->
    case fragment_match(Ctx, Frag, SObj) of
        {match, #frag { selection_set = FragFields }} ->
            {add_fields, FragFields};
        nomatch ->
            skip
    end.


%% -- OBJECT HANDLING --------------------------------

resolve_obj(Path, Ctx, Cur, Fields, SObj, SoFar) ->
    case resolve_obj_fold(Path, Ctx, Cur, Fields, SObj, SoFar, [], []) of
        {ok, Acc, Errs} ->
            {maps:from_list(Acc), Errs};
        {error, Errs} ->
            {null, Errs}
    end.
    
resolve_obj_fold(_Path, _Ctx, _Cur, [], _SObj, _SoFar, Acc, Errs) ->
    {ok, Acc, Errs};
resolve_obj_fold(Path, Ctx, Cur, [F | Next], SObj, SoFar, Acc, Errs) ->
    case resolve_obj_([F | Path], Ctx, Cur, F, SObj, SoFar) of
        skip ->
            resolve_obj_fold(Path, Ctx, Cur, Next, SObj, SoFar, Acc, Errs);
        {ok, Alias, Es, Result} ->
            resolve_obj_fold(
                Path,
                Ctx,
                Cur,
                Next,
                SObj,
                SoFar#{ Alias => true},
                [{Alias, Result} | Acc],
                Es ++ Errs);
        {object_error, Es} ->
            {error, Es ++ Errs};
        {add_fields, FragFields} ->
            resolve_obj_fold(Path, Ctx, Cur, FragFields ++ Next, SObj, SoFar, Acc, Errs)
    end.

resolve_obj_(_Path, Ctx, _Cur, #frag {} = Frag, SObj, _SoFar) ->
    resolve_frag(Ctx, Frag, SObj);
resolve_obj_(_Path, Ctx, _Cur, #frag_spread {} = NamedSpread, SObj, _SoFar) ->
    resolve_frag(Ctx, NamedSpread, SObj);
resolve_obj_(Path, Ctx, Cur, #field { selection_set = SSet, schema_obj = FObj } = F,
	#object_type { id = OID, fields = SFields } = SObj, SoFar) ->
    Name = name(F),
    Alias = alias(F),
    case maps:is_key(Alias, SoFar) of
        true -> skip; %% Already rendered this entry, skip it
        false ->
            case maps:get(Name, SFields, not_found) of
                not_found when Name == <<"__typename">> ->
                    {ok, Alias, [], resolve_typename(SObj)};
                not_found ->
                    throw({execute, {unknown_field, Name, line(F)}});
                #schema_field { ty = Ty, resolve = RF } ->
                    Fun = resolver_function(RF),
                    Args = resolve_args(Ctx, F),
                    case Fun(Ctx#{ field => Name, object_type => OID }, Cur, Args) of
                        {error, Reason} ->
                            Error = format_error(Path, Ctx, Name, OID, Reason),
                            handle_null(Ty, Alias, [Error], null);
                        {ok, null} ->
                            handle_null(Ty, Alias, [], null);
                        {ok, Result} ->
                            %%ok = coerce_type(Ty, FObj),
                             {Materialized, Errs} = handle_type(Path, Ctx, Result, Ty, SSet, FObj),
                             handle_null(Ty, Alias, Errs, Materialized);
                     Wrong ->
                           exit({wrong_resolver_function_return, Fun, Alias, Wrong})
                   end
            end
    end.

handle_null({non_null, _}, Alias, _, null) ->
    {object_error, [{context_non_null, Alias}]};
handle_null(_, Alias, [], Val) ->
    {ok, Alias, [], Val};
handle_null(_, Alias, Errs, Val) ->
    {ok, Alias, Errs, Val}.

coerce_type(X, X) -> ok;
coerce_type(X, Obj) when is_binary(X) ->
    case graphql_schema:id(Obj) of
        X -> ok;
        Y -> {X, '/=', Y}
    end.

handle_type(Path, Ctx, Result, Ty, SSet, FObj) ->
    case graphql_ast:resolve_type(Ty) of
        {scalar, Scalar} ->
            SType = output_coerce_type(Scalar),
            handle(Path, Ctx, Result, SType, SSet);
        {list, {scalar, Scalar}} when is_list(Result) ->
            SType = output_coerce_type(Scalar),
            Coerced = [handle(Path, Ctx, R, SType, SSet) || R <- Result],
            {Vals, Errs} = lists:unzip(Coerced),
            {[V || V <- Vals, V /= null], lists:concat(Errs)};
        {list, {scalar, _}} ->
            {null, [non_list_type]};
        {list, _T} ->
            handle_list(Path, Ctx, Result, FObj, SSet);
        _T ->
            handle(Path, Ctx, Result, FObj, SSet)
    end.

%% -- FUNCTION RESOLVERS ---------------------------------

resolver_function(R) when is_function(R, 3) -> R;
resolver_function(undefined) -> fun ?MODULE:default_resolver/3.


default_resolver(_, none, _) -> {error, no_object};
default_resolver(_, undefined, _) -> {error, undefined_object};
default_resolver(_, null, _) ->
    %% A Null value is a valid object value
    {ok, null};
default_resolver(#{ field := Field}, Cur, _Args) ->
    case maps:get(Field, Cur, not_found) of
        {'$lazy', F} when is_function(F, 0) -> F();
        not_found ->
            {error, not_found};
        V -> {ok, V}
    end.

%% -- OUTPUT COERCION ------------------------------------

output_coerce_type(id) ->
    #scalar_type { id = id, output_coerce = fun
        (B) when is_binary(B) -> {ok, B};
        (_) -> {ok, null}
      end };
output_coerce_type(string) ->
    #scalar_type { id = string, output_coerce = fun
        (B) when is_binary(B) -> {ok, B};
        (_) -> {ok, null}
      end };
output_coerce_type(bool) ->
    #scalar_type { id = bool, output_coerce = fun
        (true) -> {ok, true};
        (<<"true">>) -> {ok, true};
        (false) -> {ok, false};
        (<<"false">>) -> {ok, false};
        (_) -> {ok, null}
      end };
output_coerce_type(int) ->
    #scalar_type { id = int, output_coerce = fun
        (I) when is_integer(I) -> {ok, I};
        (_) -> {ok, null}
      end };
output_coerce_type(float) ->
    #scalar_type { id = float, output_coerce = fun
        (F) when is_float(F) -> {ok, F};
        (_) -> {ok, null}
      end };
output_coerce_type(#scalar_type{} = SType) -> SType;
output_coerce_type(UserDefined) when is_binary(UserDefined) ->
    case graphql_schema:lookup(UserDefined) of
        #scalar_type{} = SType -> SType;
        not_found -> not_found
    end.

%% -- MATERIALIZATION ------------------------------------------

%% The materialization step collects underlying values and collects them together
%% into a coherent whole.
    
handle_list(Path, Ctx, Result, FObj, SSet) when is_list(Result) ->
    {L, Errs} = handle_list(Path, 0, Ctx, Result, FObj, SSet, [], []),
    Res = {[R || R <- L, R /= null], Errs},
    Res.

handle_list(_Path, _K, _Ctx, [], _FObj, _SSet, Acc, Errs) ->
    {lists:reverse(Acc), Errs};
handle_list(Path, K, Ctx, [R | RS], FObj, SSet, Acc, Errs) ->
    {MRes, Es} = handle([K | Path], Ctx, R, FObj, SSet),
    handle_list(Path, K+1, Ctx, RS, FObj, SSet, [MRes | Acc], Es ++ Errs).
    
%% -- LOWER LEVEL RESOLVERS ----------------

resolve_args(Ctx, #field { args = As }) ->
    resolve_args_(Ctx, As, #{}).
    
resolve_args_(_Ctx, [], Acc) -> Acc;
resolve_args_(Ctx, [{ID, Val} | As], Acc) ->
    K = name(ID),
    V = value(Ctx, Val),
    resolve_args_(Ctx, As, Acc#{ K => V }).

%% Produce a valid value for an argument.

value(Ctx, {Ty, Val}) -> value(Ctx, #{ type => Ty, value => Val});
value(_Ctx, #{ type := #enum_type { repr = Repr }, value := {enum, E}}) ->
    N = name(E),
    case Repr of
        binary -> N;
         atom -> binary_to_atom(N, utf8);
        tagged -> {enum, N}
    end;
value(_Ctx, #{ type := Ty, value := {enum, E}}) ->
    N = name(E),
    #enum_type { repr = Repr } = graphql_schema:get(Ty),
    case Repr of
        binary -> N;
        atom -> binary_to_atom(N, utf8);
        tagged -> {enum, N}
    end;
value(Ctx, #{ type := [Ty], value := {list, Vals}}) ->
    [value(Ctx, {Ty, V}) || V <- Vals];
value(Ctx, #{ type := [Ty], value := Vals}) when is_list(Vals) ->
    [value(Ctx, {Ty, V}) || V <- Vals];
value(Ctx, #{ type := {non_null, Ty}, value := V}) ->
    value(Ctx, #{ type => Ty, value => V});
value(Ctx, #{ type := ObjTy, value := {object, O}}) ->
    #input_object_type { fields = FieldEnv } = graphql_schema:get(
        graphql_ast:unwrap_type(ObjTy)),
    ObjVals = value_object(Ctx, FieldEnv, O),
    maps:from_list(ObjVals);
value(Ctx, #{ type := #input_object_type { fields = FieldEnv },
              value := O}) when is_map(O) ->
    ObjVals = value_object(Ctx, FieldEnv, maps:to_list(O)),
    maps:from_list(ObjVals);
value(Ctx, #{ type := ObjTy, value := O}) when is_map(O) ->
    #input_object_type { fields = FieldEnv } = graphql_schema:get(
        graphql_ast:unwrap_type(ObjTy)),
    ObjVals = value_object(Ctx, FieldEnv, maps:to_list(O)),
    maps:from_list(ObjVals);
value(#{ params := Params }, #{ value := {var, ID}}) ->
    maps:get(name(ID), Params);
value(_Ctx, #{ type := {scalar, STy}, value := V}) ->
    value_scalar(STy, V);
value(_Ctx, #{ type := _, value := V} = M) ->
    lager:info("Resolving: ~p", [M]),
    V.

value_scalar(string, null) -> null;
value_scalar(string, S) when is_binary(S) -> S;
value_scalar(id, null) -> null;
value_scalar(id, S) when is_binary(S) -> S;
value_scalar(bool, true) -> true;
value_scalar(bool, false) -> false;
value_scalar(int, I) when is_integer(I) -> I;
value_scalar(int, null) -> null;
value_scalar(float, F) when is_float(F) -> F;
value_scalar(float, null) -> null;
value_scalar(Ty, V) ->
    lager:warning("Scalar resolver missing for built-in type: ~p (value: ~p)", [Ty, V]),
    V.

value_object(_, _, []) -> [];
value_object(Ctx, FieldEnv, [{K, Val} | Rest]) ->
    Name = name(K),
    #schema_arg { ty = Ty } = maps:get(Name, FieldEnv),
    Value = value(Ctx, {Ty, Val}),
    [{Name, Value} | value_object(Ctx, FieldEnv, Rest)].

%% -- AST MANIPULATION ----------------

resolve_typename(#object_type { id = ID }) -> ID.

name(N) when is_binary(N) -> N;
name({name, _, N}) -> N;
name(#field { id = ID }) -> name(ID).

alias(#field { alias = undefined, id = ID }) -> name(ID);
alias(#field { alias = Alias }) -> name(Alias).

line({name, L, _}) -> L;
line(#field { id = ID }) -> line(ID).

%% -- ERROR FORMATTING --------------------
format_error(Path, #{ error_formatter := EFMod }, Name, OID, Reason) ->
    try EFMod:format_error(graphql_err:path(Path), Name, OID, Reason) of
        {ok, Err} -> [Err];
        {error, Err} ->
            lager:warning("Unable to format error ~p due to ~p", [Reason, {error, Err}]),
            []
    catch
        Class:Error ->
            lager:error("Error formatter crash: ~p:~p .. ~p",
                [Class, Error, erlang:get_stacktrace()]),
            []
    end;
format_error(Path, #{}, Name, OID, Reason) ->
    lager:warning("Handler crash ~p (~p:~p) with no error_formatter: ~p",
        [graphql_err:path(Path), OID, Name, Reason]),
    [].

%% -- ERROR HANDLING --
err(Path, Reason) ->
    {error, Path, Reason}.

%% -- CONTEXT CANONICALIZATION ------------
canon_context(#{ params := Params } = Ctx) ->
     Ctx#{ params := canon_params(Params) }.
     
canon_params(Ps) ->
     KVs = maps:to_list(Ps),
     maps:from_list([{binarize(K), V} || {K, V} <- KVs]).
     
binarize(A) when is_atom(A) -> atom_to_binary(A, utf8);
binarize(B) when is_binary(B) -> B;
binarize(L) when is_list(L) -> list_to_binary(L).
