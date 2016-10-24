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
    {Fragments, Rest} = operation_fragments(Operations),
    Frags = fragments(Fragments),
    operations(Ctx#{ fragments => Frags }, Rest).

%% -- FRAGMENTS -------------------------------
fragments(Frags) ->
    lists:foldl(fun(#frag { id = ID } = Frag, St) ->
        St#{ name(ID) => Frag }
    end,
    #{},
    Frags).

operation_fragments(Ops) ->
    lists:partition(fun (#frag {}) -> true;(_) -> false end, Ops).

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
    object([Op | Path], Ctx, none, Schema, SSet).

%% -- RETURNING RESULTS -------------------------------

return(_Ctx, {Result, []}) ->
    #{ data => maps:from_list(Result) };
return(_Ctx, {Result, Errs}) ->
    #{ data => maps:from_list(Result), errors => Errs }.

%% -- OBJECT HANDLING --------------------------------
object(Path, _Ctx, Cur, #scalar_type { id = ID, output_coerce = Coerce }, []) ->
    try Coerce(Cur) of
        {ok, Result} -> {Result, []};
        {error, Reason} -> err(Path, {output_coerce, ID, Cur, Reason})
    catch
        Class:Error ->
            err(Path, {coerce, ID, Cur, {Class,Error}})
    end;
object(Path, Ctx, Cur, #object_type {} = Obj, SSet) ->
    resolve_obj([Obj | Path], Ctx, Cur, SSet, Obj, #{});
object(Path, Ctx, Cur, #interface_type { id = IFaceID, resolve_type = R }, SSet) ->
    resolve_type(Path, Ctx, Cur, IFaceID, R, SSet);
object(Path, Ctx, Cur, #union_type { id = UnionID, resolve_type = R}, SSet) ->
    resolve_type(Path, Ctx, Cur, UnionID, R, SSet);
object(_Path, Ctx, Cur, #enum_type {} = Enum, SSet) ->
    resolve_enum(Ctx, Cur, SSet, Enum).

resolve_enum(_Ctx, Val, [], #enum_type {}) when is_binary(Val) ->
    {Val, []};
resolve_enum(Ctx, {enum, Val}, [], EnumType) when is_binary(Val) ->
    resolve_enum(Ctx, Val, [], EnumType);
resolve_enum(_Ctx, Val, [], #enum_type { values = Vals }) when is_integer(Val) ->
    #enum_value { val = Res } = maps:get(Val, Vals),
    {Res, []}.

resolve_frag(Ctx, Frag, SObj) ->
    case fragment_match(Ctx, Frag, SObj) of
        {match, #frag { selection_set = FragFields }} ->
            {add_fields, FragFields};
        nomatch ->
            skip
    end.

resolve_obj(Path, Ctx, Cur, Fields, SObj, SoFar) ->
    resolve_obj_fold(Path, Ctx, Cur, Fields, SObj, SoFar, [], []).
    
resolve_obj_fold(_Path, _Ctx, _Cur, [], _SObj, _SoFar, Acc, Errs) ->
    {Acc, Errs};
resolve_obj_fold(Path, Ctx, Cur, [F | Next], SObj, SoFar, Acc, Errs) ->
    case resolve_obj_([F | Path], Ctx, Cur, F, SObj, SoFar) of
        skip ->
            resolve_obj_fold(Path, Ctx, Cur, Next, SObj, SoFar, Acc, Errs);
        { {A, _} = R, E} ->
            resolve_obj_fold(Path, Ctx, Cur, Next, SObj, SoFar#{ A => true}, [R | Acc], E ++ Errs);
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
        true ->
            skip; %% Already rendered this entry, skip it
        false ->
            case maps:get(Name, SFields, not_found) of
                not_found when Name == <<"__typename">> ->
                    { {Alias, resolve_typename(SObj)}, [] };
                not_found ->
                    throw({execute, {unknown_field, Name, line(F)}});
                #schema_field { ty = Ty, resolve = RF } ->
                    Fun = resolver_function(RF),
                    Args = resolve_args(Ctx, F),
                    case Fun(Ctx#{ field => Name, object_type => OID }, Cur, Args) of
                        {error, Reason} ->
                            Error = format_error(Path, Ctx, Name, OID, Reason),
                            { {Alias, null}, Error };
                        {ok, null} ->
                          %% Failure to retrieve a result cuts the computation at this node
                          { {Alias, null}, [] };
                        {ok, Result} ->
                            {R, Es} = case graphql_ast:resolve_type(Ty) of
                                {scalar, Scalar} ->
                                    [] = SSet,
                                    { {Alias, output_coerce(Scalar, Result)}, []};
                                {list, {scalar, Scalar}} when is_list(Result) ->
                                    [] = SSet,
                                    { {Alias, list_output_coerce(Scalar, Result)}, []};
                                {list, {scalar, _}} ->
                                    [] = SSet,
                                    { {Alias, null}, [] };
                                {list, _T} ->
                                    {RL, Errs} = materialize_list(Path, Ctx, Result, FObj, SSet),
                                    { {Alias, RL}, Errs };
                                 _T ->
                                    {Materialized, Errs} = materialize(Path, Ctx, Result, FObj, SSet),
                                    { {Alias, Materialized}, Errs}
                           end,
                           { R, Es };
                       Wrong ->
                           exit({wrong_resolver_function_return, Fun, Alias, Wrong})
                   end
            end
    end.

resolver_function(R) when is_function(R, 3) -> R;
resolver_function(undefined) -> fun ?MODULE:default_resolver/3.

%% Helper for calling the type resolver in an interface or union
resolve_type(Path, Ctx, Cur, ID, R, SSet) ->
    try R(Cur) of
        {ok, Ty} ->
          #object_type{} = Obj = graphql_schema:get(canon_ty(Ty)),
          resolve_obj([Obj | Path], Ctx, Cur, SSet, Obj, #{});
        {error, Reason} ->
            throw(err(Path, {unresolved_type, ID, Reason}))
    catch
        Class:Err ->
            throw(err(Path, {resolver_crash, ID, Cur, {Class, Err}}))
    end.

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

list_output_coerce(_Scalar, []) -> [];
list_output_coerce(Scalar, [X | Xs]) ->
    case output_coerce(Scalar, X) of
        null -> list_output_coerce(Scalar, Xs);
        Res -> [Res | list_output_coerce(Scalar, Xs)]
    end.

output_coerce(_, null) -> null;
output_coerce(string, B) when is_binary(B) -> B;
output_coerce(string, _) -> null;
output_coerce(bool, true) -> true;
output_coerce(bool, false) -> false;
output_coerce(bool, <<"true">>) -> true;
output_coerce(bool, <<"false">>) -> false;
output_coerce(bool, _) -> null;
output_coerce(int, I) when is_integer(I) -> I;
output_coerce(int, _) -> null;
output_coerce(float, F) when is_float(F) -> F;
output_coerce(float, _) -> null;
output_coerce(id, B) when is_binary(B) -> B;
output_coerce(id, _) -> null;
output_coerce(UserDefined, Data) ->
    case graphql_schema:lookup(UserDefined) of
        #scalar_type { output_coerce = F } ->
           try F(Data) of
              Result -> Result
           catch
              Class:Error ->
                  throw({user_defined_output_coerce, UserDefined, Data, Class, Error})
           end;
        not_found ->
           throw({user_defined_scalar_not_found, UserDefined})
    end.

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
    
resolve_typename(#object_type { id = ID }) -> ID.

materialize_list(Path, Ctx, Result, FObj, SSet) when is_list(Result) ->
    materialize_list(Path, 0, Ctx, Result, FObj, SSet, [], []).

materialize_list(_Path, _K, _Ctx, [], _FObj, _SSet, Acc, Errs) ->
    {lists:reverse(Acc), Errs};
materialize_list(Path, K, Ctx, [R | RS], FObj, SSet, Acc, Errs) ->
    {MRes, Es} = materialize([K | Path], Ctx, R, FObj, SSet),
    materialize_list(Path, K+1, Ctx, RS, FObj, SSet, [MRes | Acc], Es ++ Errs).
    
materialize(Path, Ctx, Cur, Ty, SSet) ->
    case object(Path, Ctx, Cur, Ty, SSet) of
        {L, Es} when is_list(L) ->
            {LR, LErrs} = split_list_result(L, [], []),
            {maps:from_list(LR), LErrs ++ Es};
        {V, Es} when is_integer(V) -> {V, Es};
        {V, Es} when is_binary(V) -> {V, Es};
        {V, Es} when is_float(V) -> {V, Es}
    end.

split_list_result([], Acc, Errs) -> {Acc, Errs};
split_list_result([ {K, {V, E}} | Next], Acc, Errs) ->
    split_list_result(Next, [{K, V} | Acc], E ++ Errs);
split_list_result([ {K, V} | Next], Acc, Errs) ->
    split_list_result(Next, [{K,V}|Acc], Errs).


%% -- LOWER LEVEL RESOLVERS ----------------

resolve_args(Ctx, #field { args = As }) ->
    resolve_args_(Ctx, As, #{}).
    
resolve_args_(_Ctx, [], Acc) -> Acc;
resolve_args_(Ctx, [{ID, Val} | As], Acc) ->
    K = name(ID),
    V = value(Ctx, Val),
    resolve_args_(Ctx, As, Acc#{ K => V }).

%% -- AST MANIPULATION ----------------

name(N) when is_binary(N) -> N;
name({name, _, N}) -> N;
name(#field { id = ID }) -> name(ID).

alias(#field { alias = undefined, id = ID }) -> name(ID);
alias(#field { alias = Alias }) -> name(Alias).

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
value(Ctx, #{ value := {var, {name, _, N}}}) -> var_lookup(Ctx, N);
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
value_scalar(Ty, V) ->
    lager:info("Scalar Resolving: ~p", [{Ty, V}]),
    V.

value_object(_, _, []) -> [];
value_object(Ctx, FieldEnv, [{K, Val} | Rest]) ->
    Name = name(K),
    #schema_arg { ty = Ty } = maps:get(Name, FieldEnv),
    Value = value(Ctx, {Ty, Val}),
    [{Name, Value} | value_object(Ctx, FieldEnv, Rest)].

line({name, _, L}) -> L;
line(#field { id = ID }) -> line(ID).

var_lookup(#{ params := Params }, N) ->
    maps:get(N, Params).

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
canon_ty(A) when is_atom(A) -> atom_to_binary(A, utf8).

canon_context(#{ params := Params } = Ctx) ->
     Ctx#{ params := canon_params(Params) }.
     
canon_params(Ps) ->
     KVs = maps:to_list(Ps),
     maps:from_list([{binarize(K), V} || {K, V} <- KVs]).
     
binarize(A) when is_atom(A) -> atom_to_binary(A, utf8);
binarize(B) when is_binary(B) -> B;
binarize(L) when is_list(L) -> list_to_binary(L).
