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
    execute_request(Canon, X).

execute_request(InitialCtx, {document, Operations}) ->
    {Frags, Ops} = lists:partition(fun (#frag {}) -> true;(_) -> false end, Operations),
    Ctx = InitialCtx#{ fragments => fragments(Frags) },
    case get_operation(Ctx, Ops) of
        {ok, #op { ty = {query, _} } = Op } ->
            execute_query(Ctx#{ op_type => query }, Op);
        {ok, #op { ty = undefined } = Op } ->
            execute_query(Ctx#{ op_type => query }, Op);
        {ok, #op { ty = {mutation, _} } = Op } ->
            execute_mutation(Ctx#{ op_type => mutation }, Op);
        {error, Reason} ->
            complete_top_level(null, [Reason])
    end.

complete_top_level(Res, []) ->
    #{ data => Res };
complete_top_level(undefined, Errs) ->
    #{ errors => Errs };
complete_top_level(Res, Errs) ->
    #{ data => Res, errors => Errs}.

execute_query(Ctx, #op { selection_set = SSet,
                          schema = QType } = Op) ->
    #object_type{} = QType,
    {Res, Errs} =
        execute_sset([graphql_ast:id(Op)], Ctx, SSet, QType, none),
    complete_top_level(Res, Errs).

execute_mutation(Ctx, #op { selection_set = SSet,
                             schema = QType } = Op) ->
    #object_type{} = QType,
    {Res, Errs} =
        execute_sset([graphql_ast:id(Op)], Ctx, SSet, QType, none),
    complete_top_level(Res, Errs).

execute_sset(Path, Ctx, SSet, Type, Value) ->
    GroupedFields = collect_fields(Path, Ctx, Type, SSet),
    execute_sset(Path, Ctx, GroupedFields, Type, Value, [], []).

execute_sset(_Path, _Ctx, [], _Type, _Value, Map, Errs) ->
    {maps:from_list(lists:reverse(Map)), Errs};
execute_sset(Path, Ctx, [{Key, [F|_] = Fields} | Next], Type, Value, Map, Errs) ->
    case lookup_field(F, Type) of
        null -> execute_sset(Path, Ctx, Next, Type, Value, Map, Errs);
        not_found -> execute_sset(Path, Ctx, Next, Type, Value, Map, Errs);
        typename ->
            execute_sset(Path, Ctx, Next, Type, Value, [{Key, typename(Type)} | Map], Errs);
        FieldType ->
            case execute_field([Key | Path], Ctx, Type, Value, Fields, FieldType) of
                {ok, Result, FieldErrs} ->
                    execute_sset(Path, Ctx, Next, Type, Value, [{Key, Result} | Map], FieldErrs ++ Errs);
                {error, Errors} ->
                    {null, Errors}
            end
    end.

typename(#object_type { id = ID }) -> ID.

lookup_field(#field { id = ID }, Obj) ->
    lookup_field_name(name(ID), Obj).

lookup_field_name(<<"__typename">>, _) -> typename;
lookup_field_name(N, #object_type { fields = FS }) ->
    maps:get(N, FS, not_found);
lookup_field_name(N, #interface_type { fields = FS }) ->
    maps:get(N, FS, not_found).

view_include_skip_directives(_Ctx, []) -> include;
view_include_skip_directives(Ctx, [#directive { id = ID } = D | Next]) ->
    Args = resolve_args(Ctx, D),
    case {name(ID), Args} of
        {<<"include">>, #{ <<"if">> := true }} ->
            view_include_skip_directives(Ctx, Next);
        {<<"include">>, #{ <<"if">> := false }} -> skip;
        {<<"skip">>, #{ <<"if">> := true }} -> skip;
        {<<"skip">>, #{ <<"if">> := false }} ->
            view_include_skip_directives(Ctx, Next)
    end.

collect_fields(Path, Ctx, Type, SSet) -> collect_fields(Path, Ctx, Type, SSet, #{}).

collect_fields(Path, Ctx, Type, SSet, Visited) ->
    collect_fields(Path, Ctx, Type, SSet, Visited, orddict:new()).
collect_fields(_Path, _Ctx, _Type, [], _Visited, Grouped) ->
    Grouped;
collect_fields(Path, Ctx, Type, [#field{ directives = Dirs } = S |SS], Visited, Grouped) ->
    case view_include_skip_directives(Ctx, Dirs) of
        include ->
            collect_fields(Path, Ctx, Type, SS, Visited,
                           orddict:append(alias(S), S, Grouped));
        skip ->
            collect_fields(Path, Ctx, Type, SS, Visited, Grouped)
    end;
collect_fields(Path, Ctx, Type, [#frag_spread { id = ID, directives = Dirs }|SS], Visited, Grouped) ->
    case view_include_skip_directives(Ctx, Dirs) of
        include ->
            #{ fragments := Frags } = Ctx,
            Name = name(ID),
            %% TODO: Lift this to a function by itself called collect_view_fragment...
            case maps:is_key(Name, Visited) of
                true ->
                    collect_fields(Path, Ctx, Type, SS, Visited, Grouped);
                false ->
                    case maps:get(Name, Frags, not_found) of
                        not_found ->
                            collect_fields(Path, Ctx, Type, SS, Visited#{ Name => true }, Grouped);
                        #frag{} = F ->
                            collect_fields(Path, Ctx, Type, [F | SS], Visited#{ Name => true }, Grouped)
                    end
            end;
        skip ->
            collect_fields(Path, Ctx, Type, SS, Visited, Grouped)
    end;
collect_fields(Path, Ctx, Type, [S |SS], Visited, Grouped) ->
    case S of
        #frag{ id = FragID, selection_set = FragmentSSet, directives = Dirs } = Fragment ->
            case view_include_skip_directives(Ctx, Dirs) of
                include ->
                    case does_fragment_type_apply(Type, Fragment) of
                        false ->
                            collect_fields(Path, Ctx, Type, SS, Visited, Grouped);
                        true ->
                            FragGrouped =
                                collect_fields([name(FragID) | Path], Ctx, Type, FragmentSSet, Visited),
                            Grouped2 = collect_groups(FragGrouped, Grouped),
                            collect_fields(Path, Ctx, Type, SS, Visited, Grouped2)
                    end;
                skip ->
                    collect_fields(Path, Ctx, Type, SS, Visited, Grouped)
            end
    end.

collect_groups([], Grouped) -> Grouped;
collect_groups([{Key, Group}|Next], Grouped) ->
    App = orddict:append_list(Key, Group, Grouped),
    collect_groups(Next, App).

does_fragment_type_apply(
  #object_type { id = ID, interfaces = Implements },
  #frag { schema = FTy }) ->
      case FTy of
          #object_type { id = OID } when OID =:= ID -> true;
          #object_type {} -> false;
          #interface_type { id = IFaceID } -> lists:member(IFaceID, Implements);
          #union_type { types = Types } -> lists:member(ID, Types)
      end.

execute_field(Path, Ctx, ObjType, Value, [F|_] = Fields, #schema_field { annotations = FAns, resolve = RF}) ->
    Name = name(F),
    #schema_field { ty = ElaboratedTy } = field_type(F),
    Args = resolve_args(Ctx, F),
    Fun = resolver_function(ObjType, RF),
    ResolvedValue = resolve_field_value(Ctx, ObjType, Value, Name, FAns, Fun, Args),
    complete_value(Path, Ctx, ElaboratedTy, Fields, ResolvedValue).

resolve_field_value(Ctx, #object_type { id = OID, annotations = OAns} = ObjectType, Value, Name, FAns, Fun, Args) ->
    CtxAnnot = Ctx#{
        field => Name,
        field_annotations => FAns,
        object_type => OID,
        object_annotations => OAns
    },
    try (if
        is_function(Fun, 4) -> Fun(CtxAnnot, Value, Name, Args);
        is_function(Fun, 3) -> Fun(CtxAnnot, Value, Args)
    end) of
        {error, Reason} -> {error, Reason};
        {ok, Result} -> {ok, Result};
        default ->
            resolve_field_value(Ctx, ObjectType, Value, Name, FAns, fun ?MODULE:default_resolver/3, Args);
        Wrong ->
            {error, {wrong_resolver_return, Fun, Name, Wrong}}
    catch
        Cl:Err ->
            error_logger:error_msg(
              "Resolver function error: ~p stacktrace: ~p~n",
              [{Cl,Err}, erlang:get_stacktrace()]),
            {error, {resolver_crash, Fun, ObjectType, Name}}
    end.

complete_value(Path, Ctx, Ty, Fields, {ok, {enum, Value}}) ->
    complete_value(Path, Ctx, Ty, Fields, {ok, Value});
complete_value(Path, Ctx, {scalar, Scalar}, Fields, {ok, Value}) ->
    complete_value(Path, Ctx, output_coerce_type(Scalar), Fields, {ok, Value});
complete_value(Path, Ctx, Ty, Fields, {ok, Value}) when is_binary(Ty) ->
    SchemaType = graphql_schema:get(Ty),
    complete_value(Path, Ctx, SchemaType, Fields, {ok, Value});
complete_value(Path, Ctx, {non_null, InnerTy}, Fields, Result) ->
    %% Note we handle arbitrary results in this case. This makes sure errors
    %% factor through the non-null handler here and that handles
    %% nested {error, Reason} tuples correctly
    case complete_value(Path, Ctx, InnerTy, Fields, Result) of
        {error, Reason} ->
            %% Rule: Along a path, there is at most one error, so if the underlying
            %% object is at fault, don't care too much about this level, just pass
            %% on the error
            err(Path, Reason);
        {ok, null, InnerErrs} ->
            err(Path, null_value, InnerErrs);
        {ok, _C, _E} = V ->
            V
    end;
complete_value(_Path, _Ctx, _Ty, _Fields, {ok, null}) ->
    {ok, null, []};
complete_value(Path, _Ctx, {list, _}, _Fields, {ok, V}) when not is_list(V) ->
    err(Path, not_a_list);
complete_value(Path, Ctx, {list, InnerTy}, Fields, {ok, Value}) ->
    complete_value_list(Path, Ctx, InnerTy, Fields, Value);
complete_value(Path, _Ctx, #scalar_type { id = ID, output_coerce = OCoerce, resolve_module = undefined }, _Fields, {ok, Value}) ->
    try OCoerce(Value) of
        {ok, Result} -> complete_value_scalar(Path, ID, Result);
        {error, Reason} ->
            err(Path, {output_coerce, ID, Value, Reason})
    catch
        Cl:Err ->
            error_logger:error_msg(
              "Output coercer crash: ~p, stack: ~p",
              [{Cl,Err}, erlang:get_stacktrace()]),
            err(Path, {coerce_crash, ID, Value, {Cl, Err}})
    end;
complete_value(Path, _Ctx, #scalar_type { id = ID, resolve_module = RM }, _Fields, {ok, Value}) ->
    try RM:output(ID, Value) of
        {ok, Result} -> complete_value_scalar(Path, ID, Result);
        {error, Reason} ->
            err(Path, {output_coerce, ID, Value, Reason})
    catch
        Cl:Err ->
            error_logger:error_msg(
              "Output coercer crash: ~p, stack: ~p",
              [{Cl,Err,ID,Value}, erlang:get_stacktrace()]),
            err(Path, {coerce_crash, ID, Value, {Cl, Err}})
    end;
complete_value(_Path, _Ctx, #enum_type {}, _Fields, {ok, Value}) when is_binary(Value) ->
    %% TODO: Coercion handling for enumerated values!
    {ok, Value, []};
complete_value(_Path, _Ctx, #enum_type { values = Values }, _Fields, {ok, Value}) when is_integer(Value) ->
    %% TODO: Coercion handling for enumerated values!
    #enum_value { val = Result } = maps:get(Value, Values),
    {ok, Result, []};
complete_value(Path, Ctx, #interface_type{ resolve_type = Resolver }, Fields, {ok, Value}) ->
    {ok, ResolvedType} = resolve_abstract_type(Resolver, Value),
    complete_value(Path, Ctx, ResolvedType, Fields, {ok, Value});
complete_value(Path, Ctx, #union_type{ resolve_type = Resolver }, Fields, {ok, Value}) ->
    {ok, ResolvedType} = resolve_abstract_type(Resolver, Value),
    complete_value(Path, Ctx, ResolvedType, Fields, {ok, Value});

complete_value(Path, Ctx, #object_type{} = Ty, Fields, {ok, Value}) ->
    SubSelectionSet = merge_selection_sets(Fields),
    {Result, Errs} = execute_sset(Path, Ctx, SubSelectionSet, Ty, Value),
    {ok, Result, Errs};
complete_value(Path, _Ctx, _Ty, _Fields, {error, Reason}) ->
    {ok, null, [#{ path => Path, reason => Reason }]}.

resolve_abstract_type(Module, Value) when is_atom(Module) ->
    resolve_abstract_type(fun Module:execute/1, Value);
resolve_abstract_type(Resolver, Value) when is_function(Resolver, 1) ->
    try Resolver(Value) of
        {ok, Ty} ->
            Obj = #object_type{} = graphql_schema:get(binarize(Ty)),
            {ok, Obj};
        {error, Reason} ->
            {error, Reason}
    catch
       Cl:Err ->
            error_logger:error_msg(
              "Resolve_type crashed: ~p",
              [erlang:get_stacktrace()]),
           {error, {resolve_type_crash, {Cl,Err}}}
    end.

complete_value_scalar(_Path, _ID, Result) when is_binary(Result) -> {ok, Result, []};
complete_value_scalar(_Path, _ID, Result) when is_number(Result) -> {ok, Result, []};
complete_value_scalar(_Path, _ID, true) -> {ok, true, []};
complete_value_scalar(_Path, _ID, false) -> {ok, false, []};
complete_value_scalar(_Path, _ID, null) -> {ok, null, []};
complete_value_scalar( Path,  ID, Value) -> 
    err(Path, {output_coerce, ID, Value, not_scalar_type}).


complete_value_list(Path, Ctx, Ty, Fields, Results) ->
    IndexedResults = index(Results),
    Completed = [{I, complete_value([I | Path], Ctx, Ty, Fields, R)} || {I, R} <- IndexedResults],
    case complete_list_value_result(Completed) of
        {error, Reasons} ->
            {ok, null, Reasons};
        {ok, L} ->
            {Vals, Errs} = lists:unzip(L),
            Len = length(Completed),
            Len = length(Vals),
            {ok, Vals, lists:concat(Errs)}
    end.

complete_list_value_result(Completed) ->
    case lists:any(
           fun
               ({_, {error, _}}) -> true;
               (_) -> false
           end,
           Completed) of
        true -> {error, lists:concat([R || {_, {error, R}} <- Completed])};
        false -> {ok, [{V, Es} || {_, {ok, V, Es}} <- Completed]}
    end.

index([]) -> [];
index(L) -> lists:zip(lists:seq(0, length(L)-1), L).

find_operation(_N, []) -> not_found;
find_operation(N, [#op { id = ID } = O | Next]) ->
    case name(ID) of
        N -> O;
        _ -> find_operation(N, Next)
    end.

merge_selection_sets(Fields) ->
    F = fun
        (#field { selection_set = [] }, Acc) -> Acc;
        (#field { selection_set = SS }, Acc) -> [SS|Acc]
    end,
    lists:concat(
        lists:foldl(F, [], Fields)).

get_operation(#{ operation_name := undefined }, [Op]) ->
    {ok, Op};
get_operation(#{ operation_name := undefined }, _) ->
    {error, more_than_one_operation};
get_operation(#{ operation_name := OpName }, Ops) ->
    case find_operation(OpName, Ops) of
        not_found ->
            {error, {operation_not_found, OpName}};
        Op ->
            {ok, Op}
    end;
get_operation(#{} = Ctx, Ops) ->
    get_operation(Ctx#{ operation_name => undefined }, Ops).


%% -- FRAGMENTS -------------------------------
fragments(Frags) ->
    lists:foldl(fun(#frag { id = ID } = Frag, St) ->
        St#{ name(ID) => Frag }
    end,
    #{},
    Frags).

%% -- FUNCTION RESOLVERS ---------------------------------

resolver_function(_ObjType, R) when is_function(R, 3) -> R;
resolver_function(#object_type { resolve_module = undefined }, undefined) ->
    fun ?MODULE:default_resolver/3;
resolver_function(#object_type { resolve_module = M }, undefined) ->
    fun M:execute/4.


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
        V when is_list(V) -> {ok, [ {ok, R} || R <- V ]};
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

%% -- LOWER LEVEL RESOLVERS ----------------

resolve_args(Ctx, #directive { args = As }) ->
    resolve_args_(Ctx, As, #{});
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
value(Ctx, #{ type := {list, Ty}, value := {list, Vals}}) ->
    [value(Ctx, {Ty, V}) || V <- Vals];
value(Ctx, #{ type := {list, Ty}, value := Vals}) when is_list(Vals) ->
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
    case graphql_schema:get(graphql_ast:unwrap_type(ObjTy)) of
        #input_object_type { fields = FieldEnv } ->
            ObjVals = value_object(Ctx, FieldEnv, maps:to_list(O)),
            maps:from_list(ObjVals);
        #scalar_type{} ->
            %% Input coercion has produced a scalar type. In this
            %% case, we just let the given scalar coercion through
            O
    end;
value(#{ params := Params }, #{ value := {var, ID}}) ->
    maps:get(name(ID), Params);
value(_Ctx, #{ type := {scalar, STy}, value := V}) ->
    value_scalar(STy, V);
value(Ctx, #{ type := _, value := V} = M) ->
    default_resolution(Ctx, M),
    V.

default_resolution(_Ctx, _M) ->
    phone_home.

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
    error_logger:error_msg(
      "Report this: Scalar resolver missing for built-in type: ~p (value: ~p)~n",
      [Ty, V]),
    V.

value_object(_, _, []) -> [];
value_object(Ctx, FieldEnv, [{K, Val} | Rest]) ->
    Name = name(K),
    #schema_arg { ty = Ty } = maps:get(Name, FieldEnv),
    Value = value(Ctx, {Ty, Val}),
    [{Name, Value} | value_object(Ctx, FieldEnv, Rest)].

%% -- AST MANIPULATION ----------------

name(N) when is_binary(N) -> N;
name({name, _, N}) -> N;
name('...') -> <<"...">>;
name(#field { id = ID }) -> name(ID).

alias(#field { alias = undefined, id = ID }) -> name(ID);
alias(#field { alias = Alias }) -> name(Alias).

field_type(#field { schema = SF }) -> SF.

%% -- ERROR HANDLING --

err(Path, Reason) ->
    err(Path, Reason, []).

err(Path, Reason, More) when is_list(More) ->
    {error, [#{ path => Path, reason => Reason} | More]}.


%% -- CONTEXT CANONICALIZATION ------------
canon_context(#{ params := Params } = Ctx) ->
     Ctx#{ params := canon_params(Params) }.

canon_params(Ps) ->
     KVs = maps:to_list(Ps),
     maps:from_list([{binarize(K), V} || {K, V} <- KVs]).

binarize(A) when is_atom(A) -> atom_to_binary(A, utf8);
binarize(B) when is_binary(B) -> B;
binarize(L) when is_list(L) -> list_to_binary(L).
