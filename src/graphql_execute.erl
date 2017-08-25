-module(graphql_execute).

-include("graphql_internal.hrl").
-include("graphql_schema.hrl").

-export([x/1, x/2]).
-export([default_resolver/3]).
-export([err_msg/1]).
-export([builtin_input_coercer/1]).

-define(DEFER_TIMEOUT, 5000). %% @todo: Get rid of this timeout. It is temporary

-type source() :: reference().
-type target() :: reference().

-type defer_closure() ::
        fun ((source(), term()) ->
                    {done, target(), term(), term()} | {more, defer_closure()}).

-record(defer_state,
        { req_id :: reference(),
          work = #{} :: #{ reference() => defer_closure() } }).

-spec x(graphql:ast()) -> #{ atom() => graphql:json() }.
x(X) -> x(#{ params => #{} }, X).

-spec x(term(), graphql:ast()) -> #{ atom() => graphql:json() }.
x(Ctx, X) ->
    Canon = canon_context(Ctx),
    execute_request(Canon, X).

execute_request(InitialCtx, {document, Operations}) ->
    {Frags, Ops} = lists:partition(fun (#frag {}) -> true;(_) -> false end, Operations),
    Ctx = InitialCtx#{ fragments => fragments(Frags),
                       defer_request_id => make_ref() },
    case get_operation(Ctx, Ops) of
        {ok, #op { ty = {query, _} } = Op } ->
            execute_query(Ctx#{ op_type => query }, Op);
        {ok, #op { ty = undefined } = Op } ->
            execute_query(Ctx#{ op_type => query }, Op);
        {ok, #op { ty = {mutation, _} } = Op } ->
            execute_mutation(Ctx#{ op_type => mutation }, Op);
        {error, Reason} ->
            {error, Errs} = err([], Reason),
            complete_top_level(undefined, Errs)
    end.

complete_top_level(undefined, Errs) when is_list(Errs) ->
    #{ errors => [complete_error(E) || E <- Errs ] };
complete_top_level(Res, []) ->
    Aux = collect_auxiliary_data(),
    Result = #{ data => Res },
    decorate_top_level(Result, aux, Aux);
complete_top_level(Res, Errs) ->
    Errors = [complete_error(E) || E <- Errs ],
    Aux = collect_auxiliary_data(),
    Result = #{ data => Res, errors => Errors },
    decorate_top_level(Result, aux, Aux).

complete_error(#{ path := Path, reason := Reason }) ->
    graphql_err:mk(Path, execute, Reason).

decorate_top_level(Map, _, []) -> Map; % noop
decorate_top_level(Map, aux, AuxiliaryDataList) ->
    Map#{aux => AuxiliaryDataList}.

collect_auxiliary_data() ->
    receive
        {'$auxiliary_data', AuxiliaryDataList} ->
            AuxiliaryDataList ++ collect_auxiliary_data()
    after 0 ->
        []
    end.

execute_query(#{ defer_request_id := ReqId } = Ctx, #op { selection_set = SSet,
                         schema = QType } = Op) ->
    #object_type{} = QType,
    case execute_sset([graphql_ast:id(Op)], Ctx#{ defer_process => self(),
                                                  defer_target => top_level },
                      SSet, QType, none) of
        {ok, Res, Errs} ->
            complete_top_level(Res, Errs);
        {work, WL} ->
            DeferState = #defer_state{ req_id = ReqId,
                                       work = maps:from_list(WL) },
            defer_loop(DeferState)
    end.

execute_mutation(Ctx, #op { selection_set = SSet,
                            schema = QType } = Op) ->
    #object_type{} = QType,
    case execute_sset([graphql_ast:id(Op)], Ctx#{ defer_process => self(),
                                                  defer_target => top_level },
                      SSet, QType, none) of
        %% In mutations, there is no way you can get deferred work
        %% So we just ignore the case here. If it ever occurs with a
        %% case clause bug here, it is a broken invariant.
        {ok, Res, Errs} ->
            complete_top_level(Res, Errs)
    end.

execute_sset(Path, #{ defer_target := Upstream } = Ctx, SSet, Type, Value) ->
    GroupedFields = collect_fields(Path, Ctx, Type, SSet),
    Self = make_ref(),
    try
        case execute_sset_field(Path, Ctx#{ defer_target => Self }, GroupedFields, Type, Value) of
            {ok, Map, Errs} ->
                {ok, Map, Errs};
            {defer, Map, Errs, Work, Missing} ->
                Closure = sset_closure(Upstream, Self, Missing, Map, Errs),
                {work, [{Self, Closure}] ++ Work}
        end
    catch
        throw:{null, Errors, _Ds} ->
            %% @todo: Cancel defers, or at least consider a way of doing so!
            {ok, null, Errors}
    end.

sset_closure(Upstream, Self, Missing, Map, Errors) ->
    fun
        (FieldRef, Completed, Errs) ->
            case maps:take(FieldRef, Missing) of
                {Name, NewMissing} ->
                    NewMap = Map#{ Name => Completed },
                    NewErrors = Errs ++ Errors,
                    case maps:size(NewMissing) of
                        0 ->
                            {done, Upstream, NewMap, NewErrors};
                        _ ->
                            error_logger:info_msg("Need more: ~p", [NewMissing]),
                            {more, [{Self,
                                     sset_closure(Upstream, Self,
                                                  NewMissing,
                                                  NewMap,
                                                  NewErrors)}]}
                    end
            end
    end.

execute_sset_field(Path, Ctx, Fields, Type, Value) ->
    Map = [],
    Errs = [],
    Work = [],
    Missing = #{},
    execute_sset_field(Path, Ctx, Fields, Type, Value, Map, Errs, Work, Missing).

execute_sset_field(_Path, _Ctx, [], _Type, _Value, Map, Errs, Work, Missing) ->
    Result = maps:from_list(lists:reverse(Map)),
    case Work of
        [] ->
            0 = maps:size(Missing),
            {ok, Result, Errs};
        [_|_] ->
            true = maps:size(Missing) > 0,
            {defer, Result, Errs, Work, Missing}
    end;
execute_sset_field(Path, Ctx, [{Key, [F|_] = Fields} | Next],
             Type, Value, Map, Errs, Work, Missing) ->
    case lookup_field(F, Type) of
        null ->
            execute_sset_field(Path, Ctx, Next, Type, Value, Map, Errs, Work, Missing);
        not_found ->
            execute_sset_field(Path, Ctx, Next, Type, Value, Map, Errs, Work, Missing);
        typename ->
            execute_sset_field(Path, Ctx, Next, Type, Value,
                               [{Key, typename(Type)} | Map],
                               Errs, Work, Missing);
        FieldType ->
            case execute_field([Key | Path], Ctx, Type, Value, Fields, FieldType) of
                {ok, Result, FieldErrs} ->
                    execute_sset_field(Path, Ctx, Next, Type, Value,
                                       [{Key, Result} | Map],
                                       FieldErrs ++ Errs,
                                       Work, Missing);
                {work, WUs} ->
                    execute_sset_field(Path, Ctx, Next, Type, Value, Map, Errs,
                                       WUs ++ Work, add_missing(WUs, Key, Missing));
                {error, Errors} ->
                    throw({null, Errors, Work})
            end
    end.

add_missing([{Ref, _} | _], Key, Missing) ->
    Missing#{ Ref => Key }.

typename(#object_type { id = ID }) -> ID.

lookup_field(#field { id = ID }, Obj) ->
    lookup_field_name(name(ID), Obj).

lookup_field_name(<<"__typename">>, _) -> typename;
lookup_field_name(N, #object_type { fields = FS }) ->
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

execute_field_await(Path,
                    #{ defer_request_id := ReqId } = Ctx,
                    ElaboratedTy,
                    Fields,
                    Ref) ->
    receive
        {'$graphql_reply', ReqId, Ref, ResolvedValue} ->
            complete_value(Path, Ctx, ElaboratedTy, Fields, ResolvedValue);
        {'$graphql_reply', _, _, _} = Reply ->
            error_logger:info_msg("Ignoring old reply: ~p", [Reply]),
            execute_field_await(Path, Ctx, ElaboratedTy, Fields, Ref)
    after 750 ->
            exit(defer_mutation_timeout)
    end.

execute_field(Path, #{ defer_target := Upstream,
                       op_type := OpType } = Ctx,
              ObjType, Value, [F|_] = Fields,
              #schema_field { annotations = FAns, resolve = RF}) ->
    Name = name(F),
    #schema_field { ty = ElaboratedTy } = field_type(F),
    Args = resolve_args(Ctx, F),
    Fun = resolver_function(ObjType, RF),
    case resolve_field_value(Ctx, ObjType, Value, Name, FAns, Fun, Args) of
        {defer, Token} when OpType == mutation ->
            %% A mutation must not run the mutation in the parallel, so it awaits
            %% the data straight away
            Ref = graphql:token_ref(Token),
            execute_field_await(Path, Ctx, ElaboratedTy, Fields, Ref);
        {defer, Token} ->
            Ref = graphql:token_ref(Token),
            Closure =
                fun
                    (ResVal) ->
                        case complete_value(Path, Ctx, ElaboratedTy, Fields, ResVal) of
                            {ok, Result, Errs} ->
                                {done, Upstream, Result, Errs}
                        end
                end,
            {work, [{Ref, Closure}]};
        ResolvedValue ->
            complete_value(Path, Ctx, ElaboratedTy, Fields, ResolvedValue)
    end.

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
        {error, Reason} -> {error, {resolver_error, Reason}};
        {ok, Result} -> {ok, Result};
        {ok, Result, AuxiliaryDataList} when is_list(AuxiliaryDataList) ->
            self() ! {'$auxiliary_data', AuxiliaryDataList},
            {ok, Result};
        {defer, Token} -> {defer, Token};
        default ->
            resolve_field_value(Ctx, ObjectType, Value, Name, FAns, fun ?MODULE:default_resolver/3, Args);
        Wrong ->
            error_logger:error_msg(
              "Resolver returned wrong value: ~p(..) -> ~p",
              [Fun, Wrong]),
            {error, {wrong_resolver_return, graphql_schema:id(ObjectType), Name}}
    catch
        Cl:Err ->
            error_logger:error_msg(
              "Resolver function error: ~p stacktrace: ~p~n",
              [{Cl,Err}, erlang:get_stacktrace()]),
            {error, {resolver_crash, graphql_schema:id(ObjectType), Name}}
    end.

complete_value(Path, Ctx, Ty, Fields, {ok, {enum, Value}}) ->
    complete_value(Path, Ctx, Ty, Fields, {ok, Value});

complete_value(Path, Ctx, {scalar, Scalar}, Fields, {ok, Value}) ->
    complete_value(Path, Ctx, scalar_resolve(Scalar), Fields, {ok, Value});

complete_value(Path, Ctx, Ty, Fields, {ok, Value}) when is_binary(Ty) ->
    error_logger:warning_msg(
      "Canary: Type lookup during value completion for: ~p~n",
      [Ty]),
    SchemaType = graphql_schema:get(Ty),
    complete_value(Path, Ctx, SchemaType, Fields, {ok, Value});

complete_value(Path, #{ defer_target := Upstream } = Ctx,
               {non_null, InnerTy}, Fields, Result) ->
    %% Note we handle arbitrary results in this case. This makes sure errors
    %% factor through the non-null handler here and that handles
    %% nested {error, Reason} tuples correctly
    Self = make_ref(),
    case complete_value(Path, Ctx#{ defer_target := Self}, InnerTy, Fields, Result) of
        {error, Reason} ->
            %% Rule: Along a path, there is at most one error, so if the underlying
            %% object is at fault, don't care too much about this level, just pass
            %% on the error
            err(Path, Reason);
        {ok, null, InnerErrs} ->
            err(Path, null_value, InnerErrs);
        {ok, _C, _E} = V ->
            V;
        {work, WUs} ->
            %% This closure wraps the null properly and errors null-returns
            %% From the underlying computation if it completes later on with a
            %% null value.
            Closure =
                fun
                    (FieldRef, null, InnerErrs) when FieldRef == Self ->
                        Err = err(Path, null_value, InnerErrs),
                        {done, Upstream, Err, []};
                    (FieldRef, Completed, InnerErrs) when FieldRef == Self ->
                        {done, Upstream, Completed, InnerErrs}
                end,
            {work, [{Self, Closure}|WUs]}
    end;
complete_value(_Path, _Ctx, _Ty, _Fields, {ok, null}) ->
    {ok, null, []};

complete_value(Path, _Ctx, {list, _}, _Fields, {ok, V}) when not is_list(V) ->
    err(Path, not_a_list);

complete_value(Path, Ctx, {list, InnerTy}, Fields, {ok, Value}) ->
    complete_value_list(Path, Ctx, InnerTy, Fields, Value);

complete_value(Path, _Ctx, #scalar_type { id = ID, resolve_module = RM }, _Fields, {ok, Value}) ->
    complete_value_scalar(Path, ID, RM, Value);

complete_value(_Path, _Ctx, #enum_type { id = ID, resolve_module = RM}, _Fields, {ok, Value}) ->
    %% the enums are scalars too.
    complete_value_scalar(_Path, ID, RM, Value);

complete_value(Path, Ctx, #interface_type{ resolve_type = Resolver }, Fields, {ok, Value}) ->
    complete_value_abstract(Path, Ctx, Resolver, Fields, {ok, Value});
complete_value(Path, Ctx, #union_type{ resolve_type = Resolver }, Fields, {ok, Value}) ->
    complete_value_abstract(Path, Ctx, Resolver, Fields, {ok, Value});
complete_value(Path, Ctx, #object_type{} = Ty, Fields, {ok, Value}) ->
    SubSelectionSet = merge_selection_sets(Fields),
    execute_sset(Path, Ctx, SubSelectionSet, Ty, Value);
complete_value(Path, _Ctx, _Ty, _Fields, {error, Reason}) ->
    {error, ErrList} = err(Path, Reason),
    {ok, null, ErrList}.

%% Complete an abstract value
complete_value_abstract(Path, Ctx, Resolver, Fields, {ok, Value}) ->
    case resolve_abstract_type(Resolver, Value) of
        {ok, ResolvedType} ->
            complete_value(Path, Ctx, ResolvedType, Fields, {ok, Value});
        {error, Reason} ->
            err(Path, Reason)
    end.

resolve_abstract_type(Module, Value) when is_atom(Module) ->
    resolve_abstract_type(fun Module:execute/1, Value);
resolve_abstract_type(Resolver, Value) when is_function(Resolver, 1) ->
    try Resolver(Value) of
        {ok, Ty} ->
            Obj = #object_type{} = graphql_schema:get(binarize(Ty)),
            {ok, Obj};
        {error, Reason} ->
            {error, {type_resolver_error, Reason}}
    catch
       Cl:Err ->
            error_logger:error_msg(
              "Type resolver crashed: ~p stacktrace: ~p~n",
              [{Cl,Err}, erlang:get_stacktrace()]),
           {error, {resolve_type_crash, {Cl,Err}}}
    end.

complete_value_scalar(Path, ID, RM, Value) ->
    try RM:output(ID, Value) of
        {ok, Result} ->
            {ok, Result, []};

        {error, Reason} ->
            err(Path, {output_coerce, ID, Value, Reason})
    catch
        Cl:Err ->
            error_logger:error_msg(
              "Output coercer crash during value completion: ~p, stacktrace: ~p~n",
              [{Cl,Err,ID,Value}, erlang:get_stacktrace()]),
            err(Path, {output_coerce_abort, ID, Value, {Cl, Err}})
    end.

assert_list_completion_structure(Ty, Fields, Results) ->
    ValidResult =
        fun
            ({_I, {ok, _}}) -> true;
            ({_I, {error, _}}) -> true;
            ({_I, {defer, _}}) -> true;
            ({_I, _}) -> false
        end,
    {_Ok, Fail} = lists:partition(ValidResult, Results),
    case Fail of
        [] ->
            ok;
        [{_, R}|_] = Errs ->
            Name = graphql_ast:typename(Ty),
            Field = graphql_ast:id(hd(Fields)),
            error_logger:error_msg(
              "Error in resolver function: (Object.Field) ~ts.~ts: "
              "the result ~p doesn't follow the valid list form of "
              "{ok, _} | {error, _} (~B errors in total)~n",
              [Name, Field, R, length(Errs)]),
            {error, list_resolution}
    end.

complete_value_list(Path, #{ defer_target := Upstream } = Ctx, Ty, Fields, Results) ->
    IndexedResults = index(Results),
    case assert_list_completion_structure(Ty, Fields, IndexedResults) of
        {error, list_resolution} ->
            {error, Errs} = err(Path, list_resolution),
            {ok, null, Errs};
        ok ->
            Self = make_ref(),
            InnerCtx = Ctx#{ defer_target := Self },
            Completer =
                fun
                    F([]) -> {[], [], #{}};
                    F([{Index, Result}|Next]) ->
                        {Rest, WUs, Missing} = F(Next),
                        case complete_value([Index|Path], InnerCtx, Ty, Fields, Result) of
                            {ok, V, Errs} ->
                                { [{ok, V, error_wrap(Errs)} | Rest],
                                  WUs,
                                  Missing };
                            {error, Err} ->
                                { [{error, Err}| Rest],
                                  WUs,
                                  Missing };
                            {work, [{Ref, _Closure}|_] = Work} ->
                                { [{defer, Ref} | Rest],
                                  Work ++ WUs,
                                  Missing#{ Ref => Index } }
                        end
                end,
            {Completed, Ws, M} = Completer(IndexedResults),
            case maps:size(M) of
                0 ->
                    case complete_list_value_result(Completed) of
                        {Res, []} ->
                            {Vals, Errs} = lists:unzip(Res),
                            Len = length(Completed),
                            Len = length(Vals),
                            {ok, Vals, lists:concat(Errs)};
                        {_, Reasons} ->
                            {ok, null, Reasons}
                    end;
                _ ->
                    Closure = list_closure(Upstream, Self, M, Completed, #{}),
                    {work, [{Self, Closure}|Ws]}
            end
    end.

list_subst([], _Done)               -> [];
list_subst([{defer, Ref}|Xs], Done) -> [maps:get(Ref, Done)|list_subst(Xs, Done)];
list_subst([X|Xs], Done)            -> [X|list_subst(Xs, Done)].

list_closure(Upstream, Self, Missing, List, Done) ->
    fun
        (FieldRef, Completed, Errs) ->
            case maps:take(FieldRef, Missing) of
                {_Index, NewMissing} ->
                    NewDone = Done#{ FieldRef => {ok, Completed, Errs} },
                    case maps:size(NewMissing) of
                        0 ->
                            SubstList = list_subst(List, NewDone),
                            case complete_list_value_result(SubstList) of
                                {Res, []} ->
                                    {Vs, Es} = lists:unzip(Res),
                                    Len = length(SubstList),
                                    Len = length(Vs),
                                    {done, Upstream, Vs, lists:concat(Es)};
                                {_, Reasons} ->
                                    {done, Upstream, null, Reasons}
                            end;
                        _ ->
                            {more, [{Self,
                                     list_closure(Upstream, Self,
                                                  NewMissing,
                                                  List,
                                                  NewDone )}]}
                    end
            end
    end.

complete_list_value_result([]) ->
    {[], []};
complete_list_value_result([{error, Err}|Next]) ->
    {Res, Errs} = complete_list_value_result(Next),
    {Res, Err ++ Errs};
complete_list_value_result([{ok, V, Es}|Next]) ->
    {Res, Errs} = complete_list_value_result(Next),
    {[{V, Es} | Res], Errs}.

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
            {error, field_not_found};
        V when is_list(V) -> {ok, [ {ok, R} || R <- V ]};
        V -> {ok, V}
    end.

%% -- OUTPUT COERCION ------------------------------------

scalar_resolve(id) ->
    #scalar_type { id = <<"ID">>,
                   description = <<"Builtin output coercer type">>,
                   resolve_module = graphql_scalar_binary_coerce
                   };
scalar_resolve(string) ->
    #scalar_type { id = <<"String">>,
                   description = <<"Builtin output coercer type">>,
                   resolve_module = graphql_scalar_binary_coerce
                 };
scalar_resolve(bool) ->
    #scalar_type { id = <<"Bool">>,
                   description = <<"Builtin output coercer type">>,
                   resolve_module = graphql_scalar_bool_coerce
                 };
scalar_resolve(int) ->
    #scalar_type { id = <<"Int">>,
                   description = <<"Builtin output coercer type">>,
                   resolve_module = graphql_scalar_integer_coerce
                 };
scalar_resolve(float) ->
    #scalar_type { id = <<"Float">>,
                   description = <<"Builtin output coercer type">>,
                   resolve_module = graphql_scalar_float_coerce
                 };
scalar_resolve(#scalar_type{} = SType) -> SType;
scalar_resolve(UserDefined) when is_binary(UserDefined) ->
    case graphql_schema:lookup(UserDefined) of
        #scalar_type{} = SType -> SType;
        not_found -> not_found
    end.

builtin_input_coercer(X) ->
    {ok, X}.

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
value(Ctx, {Ty, Val})                     -> value(Ctx, Ty, Val);
value(Ctx, #{ type := Ty, value := Val }) -> value(Ctx, Ty, Val).

value(#{ params := Params } = _Ctx, _Ty, {var, ID}) ->
    %% Parameter expansion and type check is already completed
    %% at this stage
    maps:get(name(ID), Params);
value(_Ctx, _Ty, null) ->
    null;
value(Ctx, {non_null, Ty}, Val) ->
    value(Ctx, Ty, Val);
value(Ctx, {list, Ty}, Val) ->
    Vals = case Val of
               {list, L} -> L;
               L when is_list(L)  -> L
           end,
    [value(Ctx, Ty, V) || V <- Vals];
value(Ctx, Ty, Val) ->
    case Ty of
        #input_object_type { fields = FieldEnv } ->
            Obj = case Val of
                      {object, O} -> O;
                      O when is_map(O) -> maps:to_list(O)
                  end,
            ObjVals = value_object(Ctx, FieldEnv, Obj),
            maps:from_list(ObjVals);
        #scalar_type{} ->
            %% At this point, scalar conversion has happened earlier, so any
            %% erlang term is a value scalar value. Just return the value:
            Val;
        #enum_type {} ->
            Val;
        {scalar, ScalarTy} ->
            %% Built-in scalars are currently handled specially
            value_scalar(ScalarTy, Val);
        Bin when is_binary(Bin) ->
            LoadedTy = graphql_schema:get(Bin),
            value(Ctx, LoadedTy, Val)
    end.

value_scalar(string, null) -> null;
value_scalar(string, S) when is_binary(S) -> S;
value_scalar(id, null) -> null;
value_scalar(id, S) when is_binary(S) -> S;
value_scalar(bool, true) -> true;
value_scalar(bool, false) -> false;
value_scalar(int, I) when is_integer(I) -> I;
value_scalar(int, null) -> null;
value_scalar(float, F) when is_float(F) -> F;
value_scalar(float, I) when is_integer(I) -> float(I);
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

%% -- CONTEXT CANONICALIZATION ------------
canon_context(#{ params := Params } = Ctx) ->
     Ctx#{ params := canon_params(Params) }.

canon_params(Ps) ->
     KVs = maps:to_list(Ps),
     maps:from_list([{binarize(K), V} || {K, V} <- KVs]).

binarize(A) when is_atom(A) -> atom_to_binary(A, utf8);
binarize(B) when is_binary(B) -> B;
binarize(L) when is_list(L) -> list_to_binary(L).

%% -- DEFERRED PROCESSING --

%% Process deferred computations by grabbing them in the mailbox
defer_loop(#defer_state { req_id = Id } = State) ->
    receive
        {'$graphql_reply', Id, Ref, Data} ->
            error_logger:info_msg("Handling ~p", [{Id, Ref, Data}]),
            defer_handle_work(State, Ref, Data);
        {'$graphql_reply', _, _, _} = Reply ->
            %% Ignoring old request
            error_logger:info_msg("Ignoring old reply: ~p", [Reply]),
            defer_loop(State)
    after 750 ->
            exit(defer_timeout)
    end.

%% A deferred closure has a different structure depending on what kind of input data
%% it will accept. This function mediates through the different variants of calls which
%% are possible.
%% @todo: Handle Errs in a more graceful manner!
call_closure(Closure, {Source, Res, Errs}) -> Closure(Source, Res, Errs);
call_closure(Closure, Data) -> Closure(Data).

%% Process work
defer_handle_work(#defer_state {work = WorkMap } = State,
                  Target,
                  Input) ->
    case maps:take(Target, WorkMap) of
        error ->
            error_logger:info_msg("NOT FOUND! workmap: ~p", [WorkMap]),
            defer_loop(State);
        {Closure, WorkMap2} ->
            Result = call_closure(Closure, Input),
            error_logger:info_msg("Result from ~p: ~p", [Target, Result]),
            case Result of
                {done, top_level, Res, Errs} ->
                    complete_top_level(Res, Errs);
                {done, Upstream, Res, Errs} ->
                    defer_handle_work(
                      State#defer_state { work = WorkMap2 },
                      Upstream,
                      {Target, Res, Errs});
                {more, New} ->
                    NewWork = maps:from_list(New),
                    defer_loop(State#defer_state { work = maps:merge(WorkMap2, NewWork) })
            end
    end.

%% -- ERROR HANDLING --

error_wrap([]) -> [];
error_wrap([#{ reason := Reason } = E | Next]) ->
    [E#{ reason => {resolver_error, Reason} } | error_wrap(Next)].

err(Path, Reason) ->
    err(Path, Reason, []).

err(Path, Reason, More) when is_list(More) ->
    {error, [#{ path => Path,
                reason => Reason} | More]}.

err_msg(null_value) ->
    ["The schema specifies the field is non-null, "
     "but a null value was returned by the backend"];
err_msg(not_a_list) ->
    ["The schema specifies the field is a list, "
     "but a non-list value was returned by the backend"];
err_msg({output_coerce, ID, _Value, Reason}) ->
    io_lib:format("Output coercion failed for type ~s with reason ~p",
                  [ID, Reason]);
err_msg({operation_not_found, OpName}) ->
    ["The operation ", OpName, " was not found in the query document"];
err_msg({type_resolver_error, Err}) ->
    io_lib:format("~p", [Err]);
err_msg({resolver_error, Err}) ->
    io_lib:format("~p", [Err]);
err_msg({output_coerce_abort, _ID, _Value, _}) ->
    ["Internal Server error: output coercer function crashed"];
err_msg(list_resolution) ->
    ["Internal Server error: A list is being incorrectly resolved"];
err_msg(Otherwise) ->
    io_lib:format("Error in execution: ~p", [Otherwise]).

