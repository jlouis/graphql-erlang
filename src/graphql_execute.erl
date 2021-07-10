-module(graphql_execute).

-include_lib("graphql/include/graphql.hrl").
-include("graphql_internal.hrl").
-include("graphql_schema.hrl").

-compile(inline).
-compile(inline_list_funcs).
-compile({inline_size, 50}).
-compile({no_auto_import,[alias/1]}).

-export([x/1, x/2]).
-export([builtin_input_coercer/1]).
-type source() :: reference().
-type demonitor() :: {reference(), pid()} .

%% Execution context
%%
%% This record is the context we have under execution, shortened to ectx.
-record(ectx,
        {
         %% What type of operation are we executing
         op_type = query :: query | mutation | subscription,

         %% Track where we are in the response document. Use for error reporting
         path = [] :: [any()],

         %% Params given for the execution (already assumed to be checked)
         params = #{} :: #{ binary() => term() },

         %% Fragments in the execution context. Used for fragment expansion.
         frags = #{} :: #{ binary() =>  term() },

         %% Unique request reference for this particular query
         %% Used to discriminate this for older, stale queries injecting
         %% commands into our current processing loop.
         defer_request_id = undefined :: undefined | reference(),

         %% Who is the target of the defer. Currently always self().
         defer_process = undefined :: undefined | pid(),

         %% If building a defer closure at this point in execution,
         %% which closure should get the result upstream in the result
         %% tree? Updated as we process different parts of the tree
         defer_target = top_level :: top_level | reference(),

         %% The callers context given to the execute call
         ctx = #{} :: #{ atom() => term() }
        }).
-type ectx() :: #ectx{}.

-record(done,
        { upstream :: source() | top_level,
          key :: reference(),
          cancel :: [reference()],
          demonitor :: demonitor() | undefined,
          result :: ok | {ok, term(), [term()]} | {error, [term()]} }).

-record(work,
        { items :: [{reference(), defer_closure()}],
          monitor :: #{ reference() => reference() },
          demonitors :: [demonitor()],
          timeout = 0 :: non_neg_integer(),
          change_ref = undefined :: undefined
                                  | {reference(), reference(), reference()} }).

-type defer_closure() ::
        fun ((term()) -> #done{}
                       | #work{}).

-record(defer_state,
        { req_id :: source(),
          canceled = [] :: [reference()],
          monitored :: #{ reference() => reference() },
          work = #{} :: #{ source() => defer_closure() },
          timeout :: non_neg_integer() }).

-spec x(graphql:ast()) -> #{ atom() => graphql:json() }.
x(X) -> x(#{ params => #{} }, X).

-spec x(term(), graphql:ast()) -> #{ atom() => graphql:json() }.
x(Ctx, X) ->
    Canon = canon_context(Ctx),
    execute_request(Canon, X).

execute_request(InitialCtx, #document { definitions = Operations }) ->
    {Frags, Ops} = lists:partition(fun (#frag {}) -> true;(_) -> false end, Operations),
    Ctx = InitialCtx#ectx{ frags = fragments(Frags),
                           defer_request_id = make_ref() },
    case get_operation(Ctx, Ops) of
        {ok, #op { ty = {query, _} } = Op } ->
            execute_query(Ctx#ectx{ op_type = query }, Op);
        {ok, #op { ty = {subscription, _} } = Op } ->
            execute_query(Ctx#ectx{ op_type = subscription }, Op);
        {ok, #op { ty = undefined } = Op } ->
            execute_query(Ctx#ectx{ op_type = query }, Op);
        {ok, #op { ty = {mutation, _} } = Op } ->
            execute_mutation(Ctx#ectx{ op_type = mutation }, Op);
        {error, Reason} ->
            {error, Errs} = err(Ctx, Reason),
            complete_top_level(undefined, Errs)
    end.

complete_top_level(undefined, Errs) when is_list(Errs) ->
    #{ errors => Errs };
complete_top_level(Res, []) ->
    Aux = collect_auxiliary_data(),
    Result = #{ data => Res },
    decorate_top_level(Result, aux, Aux);
complete_top_level(Res, Errs) ->
    Aux = collect_auxiliary_data(),
    Result = #{ data => Res, errors => Errs },
    decorate_top_level(Result, aux, Aux).

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

execute_query(#ectx{ defer_request_id = ReqId } = Ctx,
              #op { selection_set = SSet, schema = QType }) ->
    #object_type{} = QType,
    case execute_sset(Ctx#ectx{ path = [],
                                defer_process = self(),
                                defer_target = top_level },
                      SSet, QType, none) of
        {ok, Res, Errs} ->
            complete_top_level(Res, Errs);
        #work { items = WL, monitor = Ms, demonitors = [] , timeout = TimeOut} ->
            DeferState = #defer_state{ req_id = ReqId,
                                       monitored = Ms,
                                       timeout = TimeOut,
                                       work = maps:from_list(WL) },
            defer_loop(DeferState)
    end.

execute_mutation(Ctx, #op { selection_set = SSet,
                            schema = QType }) ->
    #object_type{} = QType,
    case execute_sset(Ctx#ectx{ path = [],
                                defer_process = self(),
                                defer_target = top_level },
                      SSet, QType, none) of
        %% In mutations, there is no way you can get deferred work
        %% So we just ignore the case here. If it ever occurs with a
        %% case clause bug here, it is a broken invariant.
        {ok, Res, Errs} ->
            complete_top_level(Res, Errs)
    end.

execute_sset(#ectx{ defer_target = DeferTarget } = Ctx, SSet, Type, Value) ->
    GroupedFields = collect_fields(Ctx, Type, SSet),
    Self = make_ref(),
    try
        case execute_sset_field(Ctx#ectx{ defer_target = Self }, GroupedFields, Type, Value) of
            {ok, Map, Errs} ->
                {ok, Map, Errs};
            {defer, Map, Errs, Work, Missing} ->
                #work { items = Items } = Work,
                Closure = obj_closure(DeferTarget, Self, Missing, Map, Errs),
                Work#work { items = [{Self, Closure}] ++ Items }
        end
    catch
        throw:{null, Errors, _Ds} ->
            %% @todo: Cancel defers, or at least consider a way of doing so!
            {ok, null, Errors}
    end.

obj_closure(Upstream, Self, Missing, Map, Errors) ->
    fun
        (cancel) ->
            #done {
               upstream = Upstream,
               key = Self,
               result = ok,
               demonitor = undefined,
               cancel = maps:keys(Missing)
              };
        ({change_ref, From, To}) ->
            {Val, Removed} =  maps:take(From, Missing),
            #work {
               demonitors = [],
               monitor = #{},
               items = [{Self, obj_closure(Upstream, Self,
                                                Removed#{ To => Val },
                                                Map,
                                                Errors)}] };
        ({_Name, {error, Errs}}) ->
            #done {
               upstream = Upstream,
               key = Self,
               result = {ok, null, Errs ++ Errors},
               demonitor = undefined,
               cancel = maps:keys(Missing)
              };
        ({Ref, {ok, Completed, Errs}}) ->
            case maps:take(Ref, Missing) of
                {Name, NewMissing} ->
                    NewMap = Map#{ Name => Completed },
                    NewErrors = Errs ++ Errors,
                    case maps:size(NewMissing) of
                        0 ->
                            #done {
                               upstream = Upstream,
                               key = Self,
                               cancel = [],
                               demonitor = undefined,
                               result = {ok, NewMap, NewErrors}
                              };
                        _ ->
                            #work { items = [{Self,
                                              obj_closure(Upstream, Self,
                                                          NewMissing,
                                                          NewMap,
                                                          NewErrors)}],
                                    monitor = #{},
                                    demonitors = []}
                    end
            end
    end.

merge_work(#work { items = I1, monitor = M1, demonitors = D1, timeout = T1},
           #work { items = I2, monitor = M2, demonitors = D2, timeout = T2}) ->
    MaxTimeOut = max(T1, T2),
    #work { items = I2 ++ I1,
            monitor = maps:merge(M1, M2),
            timeout = MaxTimeOut,
            demonitors = D2 ++ D1 }.

execute_sset_field(Ctx, Fields, Type, Value) ->
    Map = [],
    Errs = [],
    Work = #work { items = [], monitor = #{}, demonitors = [] },
    Missing = #{},
    execute_sset_field(Ctx, Fields, Type, Value, Map, Errs, Work, Missing).

execute_sset_field(_Ctx, [], _Type, _Value, Map, Errs, Work, Missing) ->
    Result = maps:from_list(lists:reverse(Map)),
    case Work#work.items of
        [] ->
            0 = maps:size(Missing),
            {ok, Result, Errs};
        [_|_] ->
            true = maps:size(Missing) > 0,
            {defer, Result, Errs, Work, Missing}
    end;
execute_sset_field(Ctx, [{Key, [F|_] = Fields} | Next],
             Type, Value, Map, Errs, Work, Missing) ->
    case lookup_field(F, Type) of
        typename ->
            execute_sset_field(Ctx, Next, Type, Value,
                               [{Key, typename(Type)} | Map],
                               Errs, Work, Missing);
        FieldType ->
            CtxP = add_path(Ctx, Key),
            case execute_field(CtxP, Type, Value, Fields, FieldType) of
                {ok, Result, FieldErrs} ->
                    execute_sset_field(Ctx, Next, Type, Value,
                                       [{Key, Result} | Map],
                                       FieldErrs ++ Errs,
                                       Work, Missing);
                #work { items = WUs } = Work2 ->
                    Ref = upstream_ref(WUs),
                    execute_sset_field(Ctx, Next, Type, Value, Map, Errs,
                                       merge_work(Work, Work2),
                                       Missing#{ Ref => Key });
                {error, Errors} ->
                    throw({null, Errors, Work})
            end
    end.

upstream_ref([{Ref, _} | _]) -> Ref.

typename(#object_type { id = ID }) -> ID.

lookup_field(#field { id = ID }, Obj) ->
    lookup_field_name(name(ID), Obj).

lookup_field_name(<<"__typename">>, _)             -> typename;
lookup_field_name(N, #object_type { fields = FS }) -> maps:get(N, FS).

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

collect_fields(Ctx, Type, SSet) ->
    collect_fields(Ctx, Type, SSet, #{}).

collect_fields(Ctx, Type, SSet, Visited) ->
    collect_fields(Ctx, Type, SSet, Visited, orddict:new()).

collect_fields(_Ctx, _Type, [], _Visited, Grouped) ->
    Grouped;
collect_fields(Ctx, Type, [#field{ directives = Dirs } = S |SS], Visited, Grouped) ->
    case view_include_skip_directives(Ctx, Dirs) of
        include ->
            collect_fields(Ctx, Type, SS, Visited,
                           orddict:append(alias(S), S, Grouped));
        skip ->
            collect_fields(Ctx, Type, SS, Visited, Grouped)
    end;
collect_fields(Ctx, Type, [#frag_spread { id = ID, directives = Dirs }|SS], Visited, Grouped) ->
    case view_include_skip_directives(Ctx, Dirs) of
        include ->
            #ectx{ frags = Frags } = Ctx,
            Name = name(ID),
            %% TODO: Lift this to a function by itself called collect_view_fragment...
            case maps:is_key(Name, Visited) of
                true ->
                    collect_fields(Ctx, Type, SS, Visited, Grouped);
                false ->
                    case maps:get(Name, Frags, not_found) of
                        not_found ->
                            collect_fields(Ctx, Type, SS, Visited#{ Name => true }, Grouped);
                        #frag{} = F ->
                            collect_fields(Ctx, Type, [F | SS], Visited#{ Name => true }, Grouped)
                    end
            end;
        skip ->
            collect_fields(Ctx, Type, SS, Visited, Grouped)
    end;
collect_fields(Ctx, Type, [S|SS], Visited, Grouped) ->
    case S of
        #frag{ selection_set = FragmentSSet, directives = Dirs } = Fragment ->
            case view_include_skip_directives(Ctx, Dirs) of
                include ->
                    case does_fragment_type_apply(Type, Fragment) of
                        false ->
                            collect_fields(Ctx, Type, SS, Visited, Grouped);
                        true ->
                            FragGrouped =
                                collect_fields(Ctx, Type, FragmentSSet, Visited),
                            Grouped2 = collect_groups(FragGrouped, Grouped),
                            collect_fields(Ctx, Type, SS, Visited, Grouped2)
                    end;
                skip ->
                    collect_fields(Ctx, Type, SS, Visited, Grouped)
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

execute_field_await(#ectx{ defer_request_id = ReqId, 
                           ctx = #{ default_timeout := TimeOut}} = Ctx,
                    ElaboratedTy,
                    Fields,
                    Ref) ->
    receive
        {'$graphql_reply', ReqId, Ref, ResolvedValue} ->
            complete_value(Ctx, ElaboratedTy, Fields, ResolvedValue);
        {'$graphql_reply', _, _, _} ->
            execute_field_await(Ctx, ElaboratedTy, Fields, Ref)
    after TimeOut ->
            exit(defer_mutation_timeout)
    end.

execute_field(#ectx{ op_type = OpType,
                     ctx = #{ default_timeout := DT }} = Ctx,
              ObjType, Value, [F|_] = Fields,
              #schema_field { directives = Directives, resolve = RF}) ->
    Name = name(F),
    #schema_field { ty = ElaboratedTy } = field_type(F),
    Args = resolve_args(Ctx, F),
    Fun = resolver_function(ObjType, RF),
    case resolve_field_value(Ctx, ObjType, Value, Name, Directives, Fun, Args) of
        {defer, Token, _} when OpType == mutation ->
            %% A mutation must not run the mutation in the parallel, so it awaits
            %% the data straight away
            Ref = graphql:token_ref(Token),
            execute_field_await(Ctx, ElaboratedTy, Fields, Ref);
        {defer, Token, undefined} ->
            Monitor = undefined,
            field_closure(Ctx, ElaboratedTy, Fields, Token, Monitor, DT, queue:new());
        {defer, Token, DeferStateMap} when is_map(DeferStateMap) ->
            defer_field_closure(Ctx, ElaboratedTy, Fields, Token, DeferStateMap);
        ResolvedValue ->
            complete_value(Ctx, ElaboratedTy, Fields, ResolvedValue)
    end.

build_monitor(W) when is_pid(W) ->
    M = erlang:monitor(process, W),
    {M, W};
build_monitor(_W)->
    undefined.

remove_monitor(undefined) -> ok;
remove_monitor({M, _W}) -> demonitor(M, [flush]).

defer_field_closure(#ectx{ defer_target = _Upstream,
                           ctx = #{ default_timeout := DT}} = Ctx,
              ElaboratedTy, Fields, Token, DeferStateMap) ->
    TimeOut = maps:get(timeout, DeferStateMap, DT),
    Worker = maps:get(worker, DeferStateMap, undefined),
    ApplyChain = maps:get(apply, DeferStateMap, queue:new()),
    Monitor = build_monitor(Worker),
    field_closure(Ctx, ElaboratedTy, Fields, Token, Monitor, TimeOut, ApplyChain).

field_closure(#ectx{ defer_target = Upstream } = Ctx,
              ElaboratedTy,
              Fields,
              Token,
              Monitor,
              TimeOut,
              ApplyChain) ->
    Ref = graphql:token_ref(Token),
    Closure =
        fun
            (cancel) ->
                cancel(Token, Monitor),
                #done { upstream = Upstream,
                        key = Ref,
                        cancel = [],
                        demonitor = Monitor,
                        result = ok
                      };
            ({crash, MRef, Pid, Reason}) when MRef == element(1, Monitor) ->
                #done { upstream = Upstream,
                        key = Ref,
                        cancel = [],
                        demonitor = undefined,
                        result = {error, [{worker_crash, Pid, Reason}]}
                      };
            (ResolverResult) ->
                remove_monitor(Monitor),
                case apply_chain(ResolverResult, queue:to_list(ApplyChain)) of
                    {go, AppliedResult} ->
                        ResVal = handle_resolver_result(AppliedResult),
                        case complete_value(Ctx, ElaboratedTy, Fields, ResVal) of
                            {ok, Result, Errs} ->
                                #done { upstream = Upstream,
                                        key = Ref,
                                        cancel = [],
                                        demonitor = Monitor,
                                        result = {ok, Result, Errs} };
                            {error, Errs} ->
                                #done { upstream = Upstream,
                                        key = Ref,
                                        cancel = [],
                                        demonitor = Monitor,
                                        result = {error, Errs} };
                            #work { items = Items, demonitors = Ms } = Wrk ->
                                NewRef = upstream_ref(Items),
                                Wrk#work { change_ref = {Upstream, Ref, NewRef},
                                           demonitors = [Monitor] ++ Ms}
                        end;
                    {defer, NewToken, DeferState} ->
                        #work { items = Items } = Wrk = defer_field_closure(Ctx, ElaboratedTy, Fields, NewToken, DeferState),
                        NewRef = upstream_ref(Items),
                        Wrk#work { change_ref = {Upstream, Ref, NewRef}}
                end
        end,
    #work { items = [{Ref, Closure}],
            demonitors = [],
            timeout = TimeOut,
            monitor = case Monitor of
                          undefined -> #{};
                          {M, _} -> #{ M => Ref }
                      end }.

apply_chain(Val, []) ->
    {go, Val};
apply_chain(Val, [F|Fs]) ->
    case F(Val) of
        {ok, _Val} = Ok -> apply_chain(Ok, Fs);
        {error, _Reason} = Error -> apply_chain(Error, Fs);
        {defer, Token} ->
            {defer, Token, #{ apply => queue:from_list(Fs) }};
        {defer, Token, #{ apply := ToApply} = M} ->
            %% Insert the rest of the chain at the front in reverse
            %% order, so the item deepest in the list goes in first.
            NewQueue = lists:foldr(fun queue:in_r/2, Fs, ToApply),
            {defer, Token, M#{ apply := NewQueue }}
    end.

report_wrong_return(Obj, Name, Fun, Val) ->
    error_logger:error_msg(
      "Resolver ~p.~p returned wrong value: ~p(..) -> ~p",
      [Obj,
       Name,
       Fun,
       Val]).

format_directives([]) -> [];
format_directives([#directive { id = N, args = Args }|Ds]) ->
    [#directive{ id = name(N),
                 args = maps:from_list(
                          [{name(ID), Value} || {ID, Value} <- Args])}
     | format_directives(Ds)].

resolve_field_value(#ectx { op_type = OpType,
                            ctx = CallerContext,
                            defer_process = Proc,
                            defer_request_id = ReqId },
                    #object_type { id = OID,
                                   directives = ODirectives} = ObjectType,
                    Value, Name, FDirectives, Fun, Args) ->
    AnnotatedCallerCtx =
        CallerContext#{ op_type => OpType,
                        field => Name,
                        field_directives => format_directives(FDirectives),
                        object_type => OID,
                        object_directives => format_directives(ODirectives),
                        defer_process => Proc,
                        defer_request_id => ReqId
    },
    try Fun(AnnotatedCallerCtx, Value, Name, Args) of
        V -> 
            case handle_resolver_result(V) of
                wrong ->
                    Obj = graphql_schema:id(ObjectType),
                    report_wrong_return(Obj, Name, Fun, V),
                    {error, {wrong_resolver_return, {Obj, Name}}};
                Res -> Res
            end
    catch
        throw:{'$graphql_throw', Msg} ->
            case handle_resolver_result(Msg) of
                wrong ->
                    Obj = graphql_schema:id(ObjectType),
                    report_wrong_return(Obj, Name, Fun, Msg),
                    {error, {wrong_resolver_return, {Obj, Name}}};
                Res -> Res
            end;
        ?EXCEPTION(Cl, Err, Stacktrace) ->
            M = #{ type => graphql_schema:id(ObjectType),
                   field => Name,
                   stack => ?GET_STACK(Stacktrace),
                   class => Cl,
                   error => Err},
            {error, {resolver_crash, M}}
    end.


handle_resolver_result({error, Reason}) ->
    {error, {resolver_error, Reason}};
handle_resolver_result({ok, Result}) ->
    {ok, Result};
handle_resolver_result({ok, Result, AuxiliaryDataList}) when is_list(AuxiliaryDataList) ->
    self() ! {'$auxiliary_data', AuxiliaryDataList},
    {ok, Result};
handle_resolver_result({defer, Token}) ->
    {defer, Token, undefined};
handle_resolver_result({defer, Token, DeferStateMap}) ->
    {defer, Token, DeferStateMap};
handle_resolver_result(_Unknown) -> wrong.

complete_value(Ctx, Ty, Fields, {ok, Value}) when is_binary(Ty) ->
    error_logger:warning_msg(
      "Canary: Type lookup during value completion for: ~p~n",
      [Ty]),
    SchemaType = graphql_schema:get(Ty),
    complete_value(Ctx, SchemaType, Fields, {ok, Value});
complete_value(#ectx{ defer_target = Upstream } = Ctx,
               {non_null, InnerTy}, Fields, Result) ->
    %% Note we handle arbitrary results in this case. This makes sure errors
    %% factor through the non-null handler here and that handles
    %% nested {error, Reason} tuples correctly
    Self = make_ref(),
    case complete_value(Ctx#ectx{ defer_target = Self }, InnerTy, Fields, Result) of
        {error, Reason} ->
            %% Rule: Along a path, there is at most one error, so if the underlying
            %% object is at fault, don't care too much about this level, just pass
            %% on the error
            err(Ctx, Reason);
        {ok, null, InnerErrs} ->
            err(Ctx, null_value, InnerErrs);
        {ok, _C, _E} = V ->
            V;
        #work { items = WUs } = Work ->
            %% This closure wraps the null properly and errors null-returns
            %% From the underlying computation if it completes later on with a
            %% null value.
            %%
            %% Note: The closure ignores the key
            Work#work { items =
                            [{Self, not_null_closure(Upstream, Self, Ctx,
                                                     upstream_ref(WUs))}|WUs]}
    end;
complete_value(_Ctx, _Ty, _Fields, {ok, null}) ->
    {ok, null, []};
complete_value(Ctx, {list, _}, _Fields, {ok, V}) when not is_list(V) ->
    null(Ctx, not_a_list);
complete_value(Ctx, {list, InnerTy}, Fields, {ok, Value}) ->
    complete_value_list(Ctx, InnerTy, Fields, Value);
complete_value(Ctx, #scalar_type { id = ID, resolve_module = RM }, _Fields, {ok, Value}) ->
    complete_value_scalar(Ctx, ID, RM, Value);
complete_value(Ctx, #enum_type { id = ID,
                                        resolve_module = RM},
               _Fields, {ok, Value}) ->
    case complete_value_scalar(Ctx, ID, RM, Value) of
        {ok, null, Errors} ->
            {ok, null, Errors};
        {ok, Result, Errors} ->

            case graphql_schema:validate_enum(ID, Result) of
                ok ->
                    {ok, Result, Errors};
                {other_enums, _EnumTypes} ->
                    null(Ctx, {invalid_enum_output, ID, Result}, Errors);
                not_found ->
                    null(Ctx, {invalid_enum_output, ID, Result}, Errors)
            end
    end;
complete_value(Ctx, #interface_type{ resolve_type = Resolver }, Fields, {ok, Value}) ->
    complete_value_abstract(Ctx, Resolver, Fields, {ok, Value});
complete_value(Ctx, #union_type{ resolve_type = Resolver }, Fields, {ok, Value}) ->
    complete_value_abstract(Ctx, Resolver, Fields, {ok, Value});
complete_value(Ctx, #object_type{} = Ty, Fields, {ok, Value}) ->
    SubSelectionSet = merge_selection_sets(Fields),
    execute_sset(Ctx, SubSelectionSet, Ty, Value);
complete_value(Ctx, _Ty, _Fields, {error, Reason}) ->
    null(Ctx, Reason).

%% Complete an abstract value
complete_value_abstract(Ctx, Resolver, Fields, {ok, Value}) ->
    case resolve_abstract_type(Resolver, Value) of
        {ok, ResolvedType} ->
            complete_value(Ctx, ResolvedType, Fields, {ok, Value});
        {error, Reason} ->
            null(Ctx, Reason)
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
       ?EXCEPTION(Cl, Err, Stacktrace) ->
            error_logger:error_msg(
              "Type resolver crashed: ~p stacktrace: ~p~n",
              [{Cl,Err}, ?GET_STACK(Stacktrace)]),
           {error, {resolve_type_crash, {Cl,Err}}}
    end.

complete_value_scalar(Ctx, ID, RM, Value) ->
    try RM:output(ID, Value) of
        {ok, Result} ->
            {ok, Result, []};
        {error, Reason} ->
            null(Ctx, {output_coerce, ID, Value, Reason})
    catch
        ?EXCEPTION(Cl, Err, Stacktrace) ->
            error_logger:error_msg(
              "Output coercer crash during value completion: ~p, stacktrace: ~p~n",
              [{Cl,Err,ID,Value}, ?GET_STACK(Stacktrace)]),
            null(Ctx, {output_coerce_abort, ID, Value, {Cl, Err}})
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

complete_value_list(#ectx{ defer_target = Upstream } = Ctx,
                    Ty, Fields, Results) ->
    IndexedResults = index(Results),
    case assert_list_completion_structure(Ty, Fields, IndexedResults) of
        {error, list_resolution} ->
            null(Ctx, list_resolution);
        ok ->
            Self = make_ref(),
            InnerCtx = Ctx#ectx{ defer_target = Self },
            InitWork = #work {items = [], monitor = #{}, demonitors = [] },
            Completer =
                fun
                    F([]) -> {[], InitWork, #{}};
                    F([{Index, Result}|Next]) ->
                        {Rest, Work1, Missing} = F(Next),
                        CtxP = add_path(InnerCtx, Index),
                        case complete_value(CtxP, Ty, Fields, Result) of
                            {ok, V, Errs} ->
                                Wrapper =
                                    fun
                                        (#{ error_term := Term } = E) ->
                                            E#{ error_term := {resolver_error, Term}};
                                        (Otherwise) ->
                                            error_logger:error_report([{wrong, Otherwise}]),
                                            Otherwise
                                    end,
                                { [{ok, V, lists:map(Wrapper, Errs)} | Rest],
                                  Work1,
                                  Missing };
                            {error, Err} ->
                                { [{error, Err}| Rest],
                                  Work1,
                                  Missing };
                            #work { items = Work } = Work2 ->
                                Ref = upstream_ref(Work),
                                { [{defer, Index} | Rest],
                                  merge_work(Work1, Work2),
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
                    merge_work(
                      Ws,
                      #work { demonitors = [],
                              monitor = #{},
                              items = [{Self, Closure}] })
            end
    end.

list_subst([], _Done)               -> [];
list_subst([{defer, Idx}|Xs], Done) -> [maps:get(Idx, Done)|list_subst(Xs, Done)];
list_subst([X|Xs], Done)            -> [X|list_subst(Xs, Done)].

list_closure(Upstream, Self, Missing, List, Done) ->
    fun
        (cancel) ->
            #done {
               upstream = Upstream,
               key = Self,
               result = ok,
               demonitor = undefined,
               cancel = maps:keys(Missing)
              };
        ({change_ref, From, To}) ->
            {Val, Removed} = maps:take(From, Missing),
            #work {
               items = [{Self,
                         list_closure(Upstream, Self,
                                      Removed#{ To => Val },
                                      List,
                                      Done)}],
               monitor = #{},
               demonitors = [] };
        ({Ref, Result}) ->
            case maps:take(Ref, Missing) of
                {Index, NewMissing} ->
                    NewDone = Done#{ Index => Result },
                    case maps:size(NewMissing) of
                        0 ->
                            SubstList = list_subst(List, NewDone),
                            case complete_list_value_result(SubstList) of
                                {Res, []} ->
                                    {Vs, Es} = lists:unzip(Res),
                                    Len = length(SubstList),
                                    Len = length(Vs),
                                    #done {
                                       upstream = Upstream,
                                       key = Self,
                                       cancel = [],
                                       demonitor = undefined,
                                       result = {ok, Vs, lists:concat(Es) }
                                      };
                                {_, Reasons} ->
                                    #done {
                                       upstream = Upstream,
                                       key = Self,
                                       cancel = [],
                                       demonitor = undefined,
                                       result = {ok, null, Reasons}
                                      }
                            end;
                        _ ->
                            #work{ items = [{Self,
                                             list_closure(Upstream, Self,
                                                          NewMissing,
                                                          List,
                                                          NewDone )}],
                                   monitor = #{},
                                   demonitors = [] }
                    end
            end
    end.

not_null_closure(Upstream, Self, Ctx, Ref) ->
    fun
        (cancel) ->
            #done {
               upstream = Upstream,
               key = Self,
               cancel = [Ref],
               demonitor = undefined,
               result = ok
              };
        ({change_ref, _Old, New}) ->
            #work { demonitors = [],
                    monitor = #{},
                    items = [{Self, not_null_closure(Upstream, Self, Ctx,
                                                     New)}] };
        ({_Ref, {ok, null, InnerErrs}}) ->
            #done {
               upstream = Upstream,
               key = Self,
               cancel = [],
               demonitor = undefined,
               result = err(Ctx, null_value, InnerErrs)
              };
        ({_Ref, Res}) ->
            {ok, _, _} = Res, % Assert the current state of affairs
            #done {
               upstream = Upstream,
               key = Self,
               cancel = [],
               demonitor = undefined,
               result = Res
              }
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

get_operation(#ectx { ctx = CallerCtx }, Op) ->
    get_operation(CallerCtx, Op);
get_operation(#{ operation_name := undefined }, [Op]) ->
    {ok, Op};
%% A variant is that certain clients send the empty string as their
%% operation. In this case, the system will fail to find the
%% operationName. Solve this by allowing an empty operation as were it
%% an undefined one, and fail if there is more than a single operation:
get_operation(#{ operation_name := <<>> }, [Op]) ->
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

resolver_function(_ObjType, R) when is_function(R, 4) -> R;
resolver_function(#object_type {
                     id = Id,
                     resolve_module = undefined }, undefined) ->
    exit({no_resolver, Id});
resolver_function(#object_type { resolve_module = M }, undefined) ->
    fun M:execute/4.

%% -- OUTPUT COERCION ------------------------------------

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

%% Coerce variables into its slot based on type. This is required
%% because the specification allows you to use e.g, a variable of type
%% Pet in a slot of type [Pet]. This requires a variable expansion
%% here, which is why we need to coerce in the execution phase. It
%% somewhat fails in the sense that we are now violating the obvious
%% phase splitting in the system
%%
%% For a discussion about the Pet -> [Pet] coercion in the
%% specification, see (Oct2016 Section 3.1.7)
var_coerce(Tau, Sigma, V) when is_binary(Sigma)       ->
    X = graphql_schema:lookup(Sigma),
    var_coerce(Tau, X, V);
var_coerce(Tau, Sigma, V) when is_binary(Tau)         ->
    X = graphql_schema:lookup(Tau),
    var_coerce(X, Sigma, V);
var_coerce(Refl, Refl, Value)                         -> Value;
var_coerce({non_null, Tau}, {non_null, Sigma}, Value) ->
    var_coerce(Tau, Sigma, Value);
var_coerce({non_null, Tau}, Tau, Value)               -> Value;
var_coerce({list, Tau}, {list, Sigma}, Values) ->
    var_coerce(Tau, Sigma, Values);
var_coerce(Tau, {list, SType}, Value)                 -> [var_coerce(Tau, SType, Value)].

%% Produce a valid value for an argument.
value(Ctx, {Ty, Val})                     -> value(Ctx, Ty, Val);
value(Ctx, #{ type := Ty, value := Val }) -> value(Ctx, Ty, Val).

value(#ectx{ params = Params }, SType, #var { id = ID, ty = DType,
                                              default = Default }) ->
    %% Parameter expansion and type check is already completed
    %% at this stage
    case maps:get(name(ID), Params, not_found) of
        not_found ->
            case Default of
                %% Coerce undefined values to "null"
                undefined -> var_coerce(DType, SType, null);
                _ -> var_coerce(DType, SType, Default)
            end;
        Value ->
            var_coerce(DType, SType, Value)
    end;
value(_Ctx, _Ty, undefined) ->
    null;
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
        Bin when is_binary(Bin) ->
            LoadedTy = graphql_schema:get(Bin),
            value(Ctx, LoadedTy, Val)
    end.

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
    CParams = canon_params(Params),
    #ectx { params = CParams,
            ctx = Ctx#{ params := CParams} }.

canon_params(Ps) ->
     KVs = maps:to_list(Ps),
     maps:from_list([{binarize(K), V} || {K, V} <- KVs]).

binarize(A) when is_atom(A) -> atom_to_binary(A, utf8);
binarize(B) when is_binary(B) -> B;
binarize(L) when is_list(L) -> list_to_binary(L).

%% -- DEFERRED PROCESSING --

%% Process deferred computations by grabbing them in the mailbox
defer_loop(#defer_state { req_id = Id , timeout = TimeOut} = State) ->
    receive
        {'$graphql_reply', Id, Ref, Data} ->
            defer_handle_work(State, Ref, Data);
        {'$graphql_reply', _, _, _} ->
            defer_loop(State);
        {'$graphql_sync', Id, Pid, Msg} ->
            Pid ! Msg,
            defer_loop(State);
        {'$graphql_sync', _, _, _} ->
            defer_loop(State);
        {'DOWN', MRef, process, Pid, Reason} ->
            defer_monitor_down(State, MRef, Pid, Reason)
    after TimeOut ->
            exit(defer_timeout)
    end.

%% Cancel a token
cancel(Token, {M, Pid}) ->
    Pid ! {graphql_cancel, Token},
    erlang:demonitor(M, [flush]),
    ok.

defer_monitor_down(#defer_state{ monitored = Monitored} = State,
                   MRef, Pid, Reason) ->
    {Target, Monitored2} = maps:take(MRef, Monitored),
    NextState = State#defer_state { monitored = Monitored2 },
    defer_handle_work(NextState, Target, {crash, MRef, Pid, Reason}).

%% Process work
defer_handle_work(#defer_state { work = WorkMap,
                                 monitored = Monitored,
                                 canceled = Cancelled } = State,
                  Target,
                  Input) ->
    case maps:take(Target, WorkMap) of
        error ->
            case lists:member(Target, Cancelled) of
                true ->
                    defer_loop(
                      State#defer_state { canceled = Cancelled -- [Target] });
                false ->
                    error_logger:info_msg(
                      "Ignoring unknown uncancelled target ~p with return ~p",
                      [Target, Input]),
                    defer_loop(State)
            end;
        {Closure, WorkMap2} ->
            Result = Closure(Input),
            case Result of
                %% TODO: We can assert the monitor state here, and we probably
                %% should
                #done { upstream = top_level,
                        result = {ok, Res, Errs}} ->
                    complete_top_level(Res, Errs);
                #done { upstream = top_level,
                        result = {error, Errs} } ->
                    complete_top_level(undefined, Errs);
                #done { upstream = Upstream,
                        key = Key,
                        cancel = CancelRefs,
                        demonitor = Demonitor,
                        result = Value } ->
                    NextState = State#defer_state {
                                  work = WorkMap2,
                                  monitored = case Demonitor of
                                                  undefined -> Monitored;
                                                  {M, _W} -> maps:remove(M, Monitored)
                                              end
                                 },
                    CancelState = defer_handle_cancel(NextState, CancelRefs),
                    defer_handle_work(
                      CancelState,
                      Upstream,
                      {Key, Value});
                #work { items = New,
                        change_ref = Change,
                        monitor = ToMonitor,
                        demonitors = ToDemonitor } ->
                    NewWork = maps:from_list(New),
                    NextState = State#defer_state {
                                  work = maps:merge(WorkMap2, NewWork),
                                  monitored =
                                    maps:without([M || {M, _} <- ToDemonitor],
                                                 maps:merge(Monitored,
                                                            ToMonitor))},
                    case Change of
                        undefined ->
                            defer_loop(NextState);
                        {Upstream, From, To} ->
                            defer_handle_work(NextState, Upstream, {change_ref, From, To})
                    end
            end
    end.

defer_handle_cancel(State, []) -> State;
defer_handle_cancel(#defer_state { work = WorkMap,
                                   canceled = Cancelled } = State, [R|Rs]) ->
    case maps:take(R, WorkMap) of
        error ->
            defer_handle_cancel(
              State#defer_state { canceled = [R|Cancelled] },
              Rs);
        {Closure, WorkMap2} ->
            #done { result = ok, cancel = ToCancel } = Closure(cancel),
            case ToCancel of
                [] ->
                    defer_handle_cancel(
                      State#defer_state { work = WorkMap2,
                                          canceled = [R|Cancelled] },
                      Rs);
                _ ->
                    defer_handle_cancel(
                      State#defer_state { work = WorkMap2,
                                          canceled = [R|Cancelled] },
                      ToCancel ++ Rs)
            end
    end.


%% -- ERROR HANDLING --
null(Ctx, Reason) ->
    null(Ctx, Reason, []).

null(Ctx, Reason, More) ->
    {error, Return} = err(Ctx, Reason, More),
    {ok, null, Return}.

err(Ctx, Reason) ->
    err(Ctx, Reason, []).

err(#ectx { path = Path }, Reason, More) when is_list(More) ->
    {error, [graphql_err:mk(Path, execute, Reason)|More]}.

%% Add a path component to the context
-spec add_path(ectx(), Component :: term()) -> ectx().
add_path(#ectx{ path = P } = Ctx, C) ->
    Ctx#ectx{ path = [graphql_err:path(C)|P] }.

