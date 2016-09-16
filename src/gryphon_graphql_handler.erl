-module(gryphon_graphql_handler).

-export([init/3]).
-export([
    rest_init/2,
    allowed_methods/2,
    resource_exists/2,
    content_types_provided/2,
    content_types_accepted/2
]).
-export([
    from_graphql/2,
    from_json/2,
    to_json/2,
    to_html/2
]).

%% -- API ----------------------------------------

-spec init(any(), any(), any()) -> any().
init(_Transport, _Req, _Options) ->
    {upgrade, protocol, cowboy_rest}.
    
%% -- REST CALLBACKS -----------------------------

-spec rest_init(any(), any()) -> any().
rest_init(Req, {priv_file, _App, _Location} = PF) ->
    rest_init(Req, #{ index_location => PF });
rest_init(Req, State) when is_map(State) ->
    {Method, Req2} = cowboy_req:method(Req),
    {Viewer, Req3} = cowboy_req:meta(gryphon_user, Req2),
    {ReqID, Req4} = cowboy_req:meta(gryphon_req_id, Req3),
    {ok, Req4, State#{ method => Method, viewer => Viewer, req_id => ReqID }}.
    
-spec allowed_methods(any(), any()) -> any().
allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>], Req, State}.

-spec content_types_accepted(any(), any()) -> any().
content_types_accepted(Req, State) ->
    {[
    	{{<<"application">>, <<"graphql">>, []}, from_graphql},
    	{{<<"application">>, <<"json">>, []}, from_json}
    ], Req, State}.
    
-spec content_types_provided(any(), any()) -> any().
content_types_provided(Req, State) ->
    {[
        {{<<"application">>, <<"json">>, []}, to_json},
        {{<<"text">>, <<"html">>, []}, to_html}
    ], Req, State}.
    
-spec resource_exists(any(), any()) -> any().
resource_exists(Req, #{ method := <<"GET">> } = State) -> {true, Req, State};
resource_exists(Req, #{ method := <<"POST">> } = State) -> {false, Req, State}.

%% -- PROCESSING -----------------------

process_query(Req, State) ->
    {ok, Body, Req2} = cowboy_req:body(Req),
    {Bindings, Req3} = cowboy_req:bindings(Req2),
    Params = maps:from_list(Bindings),
    case Body of
        <<>> -> process_query(Req3, State, #{}, Params);
        _ ->
            try jsx:decode(Body, [return_maps]) of
                JSON ->
                    process_query(Req2, State, JSON, Params)
            catch
                error:badarg ->
                    err(400, invalid_json, Req3, State)
            end
    end.

process_query(Req, State, Body, Params) ->
    Doc = query([Params, Body]),
    Vars = variables([Params, Body]),
    Operation = operation_name([Params, Body]),
    run(Doc, Operation, Vars, Req, State).

-spec to_json(any(), any()) -> any().
to_json(Req, State) ->
    process_query(Req, State).

-spec to_html(any(), any()) -> any().
to_html(Req, #{ index_location := {priv_file, App, FileLocation}} = State) ->
    Filename = filename:join(code:priv_dir(App), FileLocation),
    {ok, Data} = file:read_file(Filename),
    {Data, Req, State}.

-spec from_json(any(), any()) -> any().
from_json(Req, State) ->
    process_query(Req, State).

-spec from_graphql(any(), any()) -> any().
from_graphql(Req, State) ->
    {ok, Body, Req2} = cowboy_req:body(Req),
    {Bindings, Req3} = cowboy_req:bindings(Req2),
    Params = maps:from_list(Bindings),
    process_query(Req3, State, #{ <<"query">> => Body }, Params).

%% -- Run the GraphQL Query -------------------

run(undefined, _, _, Req, State) ->
    err(400, no_query_supplied, Req, State);
run(Doc, OpName, Vars, Req, #{ viewer := Viewer, req_id := ReqID } = State) ->
    case gql:parse(Doc) of
       {ok, AST} ->
          try
             Elaborated = gql:elaborate(AST),
             {ok, #{
                 fun_env := FunEnv,
                 ast := AST2 }} = gql:type_check(Elaborated),
             ok = gql:validate(AST2),
             Coerced = gql:type_check_params(FunEnv, OpName, Vars),
             Response = gql:execute(#{
                     params => Coerced,
                     operation_name => OpName,
                     req_id => ReqID,
                     viewer => Viewer },
                 AST2),
             Req2 = cowboy_req:set_resp_body(jsx:encode(Response, [space, indent]), Req),
             {ok, Reply} = cowboy_req:reply(200, Req2),
             {halt, Reply, State}
           catch
               throw:Err ->err(400, Err, Req, State)
           end;
       ErrInfo ->
           err(400, {invalid_graphql, ErrInfo}, Req, State)
    end.

%% -- DECODING ------------------------------
query([#{ <<"query">> := Q } | _]) -> Q;
query([ _ | Next]) -> query(Next);
query([]) -> undefined.

variables([#{ <<"variables">> := Vars} | _]) when is_binary(Vars) ->
    case Vars of
        <<"">> -> #{};
        <<"null">> -> #{};
        Vs -> jsx:decode(Vs, [return_maps])
    end;
variables([#{ <<"variables">> := VMap} | _]) when is_map(VMap) -> VMap;
variables([_ | Next]) -> variables(Next);
variables([]) -> #{}.

operation_name([#{ <<"operationName">> := OpName } | _]) -> OpName;
operation_name([_ | Next]) -> operation_name(Next);
operation_name([]) -> undefined.

%% -- ERROR HANDLING -------------------------

err(Code, Msg, Req, State) ->
    Body = jsx:encode(#{ errors => format_err(Msg) }),
    Req2 = cowboy_req:set_resp_body(Body, Req),
    {ok, Reply} = cowboy_req:reply(Code, Req2),
    {halt, Reply, State}.

format_err(Reason) ->
    ErrString = iolist_to_binary(
        io_lib:format("~p", [Reason])),
    [ErrString].
