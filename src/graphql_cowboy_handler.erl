-module(graphql_cowboy_handler).

-export([init/3]).
-export([
    rest_init/2,
    allowed_methods/2,
    resource_exists/2,
    content_types_provided/2,
    content_types_accepted/2,
    charsets_provided/2
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
    {AuthSubject, Req3} = cowboy_req:meta(moose_token, Req2),
    {ReqID, Req4} = cowboy_req:meta(gryphon_req_id, Req3),
    {ok, Req4, State#{ method => Method, auth_subject => AuthSubject, req_id => ReqID }}.
    
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

-spec charsets_provided(any(), any()) -> any().
charsets_provided(Req, State) ->
    {[<<"utf-8">>], Req, State}.

-spec resource_exists(any(), any()) -> any().
resource_exists(Req, #{ method := <<"GET">> } = State) -> {true, Req, State};
resource_exists(Req, #{ method := <<"POST">> } = State) -> {false, Req, State}.

%% -- PROCESSING -----------------------

process_query(Req, State) ->
    {ok, Body, Req2} = cowboy_req:body(Req),
    {Bindings, Req3} = cowboy_req:bindings(Req2),
    Params = maps:from_list(Bindings),
    case decode_json_map(Body) of
        {ok, JSON} ->
            process_query(Req3, State, JSON, Params);

        {error, invalid_json} ->
            err(400, invalid_body_json, Req3, State)
    end.

process_query(Req, State, Body, Params) ->
    Doc = query([Params, Body]),
    case variables([Params, Body]) of
        {ok, Variables} ->
            Operation = operation_name([Params, Body]),
            run(Doc, Operation, Variables, Req, State);

        {error, invalid_json} ->
            err(400, invalid_input_variables, Req, State)
    end.

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
run(Doc, OpName, Vars, Req, #{ auth_subject := AuthSubject, req_id := ReqID } = State) ->
    case graphql:parse(Doc) of
       {ok, AST} ->
          try
             Elaborated = graphql:elaborate(AST),
             {ok, #{
                 fun_env := FunEnv,
                 ast := AST2 }} = graphql:type_check(Elaborated),
             ok = graphql:validate(AST2),
             Coerced = graphql:type_check_params(FunEnv, OpName, Vars),
             Response = graphql:execute(#{
                     params => Coerced,
                     operation_name => OpName,
                     req_id => ReqID,
                     auth_subject => AuthSubject },
                 AST2),
             Req2 = cowboy_req:set_resp_body(encode_json(Response), Req),
             {ok, Reply} = cowboy_req:reply(200, Req2),
             {halt, Reply, State}
           catch
               throw:Err ->err(400, Err, Req, State)
           end;
        {error, Error} ->
           err(400, {parser_error, Error}, Req, State)
    end.

%% -- DECODING ------------------------------
query([#{ <<"query">> := Q } | _]) -> Q;
query([ _ | Next]) -> query(Next);
query([]) -> undefined.

variables([#{ <<"variables">> := Vars} | _]) when is_binary(Vars) ->
    decode_json_map(Vars);
variables([#{ <<"variables">> := VMap} | _]) when is_map(VMap) -> {ok, VMap};
variables([_ | Next]) -> variables(Next);
variables([]) -> {ok, #{}}.

operation_name([#{ <<"operationName">> := OpName } | _]) -> OpName;
operation_name([_ | Next]) -> operation_name(Next);
operation_name([]) -> undefined.

%% -- ERROR HANDLING -------------------------

err(Code, Msg, Req, State) ->
    Body = jsx:encode(#{ errors => format_err(Msg) }),
    Req2 = cowboy_req:set_resp_body(Body, Req),
    {ok, Reply} = cowboy_req:reply(Code, Req2),
    {halt, Reply, State}.

format_err({parser_error, {Line, graphql_scanner, Description}}) ->
    [#{ type => graphql_scanner_error,
        line => Line,
        message => iolist_to_binary(io_lib:format("~s", [Description])) }];

format_err({parser_error, {Line, graphql_parser, _Description} = E}) ->
    [#{ type => graphql_parser_error,
        line => Line,
        message => iolist_to_binary(graphql_parser:format_error(E)) }];

format_err(invalid_input_variables) ->
    [#{ type => invalid_input_variables }];

format_err(invalid_body_json) ->
    [#{ type => invalid_body_json }];

format_err(invalid_json) ->
    [#{ type => json_parser_error }];

format_err(Reason) ->
    lager:warning("Unhandled user-visible error message: ~p", [Reason]),
    ErrString = iolist_to_binary(io_lib:format("~p", [Reason])),
    [#{ type => internal_error,
        message => ErrString }].

decode_json_map(<<>>) ->
    {ok, #{}};

decode_json_map(Data) when is_binary(Data) ->
    try jsx:decode(Data, [return_maps]) of
        JSON when is_map(JSON) ->
            {ok, JSON};

        null ->
            {ok, #{}};

        _ ->
            {error, invalid_json}
    catch error:badarg ->
        {error, invalid_json}
    end.

%% @private
encode_json(Data) ->
    jsx:encode(Data, []).
