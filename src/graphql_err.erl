-module(graphql_err).

-include_lib("graphql/include/graphql.hrl").
-include("graphql_internal.hrl").
-include("graphql_schema.hrl").

-export([format_errors/2]).
-export([mk/3, mk/4]).
-export([abort/2, abort/3]).
-export([path/1]).
-export([format_ty/1]).

-export([crash/2, err/2]).

-spec abort([any()], any()) -> no_return().
abort(Path, Msg) ->
    abort(Path, uncategorized, Msg).

-spec abort([any()], any(), any()) -> no_return().
abort(Path, Phase, Msg) ->
   Err = mk(Path, Phase, Msg),
   throw({error, Err}).

mk(Path, Phase, Term) ->
    #{ path => path(lists:reverse(Path)),
       phase => Phase,
       error_term => Term
     }.

mk(Path, Phase, Term, Stack) ->
    X = mk(Path, Phase, Term),
    X#{ stack_trace => Stack }.

%% ERROR FORMATTING
%% ----------------------------------------------------------------------------
crash(_Ctx, Err) ->
    %% We dump the error data internally, but we don't dump crashes
    %% to the client.
    %%
    %% We recommend providing your own override of this function to include
    %% some unique request id for the request as well.
    error_logger:error_report([{crash, Err}]),
    #{ message => <<"GraphQL Internal Server Error">>,
       extensions => #{ code => internal_server_error } }.

err(_Ctx, Err) ->
    %% Default resolver for errors. This is used to print out an error
    %% To user of the API. It can be used to manipulate the error and
    %% give a useful message to an end user
    Msg = io_lib:format("~p", [Err]),
    #{ message => Msg,
       extensions => #{ code => resolver_error } }.

format_errors(Ctx, Errs) ->
    case maps:get(error_module, Ctx, none) of
        none ->
            format_errors_(Ctx#{ error_module => ?MODULE }, Errs);
        _ ->
            format_errors_(Ctx, Errs)
    end.

format_errors_(_Ctx, []) -> [];
format_errors_(#{ error_module := Mod } = Ctx, [#{ path := Path, phase := Phase, error_term := Term } | Es]) ->
    Res = case Term of
              {resolver_crash, T} ->
                  CrashResponse = protect(Mod, crash, [Ctx, T]),
                  check_error_response(Path, CrashResponse);
              {resolver_error, T} ->
                  ErrResponse = protect(Mod, err, [Ctx, T]),
                  check_error_response(Path, ErrResponse);
              Other ->
                  OtherResponse = #{
                                    extensions => #{ code => err_code(Phase, Other) },
                                    message => err_msg({Phase, Other}) },
                  check_error_response(Path, OtherResponse)
          end,
    [Res|format_errors_(Ctx, Es)].

protect(M, F, A) ->
    try apply(M, F, A) of
        Val -> Val
    catch
        ?EXCEPTION(Cl, Err, Stacktrace) ->
            error_logger:error_report([error_module_crash,
                                      #{ class => Cl,
                                         error => Err,
                                         stack => ?GET_STACK(Stacktrace)}]),
            #{ message => <<"Error Module Crashed">> }
    end.

check_error_response(Path, #{ message := Message, extensions := Extensions })
  when is_map(Extensions) ->
    #{ path => Path,
       message => iolist_to_binary(Message),
       extensions => Extensions };
check_error_response(Path, #{ message := Message }) ->
    #{ path => Path,
       message => iolist_to_binary(Message) };
check_error_response(Path, Otherwise) ->
    error_logger:error_report([error_response_incorrect, Otherwise]),
    #{ path => Path,
       message => <<"Internal Error: Error Module supplied wrong error response">> }.


%% -- Error handling dispatch to the module responsible for the error
err_msg({execute, Reason})       -> execute_err_msg(Reason);
err_msg({type_check, Reason})    -> type_check_err_msg(Reason);
err_msg({validate, Reason})      -> validate_err_msg(Reason);
err_msg({uncategorized, Reason}) ->
    io_lib:format("General uncategorized error: ~p", [Reason]).

err_code(execute, {type_resolver_error, _}) -> type_resolver_error;
err_code(execute, {resolver_crash, _}) -> resolver_crash;
err_code(execute, {resolver_error, _}) -> resolver_error;
err_code(execute, Key)       -> simplify(Key);
err_code(type_check, Key)    -> simplify(Key);
err_code(validate, Key)      -> simplify(Key);
err_code(uncategorized, Key) -> simplify(Key).

simplify(A) when is_atom(A) -> A;
simplify(B) when is_binary(B) -> B;
simplify(T) when is_tuple(T) -> element(1, T).

-spec path(Input | [Input]) -> [binary()]
  when
    Input :: document | frag() | op() | field() | binary() | integer() | [Input].
path(Path) ->
   F = fun
           F('ROOT') -> <<"ROOT">>;
           F('...') -> <<"...">>;
           F(undefined) -> <<"..">>;
           F(#frag { id = ID }) -> name(ID);
           F(#op { id = ID }) -> name(ID);
           F(#field { id = ID }) -> name(ID);
           F(#directive { id = ID }) -> name(ID);
           F(#enum_type { id = ID }) -> name(ID);
           F(#interface_type { id = ID }) -> name(ID);
           F(#union_type { id = ID }) -> name(ID);
           F(#object_type { id = ID }) -> name(ID);
           F({name, _, _} = Name) -> name(Name);
           F({var, Name}) -> name(Name);
           F(I) when is_integer(I) -> I;
           F(B) when is_binary(B) -> B;
           F(L) when is_list(L) ->
               [F(X) || X <- L]
       end,
    if
        is_list(Path) ->
            lists:flatten([F(Elem) || Elem <- Path]);
        true ->
            F(Path)
    end.

format_ty(#enum_type { id = Ty }) -> Ty;
format_ty(#input_object_type { id = Ty }) -> Ty;
format_ty({scalar, Ty, Value}) ->
    iolist_to_binary([atom_to_binary(Ty, utf8), ":", Value]);
format_ty(#scalar_type { id = Ty }) -> Ty;
format_ty({input_object, Ty}) -> Ty;
format_ty({object, Fields}) ->
    FEs = format_fields(Fields),
    <<"{", FEs/binary, "}">>;
format_ty({enum, Ty}) ->
    Inner = format_ty(Ty),
    <<"{enum, ", Inner/binary, "}">>;
format_ty([Ty]) ->
    Inner = format_ty(Ty),
    <<"[", Inner/binary, "]">>;
format_ty({list, Ty}) ->
    Inner = format_ty(Ty),
    <<"{list, ", Inner/binary, "}">>;
format_ty({non_null, Ty}) ->
    Inner = format_ty(Ty),
    <<Inner/binary, "!">>;
format_ty(Ty) when is_binary(Ty) ->
    Ty;
format_ty({name, _, N}) -> N;
format_ty(Ty) when is_atom(Ty) ->
    atom_to_binary(Ty, utf8);
format_ty(I) when is_integer(I) ->
    integer_to_binary(I).


format_fields(Xs) ->
    F = fun({{name, Name, _}, {Ty, Val, _}}) ->
        TyF = format_ty(Ty),
        ValF = format_val(Val),
        <<Name/binary, "(", TyF/binary, "): ", ValF/binary>>
    end,
    iolist_to_binary(lists:join(<<", ">>, [F(X) || X <- Xs])).

format_val(B) when is_binary(B) -> B;
format_val(I) when is_integer(I) -> integer_to_binary(I);
format_val(F) when is_float(F) -> float_to_binary(F);
format_val(true) -> <<"true">>;
format_val(false) -> <<"false">>;
format_val(null) -> <<"null">>.

%% -- AST MANIPULATION -------------------------
name('ROOT') -> <<"ROOT">>;
name('...') -> <<"...">>;
name({name, _, N}) -> N;
name(X) when is_binary(X) -> X.

%% -- INTERNALS ----------------------------
execute_err_msg(null_value) ->
    ["The schema specifies the field is non-null, "
     "but a null value was returned by the backend"];
execute_err_msg(not_a_list) ->
    ["The schema specifies the field is a list, "
     "but a non-list value was returned by the backend"];
execute_err_msg({invalid_enum_output, ID, Result}) ->
    io_lib:format("The result ~p is not a valid enum value for type ~ts",
                  [Result, ID]);
execute_err_msg({output_coerce, ID, _Value, Reason}) ->
    io_lib:format("Output coercion failed for type ~s with reason ~p",
                  [ID, Reason]);
execute_err_msg({operation_not_found, OpName}) ->
    ["The operation ", OpName, " was not found in the query document"];
execute_err_msg({type_resolver_error, Err}) ->
    io_lib:format("Couldn't type-resolve: ~p", [Err]);
execute_err_msg({resolver_error, Err}) ->
    io_lib:format("Couldn't resolve: ~p", [Err]);
execute_err_msg({output_coerce_abort, _ID, _Value, _}) ->
    ["Internal Server error: output coercer function crashed"];
execute_err_msg(list_resolution) ->
    ["Internal Server error: A list is being incorrectly resolved"];
execute_err_msg(Otherwise) ->
    io_lib:format("Error in execution: ~p", [Otherwise]).

type_check_err_msg(unnamed_operation_params) ->
    ["Cannot supply parameter lists to unnamed (anonymous) queries"];
type_check_err_msg({operation_not_found, Op}) ->
    io_lib:format("Expected an operation ~p but no such operation was found", [Op]);
type_check_err_msg(missing_non_null_param) ->
    ["The parameter is non-null, but was undefined in parameter list"];
type_check_err_msg(non_null) ->
    ["The value is null in a non-null context"];
type_check_err_msg({null_input, N}) ->
    ["The arg ", N, " is given a null, which is not allowed in the input path"];
type_check_err_msg({not_found, Ty}) ->
    io_lib:format("The type ~p was not found in the schema", [Ty]);
type_check_err_msg({enum_not_found, Ty, Val}) ->
    X = io_lib:format("The value ~p is not a valid enum value for type ", [Val]),
    [X, graphql_err:format_ty(Ty)];
type_check_err_msg({param_mismatch, {enum, Ty, OtherTys}}) ->
    ["The enum value matches types ", graphql_err:format_ty(lists:sort(OtherTys)),
     " but was used in a context where an enum value"
     " of type ", graphql_err:format_ty(Ty), " was expected"];
type_check_err_msg({param_mismatch, Ty, V}) ->
    io_lib:format("The parameter value ~p is not of type ~p", [V, graphql_err:format_ty(Ty)]);
type_check_err_msg({not_input_type, Ty, _}) ->
    ["The type ", graphql_err:format_ty(Ty), " is a valid input type"];
type_check_err_msg({excess_fields_in_object, Fields}) ->
    io_lib:format("The object contains unknown fields and values: ~p", [Fields]);
type_check_err_msg({excess_args, Args}) ->
    io_lib:format("The argument list contains unknown arguments ~p", [Args]);
type_check_err_msg({type_mismatch, #{ document := Doc, schema := Sch }}) ->
    ["Type mismatch. The query document has a value/variable of type (",
     graphql_err:format_ty(Doc), ") but the schema expectes type (", graphql_err:format_ty(Sch), ")"];
type_check_err_msg({type_mismatch, #{ id := ID, document := Doc, schema := Sch }}) ->
    ["Type mismatch on (", ID, "). The query document has a value/variable of type (",
      graphql_err:format_ty(Doc), ") but the schema expects type (", graphql_err:format_ty(Sch), ")"];
type_check_err_msg({type_mismatch, #{ schema := Sch }}) ->
    ["Type mismatch, expected (", graphql_err:format_ty(Sch), ")"];
type_check_err_msg({input_coercion, Type, Value, Reason}) ->
    io_lib:format("Input coercion failed for type ~s with value ~p. The reason it failed is: ~p", [Type, Value, Reason]);
type_check_err_msg({input_coerce_abort, _}) ->
    ["Input coercer failed due to an internal server error"];
type_check_err_msg(unknown_fragment) ->
    ["The referenced fragment name is not present in the query document"];
type_check_err_msg({param_not_unique, Var}) ->
    ["The variable ", Var, " occurs more than once in the operation header"];
type_check_err_msg({input_object_not_unique, Var}) ->
    ["The input object has a key ", Var, " which occur more than once"];
type_check_err_msg({not_unique, X}) ->
    ["The name ", X, " occurs more than once"];
type_check_err_msg({unbound_variable, Var}) ->
    ["The document refers to a variable ", Var,
     " but no such var exists. Perhaps the variable is a typo?"];
type_check_err_msg(enum_string_literal) ->
    ["Enums must not be given as string literals in query documents"];
type_check_err_msg({unknown_enum, E}) ->
    ["The enum name ", E, " is not present in the schema"];
type_check_err_msg({invalid_scalar_value, V}) ->
    io_lib:format("The value ~p is not a valid scalar value", [V]);
type_check_err_msg(non_coercible_default) ->
    ["The default value could not be correctly coerced"];
type_check_err_msg({invalid_value_type_coercion, Ty, Val}) ->
    io_lib:format(
      "The value ~p cannot be coerced into the type ~p",
      [Val, Ty]);
type_check_err_msg({not_union_member, SpreadTy, UnionTy}) ->
    io_lib:format(
      "The spread type ~ts is not a member of the union ~ts",
      [SpreadTy, UnionTy]);
type_check_err_msg({not_interface_embedder, SpreadTy, ScopeTy}) ->
    io_lib:format(
      "The spread type ~ts is an interface. "
      "Yet the scope type ~ts does not impelement this interface.",
      [SpreadTy, ScopeTy]);
type_check_err_msg({not_union_embedder, SpreadTy, ScopeTy}) ->
    io_lib:format(
      "The spread type ~ts is an union. "
      "Yet the scope type ~ts is not a member of this union.",
      [SpreadTy, ScopeTy]);
type_check_err_msg({not_interface_member, SpreadTy, InterfaceTy}) ->
    io_lib:format(
      "The spread type ~ts is not implementing the interface ~ts",
      [SpreadTy, InterfaceTy]);
type_check_err_msg({no_common_object, SpreadTy, ScopeTy}) ->
    io_lib:format(
      "The spread type ~ts and the scope type ~ts has no objects in common",
      [SpreadTy, ScopeTy]);
type_check_err_msg({fragment_spread, SpreadTy, ScopeTy}) ->
    io_lib:format(
      "The spread type ~ts does not match the scope type ~ts",
      [SpreadTy, ScopeTy]);
type_check_err_msg({type_not_found, Ty}) ->
    ["Type not found in schema: ", graphql_err:format_ty(Ty)];
type_check_err_msg({invalid_directive_location, ID, Context}) ->
    ["The directive ", ID, " is not valid in the context ",
     atom_to_binary(Context, utf8)];
type_check_err_msg({not_input_type, Ty}) ->
    ["Type ", graphql_err:format_ty(Ty), " is not an input type but is used in input-context"];
type_check_err_msg({directives_not_unique, X}) ->
    ["The directive with name ", X, " is not unique in this location"];
type_check_err_msg(no_root_schema) ->
    ["No root schema found. One is required for correct operation"];
type_check_err_msg({unknown_field, F}) ->
    ["The query refers to a field, ", F, ", which is not present in the schema"];
type_check_err_msg(unknown_field) ->
    ["The query refers to a field which is not known"];
type_check_err_msg({unknown_argument, N}) ->
    ["The query refers to an argument, ", N, ", which is not present in the schema"];
type_check_err_msg({unknown_directive, Dir}) ->
    ["The query uses a directive, ", Dir, ", which is unknown to this GraphQL server"];
type_check_err_msg(selection_on_scalar) ->
    ["Cannot apply a selection set to a scalar field"];
type_check_err_msg(selection_on_enum) ->
    ["Cannot apply a selection set to an enum type"];
type_check_err_msg(fieldless_object) ->
    ["The path refers to an Object type, but no fields were specified"];
type_check_err_msg(fieldless_interface) ->
    ["The path refers to an Interface type, but no fields were specified"].

validate_err_msg({not_unique, X}) ->
    ["The name ", X, " is not a unique name"];
validate_err_msg({cycles_in_fragments, Cycles}) ->
    io_lib:format("The following fragments contains cycles: ~p", [Cycles]).
