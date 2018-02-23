-module(graphql_err).

-include("graphql_internal.hrl").
-include("graphql_schema.hrl").

-export([format_errors/2]).
-export([mk/3, mk/4]).
-export([abort/2, abort/3]).
-export([path/1]).
-export([format_ty/1]).

abort(Path, Msg) ->
    abort(Path, uncategorized, Msg).

-spec abort([any()], any()) -> no_return().
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
format_errors([], _Mod) -> [];
format_errors([E|Es], Mod) ->
    Res = case E of
              #{ path := Path, phase := Phase, error_term := Term } ->
                  #{ path => Path,
                     key => err_key(Phase, Term),
                     message => iolist_to_binary(err_msg({Phase, Term})) }
          end,
    [Res|format_errors(Es, Mod)].

%% -- Error handling dispatch to the module responsible for the error
err_msg({elaborate, Reason})     -> elaborate_err_msg(Reason);
err_msg({execute, Reason})       -> execute_err_msg(Reason);
err_msg({type_check, Reason})    -> type_check_err_msg(Reason);
err_msg({validate, Reason})      -> graphql_validate:err_msg(Reason);
err_msg({uncategorized, Reason}) ->
    io_lib:format("General uncategorized error: ~p", [Reason]).

err_key(elaborate, Key)     -> Key;
err_key(execute, {type_resolver_error, _}) -> type_resolver_error;
err_key(execute, {resolver_crash, _}) -> resolver_crash;
err_key(execute, {resolver_error, _}) -> resolver_error;
err_key(execute, Key)       -> Key;
err_key(type_check, Key)    -> Key;
err_key(validate, Key)      -> Key;
err_key(uncategorized, Key) -> Key.

-spec path([Input]) -> [binary()]
  when
    Input :: document | frag() | op() | field() | binary() | [Input].
path(Path) ->
   F = fun
           F('ROOT') -> <<"ROOT">>;
           F('...') -> <<"...">>;
           F(document) -> <<"document">>;
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
           F(I) when is_integer(I) -> I;
           F(B) when is_binary(B) -> B;
           F(L) when is_list(L) ->
               [F(X) || X <- L]
       end,
   lists:flatten([F(Elem) || Elem <- Path]).

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
    atom_to_binary(Ty, utf8).

format_fields(Xs) ->
    F = fun({{name, Name, _}, {Ty, Val, _}}) ->
        TyF = format_ty(Ty),
        ValF = format_val(Val),
        <<Name/binary, "(", TyF/binary, "): ", ValF/binary>>
    end,
    iolist_to_binary(join(<<", ">>, [F(X) || X <- Xs])).

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
join(_Sep, []) -> [];
join(Sep, [H|T]) -> [H|join_prepend(Sep, T)].

join_prepend(_Sep, []) -> [];
join_prepend(Sep, [H|T]) -> [Sep,H|join_prepend(Sep,T)].

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
type_check_err_msg({enum_not_found, Ty, Val}) ->
    X = io_lib:format("The value ~p is not a valid enum value for type ", [Val]),
    [X, graphql_err:format_ty(Ty)];
type_check_err_msg({param_mismatch, {enum, Ty, OtherTy}}) ->
    ["The enum value is of type ", graphql_err:format_ty(OtherTy),
     " but used in a context where an enum value"
     " of type ", graphql_err:format_ty(Ty), " was expected"];
type_check_err_msg({param_mismatch, Ty, V}) ->
    io_lib:format("The parameter value ~p is not of type ~p", [V, graphql_err:format_ty(Ty)]);
type_check_err_msg({not_input_type, Ty, _}) ->
    ["The type ", graphql_err:format_ty(Ty), " is a valid input type"];
type_check_err_msg({excess_fields_in_object, Fields}) ->
    io_lib:format("The object contains unknown fields and values: ~p", [Fields]);
type_check_err_msg({excess_args, Args}) ->
    io_lib:format("The argument list contains unknown arguments ~p", [Args]);
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
      [SpreadTy, ScopeTy]).


elaborate_err_msg({type_not_found, Ty}) ->
    ["Type not found in schema: ", graphql_err:format_ty(Ty)];
elaborate_err_msg({invalid_directive_location, ID, Context}) ->
    ["The directive ", ID, " is not valid in the context ",
     atom_to_binary(Context, utf8)];
elaborate_err_msg({not_input_type, Ty}) ->
    ["Type ", graphql_err:format_ty(Ty), " is not an input type but is used in input-context"];
elaborate_err_msg({directives_not_unique, X}) ->
    ["The directive with name ", X, " is not unique in this location"];
elaborate_err_msg(no_root_schema) ->
    ["No root schema found. One is required for correct operation"];
elaborate_err_msg({unknown_field, F}) ->
    ["The query refers to a field, ", F, ", which is not present in the schema"];
elaborate_err_msg(unknown_field) ->
    ["The query refers to a field which is not known"];
elaborate_err_msg({unknown_argument, N}) ->
    ["The query refers to an argument, ", N, ", which is not present in the schema"];
elaborate_err_msg({unknown_directive, Dir}) ->
    ["The query uses a directive, ", Dir, ", which is unknown to this GraphQL server"];
elaborate_err_msg(selection_on_scalar) ->
    ["Cannot apply a selection set to a scalar field"];
elaborate_err_msg(selection_on_enum) ->
    ["Cannot apply a selection set to an enum type"];
elaborate_err_msg(fieldless_object) ->
    ["The path refers to an Object type, but no fields were specified"];
elaborate_err_msg(fieldless_interface) ->
    ["The path refers to an Interface type, but no fields were specified"].

validate_err_msg({not_unique, X}) ->
    ["The name ", X, " is not a unique name"];
validate_err_msg({cycles_in_fragments, Cycles}) ->
    io_lib:format("The following fragments contains cycles: ~p", [Cycles]).

