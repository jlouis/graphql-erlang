-module(graphql_err).

-include("graphql_internal.hrl").
-include("graphql_schema.hrl").

-export([abort/2]).
-export([path/1]).

-spec abort([any()], any()) -> no_return().
abort(Path, Msg) ->
   Err = #{
      path => path(lists:reverse(Path)),
      key => Msg,
      message => iolist_to_binary(err_msg(Msg))
   },
   throw({error, Err}).

err_msg({type_not_found, Ty}) ->
    ["Type not found in schema: ", Ty];
err_msg(no_root_schema) ->
    ["No root schema found"];
err_msg({entity_not_found, Ty}) ->
    ["Unknown object/interface: ", Ty];
err_msg({unknown_fragment, F}) ->
    ["Unknown fragment: ", F];
err_msg({unknown_field, F}) ->
    ["Unknown field: ", F];
err_msg({unknown_type, Ty}) ->
    ["Unknown type: ", Ty];
err_msg(selection_on_scalar) ->
    ["Cannot apply a selection set to a scalar field"];
err_msg({unknown_enum, E}) ->
    ["The enum name ", E, " is not known to the schema"];
err_msg(fieldless_object) ->
    ["The path refers to an Object, but no fields were specified"];
err_msg({type_mismatch, #{ id := ID, document := Doc, schema := Sch }}) ->
    ["Type mismatch on (", ID, "). The query document has a value/variable of type (",
      format_ty(Doc), ") but the schema expects type (", format_ty(Sch), ")"];
err_msg({type_mismatch, #{ schema := Sch }}) ->
    ["Type mismatch, expected (", format_ty(Sch), ")"];
err_msg({enum_not_found, Ty, Val}) ->
    X = io_lib:format("The value ~p is not a valid enum value for type ", [Val]),
    [X, format_ty(Ty)];
err_msg({param_mismatch, Ty, _}) ->
    ["Parameter is not of type ", format_ty(Ty)];
err_msg({unknown_enum_type, Val}) ->
    ["The value ", Val, " does not belong to any enum type known to the schema"];
err_msg(fieldless_interface) ->
    ["The path refers to an Interface, but no fields were specified"];
err_msg({operation_not_found, Op}) ->
    ["Expected an operation ", Op, " but no such operation was found"];
err_msg(params_on_unnamed) ->
    ["Cannot supply parameter lists to unnamed (anonymous) queries"];
err_msg({selection_on_enum_type, Ty}) ->
    ["Cannot apply a selection set on the enum type ", format_ty(Ty)];
err_msg(missing_non_null_param) ->
    ["The parameter is non-null, but was undefined in parameter list"];
err_msg({unbound_variable, Var}) ->
    ["The document refers to a variable ", Var, " but no such var exists. Perhaps the variable is a typo?"];
err_msg({input_coerce_abort, {Class, Reason}}) ->
    io_lib:format("Input coercer failed with an exception of class ~p and reason ~p", [Class, Reason]);
err_msg({input_coercion, Type, Value, Reason}) ->
    io_lib:format("Input coercion failed for type ~s with value ~p. The reason it failed is: ~p", [Type, Value, Reason]).

-spec path([Input]) -> [binary()]
  when
    Input :: document | frag() | op() | field() | binary() | [Input].
path(Path) ->
   F = fun
           F('ROOT') -> <<"ROOT">>;
           F(document) -> <<"document">>;
           F(#frag { id = ID }) -> name(ID);
           F(#op { id = ID }) -> name(ID);
           F(#field { id = ID }) -> name(ID);
           F(#directive { id = ID }) -> name(ID);
           F(#enum_type { id = ID }) -> name(ID);
           F(#interface_type { id = ID }) -> name(ID);
           F(#union_type { id = ID }) -> name(ID);
           F(#object_type { id = ID }) -> name(ID);
           F({name, _, _} = Name) -> name(Name);
           F(I) when is_integer(I) -> integer_to_binary(I);
           F(B) when is_binary(B) -> B;
           F(L) when is_list(L) ->
               [F(X) || X <- L]
       end,
   lists:flatten([F(Elem) || Elem <- Path]).

format_ty(#enum_type { id = Ty }) -> Ty;
format_ty(#input_object_type { id = Ty }) -> Ty;
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
format_ty({scalar, X, _}) -> format_scalar(X);
format_ty({scalar, X}) -> format_scalar(X);
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

format_scalar(string) -> <<"string">>;
format_scalar(bool) -> <<"bool">>;
format_scalar(int) -> <<"int">>;
format_scalar(float) -> <<"float">>;
format_scalar(id) -> <<"id">>;
format_scalar(B) when is_binary(B) -> B.

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
