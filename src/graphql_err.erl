-module(graphql_err).

-include("graphql_internal.hrl").
-include("graphql_schema.hrl").

-export([abort/2, abort/3]).
-export([path/1]).
-export([format_ty/1]).

abort(Path, Msg) ->
    abort(Path, uncategorized, Msg).

-spec abort([any()], any()) -> no_return().
abort(Path, Phase, Msg) ->
   Err = #{
      path => path(lists:reverse(Path)),
      key => err_key(Phase, Msg),
      message => iolist_to_binary(err_msg({Phase, Msg}))
   },
   throw({error, Err}).

%% -- Error handling dispatch to the module responsible for the error
err_msg({elaborate, Reason}) ->
    graphql_elaborate:err_msg(Reason);
err_msg({type_check, Reason}) ->
    graphql_type_check:err_msg(Reason);
err_msg({uncategorized, Reason}) ->
    err_uncategorized(Reason).

err_key(type_check, Key) -> Key;
err_key(elaborate, Key) -> Key;
err_key(uncategorized, Key) -> Key.

err_uncategorized({unknown_type, Ty}) ->
    ["Unknown type: ", Ty];
err_uncategorized({not_unique, Op}) ->
    ["The operation name ",
     Op, " occurs more than once in query document"];
err_uncategorized({unknown_enum_type, Val}) ->
    ["The value ", Val, " does not belong to any enum type known to the schema"];
err_uncategorized({selection_on_enum_type, Ty}) ->
    ["Cannot apply a selection set on the enum type ", format_ty(Ty)];
err_uncategorized(Otherwise) ->
    io_lib:format("General uncategorized error: ~p", [Otherwise]).

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
