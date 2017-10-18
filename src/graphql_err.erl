-module(graphql_err).

-include("graphql_internal.hrl").
-include("graphql_schema.hrl").

-export([mk/3]).
-export([abort/2, abort/3]).
-export([path/1]).
-export([format_ty/1]).

abort(Path, Msg) ->
    abort(Path, uncategorized, Msg).

-spec abort([any()], any()) -> no_return().
abort(Path, Phase, Msg) ->
   Err = mk(Path, Phase, Msg),
   throw({error, Err}).

mk(Path, Phase, Msg) ->
    #{ path => path(lists:reverse(Path)),
       key => err_key(Phase, Msg),
       message => iolist_to_binary(err_msg({Phase, Msg}))
     }.

%% -- Error handling dispatch to the module responsible for the error
err_msg({elaborate, Reason})     -> graphql_elaborate:err_msg(Reason);
err_msg({execute, Reason})       -> graphql_execute:err_msg(Reason);
err_msg({type_check, Reason})    -> graphql_type_check:err_msg(Reason);
err_msg({validate, Reason})      -> graphql_validate:err_msg(Reason);
err_msg({uncategorized, Reason}) ->
    io_lib:format("General uncategorized error: ~p", [Reason]).

err_key(elaborate, Key)     -> Key;
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
