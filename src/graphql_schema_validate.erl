-module(graphql_schema_validate).

-include("graphql_schema.hrl").

-export([x/0]).

-spec x() -> ok.
x() ->
    Objects = graphql_schema:all(),
    try
        [x(Obj) || Obj <- Objects],
        ok
    catch
        throw:Error ->
            %% These errors are usually a bug in the programmers code,
            %% hence they are written to the error_logger as well so
            %% you get nice error messages for them apart from the
            %% Erlang term.
            error_logger:error_msg(format_error(Error)),
            exit(Error)
    end.

x(Obj) ->
    try validate(Obj) of
        ok -> ok
    catch
        throw:{invalid, Reason} ->
            throw({schema_validation, graphql_schema:id(Obj), Reason})
    end.


validate(#scalar_type {}) -> ok;
validate(#root_schema {} = X) -> root_schema(X);
validate(#object_type {} = X) -> object_type(X);
validate(#enum_type {} = X) -> enum_type(X);
validate(#interface_type {} = X) -> interface_type(X);
validate(#union_type {} = X) -> union_type(X);
validate(#input_object_type {} = X) -> input_object_type(X).

enum_type(#enum_type {}) ->
    %% TODO: Validate values
    ok.

input_object_type(#input_object_type { fields = FS }) ->
    all(fun schema_input_type_arg/1, maps:to_list(FS)),
    ok.

union_type(#union_type { types = Types }) ->
    all(fun is_union_type/1, Types),
    ok.

interface_type(#interface_type { fields= FS }) ->
    all(fun schema_field/1, maps:to_list(FS)),
    ok.

object_type(#object_type {
	fields = FS,
	interfaces = IFaces} = Obj) ->
    all(fun is_interface/1, IFaces),
    all(fun(IF) -> implements(lookup(IF), Obj) end, IFaces),
    all(fun schema_field/1, maps:to_list(FS)),
    ok.

root_schema(#root_schema {
	query = Q,
	mutation = M,
	subscription = S,
	interfaces = IFaces }) ->
    undefined_object(Q),
    undefined_object(M),
    undefined_object(S),
    all(fun is_interface/1, IFaces),
    ok.
    
schema_field({_, #schema_field { ty = Ty, args = Args }}) ->
    all(fun schema_input_type_arg/1, maps:to_list(Args)),
    type(Ty),
    ok.

schema_input_type_arg({_, #schema_arg { ty = Ty }}) ->
    %% TODO: Default check!
    input_type(Ty),
    ok.

undefined_object(undefined) -> ok;
undefined_object(Obj) -> is_object(Obj).

implements(#interface_type { fields = IFFields } = IFace,
           #object_type { fields = ObjFields }) ->
    IL = lists:sort(maps:to_list(IFFields)),
    OL = lists:sort(maps:to_list(ObjFields)),
    case implements_field_check(IL, OL) of
        ok ->
            ok;
        {error, Reason} ->
            err({implements, graphql_schema:id(IFace), Reason})
    end.
    
implements_field_check([], []) -> ok;
implements_field_check([], [_|OS]) -> implements_field_check([], OS);
implements_field_check([{K, IF} | IS], [{K, OF} | OS]) ->
    %% TODO: Arg check!
    IFType = IF#schema_field.ty,
    OFType = OF#schema_field.ty,
    case IFType == OFType of
        true ->
            implements_field_check(IS, OS);
        false ->
            {error, {type_mismatch, #{ key => K,
                                       interface => IFType,
                                       object => OFType }}}
    end;
implements_field_check([{IK, _} | _] = IL, [{OK, _} | OS]) when IK > OK ->
    implements_field_check(IL, OS);
implements_field_check([{IK, _} | _], [{OK, _} | _]) when IK < OK ->
    {error, {field_not_found_in_object, IK}}.
    
is_interface(IFace) ->
    case lookup(IFace) of
        #interface_type{} -> ok;
        _ -> err({not_interface, IFace})
    end.

is_object(Obj) ->
    case lookup(Obj) of
        #object_type{} -> ok;
        _ -> err({not_object, Obj})
    end.

is_scalar(Obj) ->
    case lookup(Obj) of
        #scalar_type{} -> ok;
        _ -> err({not_scalar, Obj})
    end.

is_union_type(Obj) ->
    case lookup(Obj) of
        #object_type{} -> ok;
        _ -> err({not_union_type, Obj})
    end.

type({non_null, T}) -> type(T);
type({list, T}) -> type(T);
type({scalar, S}) -> scalar(S);
type(X) when is_binary(X) ->
    case lookup(X) of
        #input_object_type {} ->
            err({invalid_output_type, X});

        _ ->
            ok
    end.

input_type({non_null, T}) -> input_type(T);
input_type({list, T}) -> input_type(T);
input_type({scalar, S}) -> scalar(S);
input_type(X) when is_binary(X) ->
    case lookup(X) of
        #input_object_type {} ->
            ok;

        #enum_type {} ->
            ok;

        #scalar_type {} ->
            ok;

        _V ->
            err({invalid_input_type, X})
    end.

scalar(string) -> ok;
scalar(id) -> ok;
scalar(float) -> ok;
scalar(int) -> ok;
scalar(bool) -> ok;
scalar(X) -> is_scalar(X).

all(_F, []) -> ok;
all(F, [E|Es]) ->
    ok = F(E),
    all(F, Es).

lookup(Key) ->
    case graphql_schema:lookup(Key) of
        not_found -> err({not_found, Key});
        X -> X
    end.
    
err(Reason) -> throw({invalid, Reason}).

format_error(X) -> iolist_to_binary(err_fmt(X)).

err_fmt({schema_validation, Type, {not_found, NF}}) ->
    io_lib:format(
      "Schema Error in type ~p: it refers to a type ~p, "
      "which is not present in the schema", [Type, NF]);
err_fmt(X) ->
    io_lib:format(
      "Unhandled schema validator error message: ~p", [X]).
