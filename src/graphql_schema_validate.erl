-module(graphql_schema_validate).

-include("graphql_schema.hrl").

-export([x/1]).

-spec x(graphql_schema:endpoint_context()) -> ok.
x(EP) ->
    Objects = graphql_schema:all(EP),
    try
        [x(EP, Obj) || Obj <- Objects],
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

x(EP, Obj) ->
    try validate(EP, Obj) of
        ok -> ok
    catch
        throw:{invalid, Reason} ->
            throw({schema_validation, graphql_schema:id(Obj), Reason})
    end.


validate(_EP, #scalar_type {}) -> ok;
validate(EP, #root_schema {} = X) -> root_schema(EP, X);
validate(EP, #object_type {} = X) -> object_type(EP, X);
validate(EP, #enum_type {} = X) -> enum_type(EP, X);
validate(EP, #interface_type {} = X) -> interface_type(EP, X);
validate(EP, #union_type {} = X) -> union_type(EP, X);
validate(EP, #input_object_type {} = X) -> input_object_type(EP, X).

enum_type(_EP, #enum_type {}) ->
    %% TODO: Validate values
    ok.

input_object_type(EP, #input_object_type { fields = FS }) ->
    all(EP, fun schema_input_type_arg/2, maps:to_list(FS)),
    ok.

union_type(EP, #union_type { types = Types }) ->
    all(EP, fun is_union_type/2, Types),
    ok.

interface_type(EP, #interface_type { fields= FS }) ->
    all(EP, fun schema_field/2, maps:to_list(FS)),
    ok.

object_type(EP, #object_type {
	fields = FS,
	interfaces = IFaces} = Obj) ->
    all(EP, fun is_interface/2, IFaces),
    all(EP, fun(EPi, IF) -> implements(lookup(EPi, IF), Obj) end, IFaces),
    all(EP, fun schema_field/2, maps:to_list(FS)),
    ok.

root_schema(EP, #root_schema {
	query = Q,
	mutation = M,
	subscription = S,
	interfaces = IFaces }) ->
    undefined_object(EP, Q),
    undefined_object(EP, M),
    undefined_object(EP, S),
    all(EP, fun is_interface/2, IFaces),
    ok.
    
schema_field(EP, {_, #schema_field { ty = Ty, args = Args }}) ->
    all(EP, fun schema_input_type_arg/2, maps:to_list(Args)),
    type(EP, Ty),
    ok.

schema_input_type_arg(EP, {_, #schema_arg { ty = Ty }}) ->
    %% TODO: Default check!
    input_type(EP, Ty),
    ok.

undefined_object(_EP, undefined) -> ok;
undefined_object(EP, Obj) -> is_object(EP, Obj).

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
    
is_interface(EP, IFace) ->
    case lookup(EP, IFace) of
        #interface_type{} -> ok;
        _ -> err({not_interface, IFace})
    end.

is_object(EP, Obj) ->
    case lookup(EP, Obj) of
        #object_type{} -> ok;
        _ -> err({not_object, Obj})
    end.

is_union_type(EP, Obj) ->
    case lookup(EP, Obj) of
        #object_type{} -> ok;
        _ -> err({not_union_type, Obj})
    end.

type(EP, {non_null, T}) -> type(EP, T);
type(EP, {list, T}) -> type(EP, T);
type(EP, X) when is_binary(X) ->
    case lookup(EP, X) of
        #input_object_type {} ->
            err({invalid_output_type, X});

        _ ->
            ok
    end.

input_type(EP, {non_null, T}) -> input_type(EP, T);
input_type(EP, {list, T}) -> input_type(EP, T);
input_type(EP, X) when is_binary(X) ->
    case lookup(EP, X) of
        #input_object_type {} -> ok;
        #enum_type {} -> ok;
        #scalar_type {} -> ok;
        _V ->
            err({invalid_input_type, X})
    end.

all(_EP, _F, []) -> ok;
all(EP, F, [E|Es]) ->
    ok = F(EP, E),
    all(EP, F, Es).

lookup(EP, Key) ->
    case graphql_schema:lookup(EP, Key) of
        not_found -> err({not_found, Key});
        X -> X
    end.
    
err(Reason) -> throw({invalid, Reason}).

format_error(X) -> binary_to_list(
                     iolist_to_binary(
                       err_fmt(X))).

err_fmt({schema_validation, Type, {not_found, NF}}) ->
    io_lib:format(
      "Schema Error in type ~p: it refers to a type ~p, "
      "which is not present in the schema", [Type, NF]);
err_fmt(X) ->
    io_lib:format(
      "Unhandled schema validator error message: ~p", [X]).
