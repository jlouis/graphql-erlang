-module(graphql_schema_validate).

-include("graphql_schema.hrl").
-include("graphql.hrl").

-export([x/1, root/2]).

-define (DIRECTIVE_LOCATIONS, [
    'QUERY', 'MUTATION', 'SUBSCRIPTION', 'FIELD', 'FRAGMENT_DEFINITION',
    'FRAGMENT_SPREAD', 'INLINE_FRAGMENT', 'SCHEMA', 'SCALAR', 'OBJECT',
    'FIELD_DEFINITION', 'ARGUMENT_DEFINITION', 'INTERFACE', 'UNION',
    'ENUM', 'ENUM_VALUE', 'INPUT_OBJECT', 'INPUT_FIELD_DEFINITION']).

-spec root(endpoint_context(), #root_schema{}) -> #root_schema{}.
root(Ep, #root_schema{ query = Q,
                   mutation = M,
                   subscription = S} = Root) ->
    ok = x(Ep),
    {ok, QC} = root_lookup(Ep, Q, query),
    {ok, MC} = root_lookup(Ep, M, mutation),
    {ok, SC} = root_lookup(Ep, S, subscription),
    Root#root_schema { query = QC,
                       mutation = MC,
                       subscription = SC }.


root_lookup(Ep, undefined, Type) ->
    %% If given an undefined entry, try to use the default
    %% and inject it. Make mention that we are using a default
    %% Value
    Val = case Type of
              query -> <<"Query">>;
              mutation -> <<"Mutation">>;
              subscription -> <<"Subscription">>
          end,
    root_lookup_(Ep, Val, Type, default);
root_lookup(Ep, Val, Type) ->
    root_lookup_(Ep, Val, Type, direct).

%% Root lookup rules are as follows:
%% - Queries MUST exist and it has to be the default value if nothing has
%%   been added
%% - Directly written Mutations and Subscriptions MUST exist
%% - Try to coerce otherwise 
root_lookup_(Ep, Q, Type, Def) ->
    case graphql_schema:lookup(Ep, Q) of
        not_found when Type == query -> err({schema_without_query, Q});
        not_found when Def == direct -> err({schema_missing_type, Q});
        not_found -> {ok, undefined};
        #object_type{} -> {ok, Q}
    end.

-spec x(endpoint_context()) -> ok.
x(Ep) ->
    Objects = graphql_schema:all(Ep),
    try
        [x(Ep, Obj) || Obj <- Objects],
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

x(Ep, Obj) ->
    try validate(Ep, Obj) of
        ok -> ok
    catch
        throw:{invalid, Reason} ->
            throw({schema_validation, graphql_schema:id(Obj), Reason})
    end.


validate(Ep, #scalar_type {} = X) -> scalar_type(Ep, X);
validate(Ep, #root_schema {} = X) -> root_schema(Ep, X);
validate(Ep, #object_type {} = X) -> object_type(Ep, X);
validate(Ep, #enum_type {} = X) -> enum_type(Ep, X);
validate(Ep, #interface_type {} = X) -> interface_type(Ep, X);
validate(Ep, #union_type {} = X) -> union_type(Ep, X);
validate(Ep, #input_object_type {} = X) -> input_object_type(Ep, X);
validate(Ep, #directive_type{} = X) -> directive_type(Ep, X).

scalar_type(Ep, #scalar_type {directives = Ds}) ->
    is_valid_directives(Ep, Ds, 'SCALAR'),
    ok.

enum_type(Ep, #enum_type { directives = Ds, values = Vs }) ->
    is_valid_directives(Ep, Ds, 'ENUM'),
    all(Ep, fun schema_enum_value/2, maps:to_list(Vs)),
    ok.

input_object_type(Ep, #input_object_type { fields = FS, directives = Ds }) ->
    all(Ep, fun schema_input_type_arg/2, maps:to_list(FS)),
    is_valid_directives(Ep, Ds, 'INPUT_OBJECT'),
    ok.

union_type(_Ep, #union_type { types = [] } = Union) ->
    err({empty_union, Union});
union_type(Ep, #union_type { types = Types, directives = Ds }) ->
    all(Ep, fun is_union_type/2, Types),
    is_valid_directives(Ep, Ds, 'UNION'),
    case unique([name(T) || T <- Types]) of
        ok ->
            ok;
        {not_unique, X} ->
            err({union_not_unique, X})
    end.

interface_type(Ep, #interface_type { fields= FS, directives = Ds }) ->
    all(Ep, fun schema_field/2, maps:to_list(FS)),
    is_valid_directives(Ep, Ds, 'INTERFACE'),
    ok.

object_type(Ep, #object_type {
	fields = FS,
	interfaces = IFaces,
    directives = Ds} = Obj) ->
    all(Ep, fun is_interface/2, IFaces),
    all(Ep, fun(Ctx, IF) -> implements(lookup(Ctx, IF), Obj) end, IFaces),
    all(Ep, fun schema_field/2, maps:to_list(FS)),
    is_valid_directives(Ep, Ds, 'OBJECT'),
    ok.

directive_type(Ep, #directive_type {
        args = _Args,
        locations = Locations
        }) ->
    all(Ep, fun is_directive_location/2, Locations),
    ok.
    

root_schema(Ep, #root_schema {
               query = Q,
               mutation = M,
               subscription = S,
               interfaces = IFaces,
               directives = Ds }) ->
    is_object(Ep, Q),
    undefined_object(Ep, M),
    undefined_object(Ep, S),
    all(Ep, fun is_interface/2, IFaces),
    is_valid_directives(Ep, Ds, 'SCHEMA'),
    ok.
    
schema_field(Ep, {_, #schema_field { ty = Ty, args = Args, directives = Ds }}) ->
    all(Ep, fun schema_input_type_arg/2, maps:to_list(Args)),
    type(Ep, Ty),
    is_valid_directives(Ep, Ds, 'FIELD_DEFINITION'),
    ok.

schema_input_type_arg(Ep, {_, #schema_arg { ty = Ty }}) ->
    %% TODO: Default check!
    %% TODO: argument definition directive check
    input_type(Ep, Ty),
    ok.

schema_enum_value(Ep, {_, #enum_value { directives = Ds }}) ->
    is_valid_directives(Ep, Ds, 'ENUM_VALUE'),
    ok.

undefined_object(_Ep, undefined) -> ok;
undefined_object(Ep, Obj) -> is_object(Ep, Obj).

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
    
is_interface(Ep, IFace) ->
    case lookup(Ep, IFace) of
        #interface_type{} -> ok;
        _ -> err({not_interface, IFace})
    end.

is_object(Ep, Obj) ->
    case lookup(Ep, Obj) of
        #object_type{} -> ok;
        _ -> err({not_object, Obj})
    end.

is_union_type(Ep, Obj) ->
    case lookup(Ep, Obj) of
        #object_type{} -> ok;
        _ -> err({not_union_type, Obj})
    end.

is_valid_directives(Ep, Dirs, Location) ->
    all(Ep, fun(Ctx, D) -> is_valid_directive(Ctx, D, Location) end, Dirs).

is_valid_directive(Ep, #directive{ id = Id }, Location) ->
    is_valid_directive(Ep, name(Id), Location);
is_valid_directive(Ep, Dir, Location) ->
    try lookup(Ep, Dir) of
        #directive_type{ id = Id, locations = Locations } ->
            case lists:member(Location, Locations) of
                true -> ok;
                false -> err({invalid_directive_use, #{
                        directive_type => Id,
                        used_at        => Location,
                        allowed_at     => Locations
                    }})
            end;
        _ -> err({not_directive, Dir})
    catch
        throw:{invalid, {not_found, _Key}} ->
            err({not_directive, Dir})
    end.

is_directive_location(_Ep, Loc) ->
    case lists:member(Loc, ?DIRECTIVE_LOCATIONS) of
        true -> ok;
        false -> err({not_directive_location, Loc})
    end.

type(Ep, {non_null, T}) -> type(Ep, T);
type(Ep, {list, T}) -> type(Ep, T);
type(Ep, X) when is_binary(X) ->
    case lookup(Ep, X) of
        #input_object_type {} ->
            err({invalid_output_type, X});

        _ ->
            ok
    end.

input_type(Ep, {non_null, T}) -> input_type(Ep, T);
input_type(Ep, {list, T}) -> input_type(Ep, T);
input_type(Ep, X) when is_binary(X) ->
    case lookup(Ep, X) of
        #input_object_type {} -> ok;
        #enum_type {} -> ok;
        #scalar_type {} -> ok;
        _V ->
            err({invalid_input_type, X})
    end.

all(_Ep, _F, []) -> ok;
all(Ep, F, [E|Es]) ->
    ok = F(Ep, E),
    all(Ep, F, Es).

lookup(Ep, Key) ->
    case graphql_schema:lookup(Ep, Key) of
        not_found -> err({not_found, Key});
        X -> X
    end.

unique([]) -> ok;
unique([X|Xs]) ->
    case lists:member(X, Xs) of
        false -> unique(Xs);
        true ->
            {not_unique, X}
    end.

name({name, _, N}) -> N;
name(X) when is_binary(X) -> X.
    
err(Reason) -> throw({invalid, Reason}).

format_error(X) -> binary_to_list(
                     iolist_to_binary(
                       err_fmt(X))).

err_fmt({schema_validation, Type, {not_found, NF}}) ->
    io_lib:format(
      "Schema Error in type ~p: it refers to a type ~p, "
      "which is not present in the schema", [Type, NF]);
err_fmt({schema_validation, Type, {not_directive, D}}) ->
    io_lib:format(
      "Schema Error in type ~p: it refers to a directive ~p, "
      "which is not present in the schema", [Type, D]);
err_fmt({schema_validation, Type, {union_not_unique, X}}) ->
    io_lib:format(
      "Schema Error in type ~p: it contains duplicate types ~p",
      [Type, X]);
err_fmt(X) ->
    io_lib:format(
      "Unhandled schema validator error message: ~p", [X]).
