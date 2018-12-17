-module(graphql_schema_validate).

-include("graphql_schema.hrl").
-include("graphql.hrl").

-export([x/0, root/1]).

-define (DIRECTIVE_LOCATIONS, [
    'QUERY', 'MUTATION', 'SUBSCRIPTION', 'FIELD', 'FRAGMENT_DEFINITION',
    'FRAGMENT_SPREAD', 'INLINE_FRAGMENT', 'SCHEMA', 'SCALAR', 'OBJECT',
    'FIELD_DEFINITION', 'ARGUMENT_DEFINITION', 'INTERFACE', 'UNION',
    'ENUM', 'ENUM_VALUE', 'INPUT_OBJECT', 'INPUT_FIELD_DEFINITION']).

-spec root(#root_schema{}) -> #root_schema{}.
root(#root_schema{ query = Q,
                   mutation = M,
                   subscription = S} = Root) ->
    ok = x(),
    {ok, QC} = root_lookup(Q, query),
    {ok, MC} = root_lookup(M, mutation),
    {ok, SC} = root_lookup(S, subscription),
    Root#root_schema { query = QC,
                       mutation = MC,
                       subscription = SC }.


root_lookup(undefined, Type) ->
    %% If given an undefined entry, try to use the default
    %% and inject it. Make mention that we are using a default
    %% Value
    Val = case Type of
              query -> <<"Query">>;
              mutation -> <<"Mutation">>;
              subscription -> <<"Subscription">>
          end,
    root_lookup_(Val, Type, default);
root_lookup(Val, Type) ->
    root_lookup_(Val, Type, direct).

%% Root lookup rules are as follows:
%% - Queries MUST exist and it has to be the default value if nothing has
%%   been added
%% - Directly written Mutations and Subscriptions MUST exist
%% - Try to coerce otherwise 
root_lookup_(Q, Type, Def) ->
    case graphql_schema:lookup_ets(Q) of
        not_found when Type == query -> err({schema_without_query, Q});
        not_found when Def == direct -> err({schema_missing_type, Q});
        not_found -> {ok, undefined};
        #object_type{} -> {ok, Q}
    end.

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


validate(#scalar_type {} = X) -> scalar_type(X);
validate(#root_schema {} = X) -> root_schema(X);
validate(#object_type {} = X) -> object_type(X);
validate(#enum_type {} = X) -> enum_type(X);
validate(#interface_type {} = X) -> interface_type(X);
validate(#union_type {} = X) -> union_type(X);
validate(#input_object_type {} = X) -> input_object_type(X);
validate(#directive_type{} = X) -> directive_type(X).

scalar_type(#scalar_type {directives = Ds}) ->
    is_valid_directives(Ds, 'SCALAR'),
    ok.

enum_type(#enum_type { directives = Ds, values = Vs }) ->
    is_valid_directives(Ds, 'ENUM'),
    all(fun schema_enum_value/1, maps:to_list(Vs)),
    ok.

input_object_type(#input_object_type { fields = FS, directives = Ds }) ->
    all(fun schema_input_type_arg/1, maps:to_list(FS)),
    is_valid_directives(Ds, 'INPUT_OBJECT'),
    ok.

union_type(#union_type { types = [] } = Union) ->
    err({empty_union, Union});
union_type(#union_type { types = Types, directives = Ds }) ->
    all(fun is_union_type/1, Types),
    is_valid_directives(Ds, 'UNION'),
    case unique([name(T) || T <- Types]) of
        ok ->
            ok;
        {not_unique, X} ->
            err({union_not_unique, X})
    end.

interface_type(#interface_type { fields= FS, directives = Ds }) ->
    all(fun schema_field/1, maps:to_list(FS)),
    is_valid_directives(Ds, 'INTERFACE'),
    ok.

object_type(#object_type {
	fields = FS,
	interfaces = IFaces,
    directives = Ds} = Obj) ->
    all(fun is_interface/1, IFaces),
    all(fun(IF) -> implements(lookup(IF), Obj) end, IFaces),
    all(fun schema_field/1, maps:to_list(FS)),
    is_valid_directives(Ds, 'OBJECT'),
    ok.

directive_type(#directive_type {
        args = _Args,
        locations = Locations
        }) ->
    all(fun is_directive_location/1, Locations),
    ok.
    

root_schema(#root_schema {
               query = Q,
               mutation = M,
               subscription = S,
               interfaces = IFaces,
               directives = Ds }) ->
    is_object(Q),
    undefined_object(M),
    undefined_object(S),
    all(fun is_interface/1, IFaces),
    is_valid_directives(Ds, 'SCHEMA'),
    ok.
    
schema_field({_, #schema_field { ty = Ty, args = Args, directives = Ds }}) ->
    all(fun schema_input_type_arg/1, maps:to_list(Args)),
    type(Ty),
    is_valid_directives(Ds, 'FIELD_DEFINITION'),
    ok.

schema_input_type_arg({_, #schema_arg { ty = Ty }}) ->
    %% TODO: Default check!
    %% TODO: argument definition directive check
    input_type(Ty),
    ok.

schema_enum_value({_, #enum_value { directives = Ds }}) ->
    is_valid_directives(Ds, 'ENUM_VALUE'),
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

is_union_type(Obj) ->
    case lookup(Obj) of
        #object_type{} -> ok;
        _ -> err({not_union_type, Obj})
    end.

is_valid_directives(Dirs, Location) ->
    all(fun(D) -> is_valid_directive(D, Location) end, Dirs).

is_valid_directive(#directive{ id = Id }, Location) ->
    is_valid_directive(name(Id), Location);
is_valid_directive(Dir, Location) ->
    try lookup(Dir) of
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

is_directive_location(Loc) ->
    case lists:member(Loc, ?DIRECTIVE_LOCATIONS) of
        true -> ok;
        false -> err({not_directive_location, Loc})
    end.

type({non_null, T}) -> type(T);
type({list, T}) -> type(T);
type(X) when is_binary(X) ->
    case lookup(X) of
        #input_object_type {} ->
            err({invalid_output_type, X});

        _ ->
            ok
    end.

input_type({non_null, T}) -> input_type(T);
input_type({list, T}) -> input_type(T);
input_type(X) when is_binary(X) ->
    case lookup(X) of
        #input_object_type {} -> ok;
        #enum_type {} -> ok;
        #scalar_type {} -> ok;
        _V ->
            err({invalid_input_type, X})
    end.

all(_F, []) -> ok;
all(F, [E|Es]) ->
    ok = F(E),
    all(F, Es).

lookup(Key) ->
    case graphql_schema:lookup_ets(Key) of
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
