-module(graphql_ast).

-include("graphql_internal.hrl").
-include("graphql_schema.hrl").

-export([resolve_type/1, unwrap_to_base_type/1]).
-export([unwrap_type/1]).
-export([name/1]).

-spec resolve_type(graphql_type()) -> tycond().
resolve_type({scalar, Sc}) -> {scalar, Sc};
resolve_type({non_null, Ty}) -> resolve_type(Ty);
resolve_type([Ty]) -> {list, resolve_type(Ty)};
resolve_type(B) when is_binary(B) -> B;
resolve_type(#scalar_type{} = Ty) -> Ty;
resolve_type(#enum_type{} = Ty) -> Ty;
resolve_type(#input_object_type{} = Ty) -> Ty;
resolve_type({name, N, _}) -> N.

-spec unwrap_to_base_type(graphql_type()) -> tycond().
unwrap_to_base_type({scalar, X}) -> {scalar, X};
unwrap_to_base_type({name, N, _}) -> N;
unwrap_to_base_type(#enum_type{} = Ty) -> Ty;
unwrap_to_base_type(#input_object_type{} = Ty) -> Ty;
unwrap_to_base_type(#scalar_type{} = Ty) -> Ty;
unwrap_to_base_type(Ty) when is_binary(Ty) -> Ty;
unwrap_to_base_type({non_null, Ty}) -> unwrap_to_base_type(Ty);
unwrap_to_base_type({list, Ty}) -> unwrap_to_base_type(Ty).

-spec unwrap_type(graphql_type()) -> tycond().
unwrap_type(Ty) ->
    unwrap_to_base_type(resolve_type(Ty)).

-spec name('ROOT' | name()) -> binary().
name('ROOT') -> <<"ROOT">>;
name({name, X, _}) -> X;
name({var, N}) -> name(N).

