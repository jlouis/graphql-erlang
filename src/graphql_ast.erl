-module(graphql_ast).

-include("graphql_internal.hrl").

-export([resolve_type/1, resolve_root_type/4, unwrap_to_base_type/1]).
-export([unwrap_type/1]).
-export([name/1]).

-spec resolve_root_type(undefined | operation_type(), X, X, X) -> X.
resolve_root_type(undefined, Q, _, _) -> Q;
resolve_root_type({query, _}, Q, _, _) -> Q;
resolve_root_type({mutation, _}, _, M, _) -> M;
resolve_root_type({subscription, _}, _, _, S) -> S.

-spec resolve_type(graphql_type()) -> graphql_type_resolution().
resolve_type({scalar, Sc}) -> {scalar, Sc};
resolve_type({non_null, Ty}) -> resolve_type(Ty);
resolve_type([Ty]) -> {list, resolve_type(Ty)};
resolve_type(B) when is_binary(B) -> B.

-spec unwrap_to_base_type(graphql_type_resolution()) -> graphql_type_resolution().
unwrap_to_base_type({scalar, X}) -> {scalar, X};
unwrap_to_base_type(Ty) when is_binary(Ty) -> Ty;
unwrap_to_base_type({non_null, Ty}) -> unwrap_to_base_type(Ty);
unwrap_to_base_type({list, Ty}) -> unwrap_to_base_type(Ty).

-spec unwrap_type(graphql_type()) -> graphql_type_resolution().
unwrap_type(Ty) ->
    unwrap_to_base_type(resolve_type(Ty)).

-spec name('ROOT' | name()) -> binary().
name('ROOT') -> 'ROOT';
name({name, X, _}) -> X.
