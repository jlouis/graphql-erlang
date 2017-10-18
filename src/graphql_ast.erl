-module(graphql_ast).

-include("graphql_internal.hrl").
-include("graphql_schema.hrl").

-export([resolve_type/1, unwrap_to_base_type/1]).
-export([unwrap_type/1]).
-export([name/1, id/1, typename/1]).

-spec resolve_type(graphql_type()) -> graphql_base_type().
resolve_type({non_null, Ty}) -> resolve_type(Ty);
resolve_type({list, Ty}) -> {list, resolve_type(Ty)};
resolve_type(B) when is_binary(B) -> B;
resolve_type(#scalar_type{} = Ty) -> Ty;
resolve_type(#enum_type{} = Ty) -> Ty;
resolve_type(#input_object_type{} = Ty) -> Ty;
resolve_type(#object_type{} = Ty) -> Ty;
resolve_type(#interface_type{} = Ty) -> Ty;
resolve_type(#union_type{} = Ty) -> Ty;
resolve_type({name, _, N}) -> N.

-spec unwrap_to_base_type(graphql_type()) -> graphql_base_type().
unwrap_to_base_type({name, _, N}) -> N;
unwrap_to_base_type(#enum_type{} = Ty) -> Ty;
unwrap_to_base_type(#input_object_type{} = Ty) -> Ty;
unwrap_to_base_type(#scalar_type{} = Ty) -> Ty;
unwrap_to_base_type(#object_type{} = Ty) -> Ty;
unwrap_to_base_type(#interface_type{} = Ty) -> Ty;
unwrap_to_base_type(#union_type{} = Ty) -> Ty;
unwrap_to_base_type(Ty) when is_binary(Ty) -> Ty;
unwrap_to_base_type({non_null, Ty}) -> unwrap_to_base_type(Ty);
unwrap_to_base_type({list, Ty}) -> unwrap_to_base_type(Ty).

-spec unwrap_type(graphql_type()) -> graphql_base_type().
unwrap_type(Ty) ->
    unwrap_to_base_type(Ty).

-spec name('ROOT' | name()) -> binary().
name('ROOT') -> <<"ROOT">>;
name({name, _Line, X}) -> X;
name({var, N}) -> name(N).

id(E) ->
    case id_(E) of
        {name, _, N} -> N;
        '...' -> <<"...">>;
        'ROOT' -> <<"ROOT">>
    end.

id_(#op { id = ID }) -> ID;
id_(#field { id = ID }) -> ID;
id_(#frag_spread { id = ID }) -> ID;
id_(#frag { id = ID }) -> ID;
id_(#vardef { id = ID }) -> ID.
     
typename(#enum_type { id = ID }) -> ID;
typename(#interface_type { id = ID }) -> ID;
typename(#union_type { id = ID }) -> ID;
typename(#scalar_type { id = ID }) -> ID;
typename(#input_object_type { id = ID }) -> ID;
typename(#object_type { id = ID }) -> ID.

