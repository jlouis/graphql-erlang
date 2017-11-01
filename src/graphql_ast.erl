-module(graphql_ast).

-include("graphql_internal.hrl").
-include("graphql_schema.hrl").

-export([name/1, id/1, typename/1]).

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
id_(#vardef { id = ID }) -> ID;
id_(#directive { id = ID }) -> ID.
     
typename(#enum_type { id = ID }) -> ID;
typename(#interface_type { id = ID }) -> ID;
typename(#union_type { id = ID }) -> ID;
typename(#scalar_type { id = ID }) -> ID;
typename(#input_object_type { id = ID }) -> ID;
typename(#object_type { id = ID }) -> ID.

