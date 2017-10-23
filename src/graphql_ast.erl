-module(graphql_ast).

-include("graphql_internal.hrl").
-include("graphql_schema.hrl").

-export([name/1, id/1]).

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
     
