-module(graphql_dot).

-include("graphql_schema.hrl").

-export([dump/1]).

-spec dump(string()) -> ok.
dump(FName) ->
    Graph = x(),
    file:write_file(FName, Graph).

x() ->
    H = header(),
    F = footer(),
    Body = body(),
    [H, Body, F].

footer() -> "}\n".

header() ->
   join(<<"\n">>, [
     "digraph erd {",
     "  node [fontname=Helvetica, fontsize=12,shape=palintext, labelfontname=Helvetica];"
     "  rankdir=LR;",
     "  labeljust=l;",
     "  labelloc= t;",
     "  fontsize=24;",
     "  fontname=Helvetica;",
     "  label = \"ERD Diagram\";",
     "  nodesep=0.3;",
     "  remincross=true;"]).

body() ->
    Entries = graphql_schema:all(),
    Map = maps:from_list([{id(E), E} || E <- Entries]),
    [
      [format(entry(E, Map)) || E <- Entries],
      [format(edge(E, Map)) || E <- Entries]
    ].
    
edge(#object_type { id = <<"__", _/binary>> }, _) -> skip;
edge(#object_type { id = ID, interfaces = IFaces, fields = FS }, M) ->
    {edges,
        [interface(ID, I, M) || I <- IFaces] ++
        lists:usort([e(unnode(ID), F, M) || F <- maps:to_list(FS)])};
edge(#interface_type {}, _M) ->
    skip;
edge(#union_type { id = ID, types = Types }, _) ->
   {edges,
       [union(ID, T) || T <- Types]};
edge(_, _) -> skip.

union(Src, Dest) ->
    {edge, Src, Dest, #{ style => dotted, color => grey }}.

interface(Dest, IF, _M) ->
    {edge, unnode(IF), unnode(Dest), #{ style => dashed, color => grey }}.

e(Src, {_, #schema_field { ty = Ty }}, M) ->
    case unwrap(Ty) of
        skip -> skip;
        Dest -> e_valid(Src, Dest, M)
    end.

e_valid(Src, Dest, M) ->
    case maps:get(Dest, M) of
        #object_type { id = <<"__", _/binary>> } -> skip;
        #object_type{} -> {edge, Src, unnode(Dest)};
        #interface_type{} -> {edge, Src, unnode(Dest)};
        #union_type{} -> {edge, Src, unnode(Dest)};
        #scalar_type{} -> skip;
        #enum_type{} -> skip;
        Obj ->
            io:format("Warning, skipping object: ~p", [Obj]),
            skip
    end.

entry(#object_type { id = <<"__", _/binary>> }, _) -> skip;
entry(#object_type { id = ID, fields = FS }, _) ->
    Fields = fields(FS),
    {node, ID, #{
    	color => <<"black">>,
    	label=> [$", ID, " | ", Fields, $"],
    	shape => 'Mrecord'
    }};
entry(#interface_type { id = ID, fields = FS }, _) ->
    Fields = fields(FS),
    {node, ID, #{
    	color => <<"darkolivegreen4">>,
    	label => [$", unnode(ID), " | ", Fields, $"],
    	shape => record
    }};
entry(#union_type { id = ID }, _) ->
    {node, ID, #{
    	color => deepskyblue4,
    	label => [$", ID, $"],
    	shape => record
    }};
entry(_, _) -> skip.

fields(FS) ->
    Fields = [field(F) || F <- maps:to_list(FS)],
    join(<<" | ">>, Fields).

field({ID, #schema_field { ty = Ty, args = #{}}}) ->
    [ID, " : ", ty(Ty)];
field({ID, #schema_field { ty = Ty, args = Args }}) ->
    As = [arg(A) || A <- maps:to_list(Args)],
    [ID, "(", join(<<", ">>, As), ") : ", ty(Ty)].
    
arg({ID, #schema_arg{ ty = Ty }}) -> [ID, " : ", ty(Ty)].

ty(#scalar_type { id = Id }) -> Id;
ty(B) when is_binary(B) -> B;
ty({list, Ty}) -> ["[", ty(Ty), "]"];
ty({non_null, Ty}) -> [ty(Ty), "!"].

unwrap(Ty) when is_binary(Ty) -> Ty;
unwrap({list, Ty}) -> unwrap(Ty);
unwrap({non_null, Ty}) -> unwrap(Ty);
unwrap(_) -> skip.

unnode(<<"Node">>) -> <<"XNode">>;
unnode(X) -> X.

format(skip) -> "";
format({node, ID, LMap}) ->
    [ID, " [", join(<<", ">>, opts(maps:to_list(LMap))), "];\n"];
format({edges, ES}) ->
    [join(<<"\n">>, [format(E) || E <- ES]), "\n"];
format({edge, Src, Dest}) -> [Src, " -> ", Dest];
format({edge, Src, Dest, Opts}) ->
    [Src, " -> ", Dest, "[", join(<<", ">>, opts(maps:to_list(Opts))), "];\n"].
    
opts([]) -> "";
opts([{K, V} | Ls]) ->
    E = [key(K), "=", value(V)],
    [E | opts(Ls)].

key(B) when is_binary(B) -> B;
key(K) when is_atom(K) -> atom_to_binary(K, utf8).

value(B) when is_binary(B) -> B;
value(A) when is_atom(A) -> atom_to_binary(A, utf8);
value(X) -> iolist_to_binary(X).

id(#enum_type {id = ID }) -> ID;
id(#interface_type { id = ID }) -> ID;
id(#union_type { id = ID }) -> ID;
id(#scalar_type { id = ID }) -> ID;
id(#input_object_type { id = ID }) -> ID;
id(#object_type { id = ID }) -> ID;
id(#root_schema { id = ID }) -> ID;
id(#directive_type{ id = ID }) -> ID.

%% ----------------------------
join(_Sep, []) -> [];
join(Sep, [H|T]) -> [H|join_prepend(Sep, T)].

join_prepend(_Sep, []) -> [];
join_prepend(Sep, [H|T]) -> [Sep,H|join_prepend(Sep,T)].
