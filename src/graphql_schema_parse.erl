-module(graphql_schema_parse).

-include("graphql_internal.hrl").

-export([inject/2]).
inject(BaseMapping, {ok, {document, Entries}}) ->
    Mapping = handle_mapping(BaseMapping),
    {SchemaEntries, Other} = lists:partition(fun schema_defn/1, Entries),
    report_other_entries(Other),
    Defs = [mk(Mapping, E) || E <- SchemaEntries],
    [inject(Def) || Def <- Defs],
    ok.

mk(#{ scalars := Sc }, #p_scalar { id = ID, annotations = Annots }) ->
    Name = name(ID),
    Description = description(Annots),
    CoerceMod = maps:get(Name, Sc, undefined),
    {scalar, #{
       id => Name,
       description => Description,
       coerce_module => CoerceMod
      }};
mk(#{ unions := Us }, #p_union { id = ID,
                                 annotations = Annots,
                                 members = Ms }) ->
    Name = name(ID),
    Description = description(Annots),
    Mod = maps:get(Name, Us),
    Types = [name(M) || M <- Ms],
    {union, #{
       id => Name,
       description => Description,
       resolve_module => Mod,
       types => Types }};
mk(#{ interfaces := IF }, #p_interface {
                            id = ID,
                            annotations = Annots,
                            fields = FS }) ->
    Name = name(ID),
    Description = description(Annots),
    Mod = maps:get(Name, IF),
    Fields = fields(FS),
    {interface, #{
       id => Name,
       description => Description,
       resolve_module => Mod,
       fields => Fields
      }}.
          
fields(Raw) ->
    maps:from_list([field(R) || R <- Raw]).
  

inject(Def) ->
    ok = graphql:insert_schema_definition(Def).

schema_defn(#p_type{}) -> true;
schema_defn(#p_input_object{}) -> true;
schema_defn(#p_interface{}) -> true;
schema_defn(#p_union{}) -> true;
schema_defn(#p_scalar{}) -> true;
schema_defn(#p_enum{}) -> true;
schema_defn(_) -> false.

report_other_entries([]) -> ok;
report_other_entries(Es) ->
    lager:warning("Loading graphql schema from file, but it contains non-schema entries: ~p", [Es]),
    ok.

handle_mapping(Map) ->
    F = fun(_K, V) -> handle_map(V) end,
    maps:map(F, Map).

handle_map(M) ->
    L = maps:to_list(M),
    maps:from_list(
      lists:map(fun({K, V}) -> {binarize(K), V} end, L)).

binarize(A) when is_atom(A) ->
    atom_to_binary(A, utf8).

name({name, _, N}) -> N.
    
                        
description(Annots) ->
    case find(<<"description">>, Annots) of
        not_found ->
            <<"No description provided">>;
        #annotation { args = Args } ->
            ct:pal("Args is: ~p", [Args]),
            <<"TODO">>
    end.

field(#p_field_def{ id = ID, annotations = Annots, type = T }) ->
    Name = name(ID),
    Description = description(Annots),
    %% We assume schemas are always under our control, so this is safe
    K = binary_to_atom(Name, utf8),
    V = #{ type => T, description => Description },
    {K, V}.

find(_T, []) -> not_found;
find(T, [#annotation { id = {name, _, T}} = A|_]) -> A;
find(T, [#annotation{}|Next]) -> find(T, Next).

