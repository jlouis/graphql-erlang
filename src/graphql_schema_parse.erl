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
    Mod = mapping(Name, Us),
    Types = [handle_type(M) || M <- Ms],
    {union, #{
       id => Name,
       description => Description,
       resolve_module => Mod,
       types => Types }};
mk(#{ objects := OM }, #p_type {
           id = ID,
           annotations = Annots,
           fields = Fs,
           implements = Impls }) ->
    Name = name(ID),
    Description = description(Annots),
    Mod = mapping(Name, OM),
    Fields = fields(Fs),
    Implements = [name(I) || I <- Impls],
    {object, #{
       id => Name,
       description => Description,
       fields => Fields,
       resolve_module => Mod,
       implements => Implements }};
mk(_Map, #p_enum {
            id = ID,
            annotations = Annots,
            variants = Vs }) ->
    Name = name(ID),
    Description = description(Annots),
    Variants = variants(Vs),
    {enum, #{
       id => Name,
       repr => binary,
       description => Description,
       values => Variants
      }};
mk(_Map, #p_input_object {
            id = ID,
            annotations = Annots,
            defs = Ds }) ->
    Name = name(ID),
    Description = description(Annots),
    Defs = input_defs(Ds),
    {input_object, #{
       id => Name,
       description => Description,
       fields => Defs }};
mk(#{ interfaces := IF }, #p_interface {
                            id = ID,
                            annotations = Annots,
                            fields = FS }) ->
    Name = name(ID),
    Description = description(Annots),
    Mod = mapping(Name, IF),
    Fields = fields(FS),
    {interface, #{
       id => Name,
       description => Description,
       resolve_module => Mod,
       fields => Fields
      }}.
          
fields(Raw) ->
    maps:from_list([field(R) || R <- Raw]).

input_defs(Raw) ->  
    maps:from_list([input_def(D) || D <- Raw]).

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
            find(<<"text">>, Args)
    end.

input_def(#p_input_value { id = ID,
                           annotations = Annots,
                           default = Default, 
                           type = Type }) ->
    Name = name(ID),
    Description = description(Annots),
    K = binary_to_atom(Name, utf8),
    V = #{ type => handle_type(Type),
           default => Default,
           description => Description },
    {K, V}.

field(#p_field_def{ id = ID, annotations = Annots, type = T }) ->
    Name = name(ID),
    Description = description(Annots),
    %% We assume schemas are always under our control, so this is safe
    K = binary_to_atom(Name, utf8),
    V = #{ type => handle_type(T), description => Description },
    {K, V}.

find(_T, []) -> not_found;
find(T, [{{name, _, T}, V}|_]) -> V;
find(T, [{{name, _, _}, _}|Next]) -> find(T, Next);
find(T, [#annotation { id = {name, _, T}} = A|_]) -> A;
find(T, [#annotation{}|Next]) -> find(T, Next).

variants(Vs) ->
    F = fun(V, I) ->
                K = binary_to_atom(V, utf8),
                {K, #{ value => I, description => "No descriptions supported yet" }}
        end,
    maps:from_list(mapi(F, Vs)).
                
mapi(F, L) -> mapi(F, L, 0).

mapi(_F, [], _) -> [];
mapi(F,  [X|Xs], K) -> [F(X, K) | mapi(F, Xs, K+1)].

handle_type({non_null, T}) -> {non_null, handle_type(T)};
handle_type({list, T}) -> {list, handle_type(T)};
handle_type({name, _, T}) -> binary_to_atom(T, utf8);
handle_type({scalar, X}) -> {scalar, X}.

mapping(Name, Map) ->
    case maps:get(Name, Map, undefined) of
        undefined ->
            maps:get(default, Map);
        X -> X
    end.
            
