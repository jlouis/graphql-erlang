-module(graphql_schema_parse).

-include("graphql_internal.hrl").
-include("graphql_schema.hrl").

-export([inject/3]).
inject(EP, BaseMapping, {ok, {document, Entries}}) ->
    Mapping = handle_mapping(BaseMapping),
    {SchemaEntries, Other} = lists:partition(fun schema_defn/1, Entries),
    report_other_entries(Other),
    Defs = [mk(EP, Mapping, E) || E <- SchemaEntries],
    [inject(Def) || Def <- Defs],
    ok.

mk(_EP, #{ scalars := Sc }, #p_scalar { id = ID, annotations = Annots }) ->
    Name = name(ID),
    Description = description(Annots),
    Mod = mapping(Name, Sc),
    {scalar, #{
       id => Name,
       description => Description,
       annotations => annotations(Annots),
       resolve_module => Mod
      }};
mk(EP, #{ unions := Us }, #p_union { id = ID,
                                 annotations = Annots,
                                 members = Ms }) ->
    Name = name(ID),
    Description = description(Annots),
    Mod = mapping(Name, Us),
    Types = [handle_type(EP, M) || M <- Ms],
    {union, #{
       id => Name,
       description => Description,
       annotations => annotations(Annots),
       resolve_module => Mod,
       types => Types }};
mk(EP, #{ objects := OM }, #p_type {
           id = ID,
           annotations = Annots,
           fields = Fs,
           implements = Impls }) ->
    Name = name(ID),
    Description = description(Annots),
    Mod = mapping(Name, OM),
    Fields = fields(EP, Fs),
    Implements = [name(I) || I <- Impls],
    {object, #{
       id => Name,
       description => Description,
       fields => Fields,
       annotations => annotations(Annots),
       resolve_module => Mod,
       interfaces => Implements }};
mk(_EP, #{ enums := En }, #p_enum { id = ID,
                               annotations = Annots,
                               variants = Vs }) ->
    Name = name(ID),
    Description = description(Annots),
    Variants = variants(Vs),
    Mod = mapping(Name, En),
    {enum, #{
       id => Name,
       description => Description,
       annotations => annotations(Annots),
       values => Variants,
       resolve_module => Mod }};
mk(EP, _Map, #p_input_object {
            id = ID,
            annotations = Annots,
            defs = Ds }) ->
    Name = name(ID),
    Description = description(Annots),
    Defs = input_defs(EP, Ds),
    {input_object, #{
       id => Name,
       description => Description,
       annotations => annotations(Annots),
       fields => Defs }};
mk(EP, #{ interfaces := IF }, #p_interface {
                            id = ID,
                            annotations = Annots,
                            fields = FS }) ->
    Name = name(ID),
    Description = description(Annots),
    Mod = mapping(Name, IF),
    Fields = fields(EP, FS),
    {interface, #{
       id => Name,
       description => Description,
       resolve_module => Mod,
       annotations => annotations(Annots),
       fields => Fields
      }}.

fields(EP, Raw) ->
    maps:from_list([field(EP, R) || R <- Raw]).

input_defs(EP, Raw) ->
    maps:from_list([input_def(EP, D) || D <- Raw]).

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
    error_logger:error_msg("Loading graphql schema from file, "
                           "but it contains non-schema entries: ~p~n",
                           [Es]),
    ok.

handle_mapping(Map) ->
    F = fun(_K, V) -> handle_map(V) end,
    maps:map(F, Map).

handle_map(M) ->
    L = maps:to_list(M),
    maps:from_list(
      lists:map(fun({K, V}) -> {binarize(K), V} end, L)).

binarize(default) -> default;
binarize(A) when is_atom(A) -> atom_to_binary(A, utf8).

name({name, _, N}) -> N.

annotations(As) ->
    maps:from_list([annotation(A) || A <- As]).

annotation(#annotation { id = {name, _, T}, args = Args }) ->
    {T, maps:from_list([annotation_arg(Ag) || Ag <- Args])}.

annotation_arg({{name, _, A}, Val}) -> {A, Val}.

description(Annots) ->
    case find(<<"description">>, Annots) of
        not_found ->
            <<"No description provided">>;
        #annotation { args = Args } ->
            find(<<"text">>, Args)
    end.

input_def(EP, #p_input_value { id = ID,
                           annotations = Annots,
                           default = Default,
                           type = Type }) ->
    Name = name(ID),
    Description = description(Annots),
    K = binary_to_atom(Name, utf8),
    V = #{ type => handle_type(EP, Type),
           default => Default,
           annotations => annotations(Annots),
           description => Description },
    {K, V}.

field(EP, #p_field_def{ id = ID, annotations = Annots, type = T, args = Args }) ->
    Name = name(ID),
    Description = description(Annots),
    %% We assume schemas are always under our control, so this is safe
    K = binary_to_atom(Name, utf8),
    V = #{ type => handle_type(EP, T),
           description => Description,
           annotations => annotations(Annots),
           args => handle_args(EP, Args)},
    {K, V}.

handle_args(EP, Args) ->
    maps:from_list([input_def(EP, A) || A <- Args]).

find(_T, []) -> not_found;
find(T, [{{name, _, T}, V}|_]) -> V;
find(T, [{{name, _, _}, _}|Next]) -> find(T, Next);
find(T, [#annotation { id = {name, _, T}} = A|_]) -> A;
find(T, [#annotation{}|Next]) -> find(T, Next).

variants(Vs) ->
    F = fun(#p_enum_value { id = V,
                            annotations = Annots }, I) ->
                K = binary_to_atom(V, utf8),
                Description = description(Annots),
                {K, #{ value => I,
                       annotations => annotations(Annots),
                       description => Description }}
        end,
    maps:from_list(mapi(F, Vs)).

mapi(F, L) -> mapi(F, L, 0).

mapi(_F, [], _) -> [];
mapi(F,  [X|Xs], K) -> [F(X, K) | mapi(F, Xs, K+1)].

handle_type(EP, {non_null, T}) -> {non_null, handle_type(EP, T)};
handle_type(EP, {list, T}) -> {list, handle_type(EP, T)};
handle_type(_EP, {name, _, T}) -> binary_to_atom(T, utf8);
handle_type(EP, {scalar, Name}) ->
    #scalar_type{} = Ty = graphql_schema:get(EP, Name),
    handle_type(EP, Ty);
handle_type(_EP, #scalar_type{ id = Id }) -> binary_to_atom(Id, utf8).

mapping(Name, Map) ->
    case maps:get(Name, Map, undefined) of
        undefined ->
            maps:get(default, Map);
        X -> X
    end.
