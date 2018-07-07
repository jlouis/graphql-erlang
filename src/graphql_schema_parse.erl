-module(graphql_schema_parse).

-include("graphql_internal.hrl").
-include("graphql_schema.hrl").

-export([inject/2]).

inject(BaseMapping, {ok, {document, Entries}}) ->
    Mapping = handle_mapping(BaseMapping),
    {SchemaEntries, Other} = lists:partition(fun schema_defn/1, Entries),
    report_other_entries(Other),
    Defs = [mk(Mapping, E) || E <- SchemaEntries],
    [inject(Def) || Def <- Defs],
    ok.

mk(#{}, #p_schema_definition { directives = Directives,
                               defs = RootOps }) ->
    {schema,
     #{ defs => maps:from_list([root_op(R) || R <- RootOps]),
        directives => Directives }};
mk(#{ scalars := Sc }, #p_scalar { description = Desc,
                                   directives = Directives,
                                   id = ID }) ->
    Name = name(ID),
    Mod = mapping(Name, Sc),
    {scalar, #{
       id => Name,
       directives => Directives,
       description => description(Desc),
       resolve_module => Mod
      }};
mk(#{ unions := Us }, #p_union { id = ID,
                                 directives = Directives,
                                 description = Desc,
                                 members = Ms }) ->
    Name = name(ID),
    Mod = mapping(Name, Us),
    Types = [handle_type(M) || M <- Ms],
    {union, #{
       id => Name,
       description => description(Desc),
       directives => Directives,
       resolve_module => Mod,
       types => Types }};
mk(#{ objects := OM },
   #p_object { id = ID,
               description = Desc,
               directives = Directives,
               fields = Fs,
               interfaces = Impls }) ->
    Name = name(ID),
    Mod = mapping(Name, OM),
    Fields = fields(Fs),
    Implements = [name(I) || I <- Impls],
    {object, #{
       id => Name,
       description => description(Desc),
       fields => Fields,
       directives => Directives,
       resolve_module => Mod,
       interfaces => Implements }};
mk(#{ enums := En },
   #p_enum {id = ID,
            directives = Directives,
            description = Desc,
            variants = Vs }) ->
    Name = name(ID),
    Variants = variants(Vs),
    Mod = mapping(Name, En),
    {enum, #{
       id => Name,
       description => description(Desc),
       directives => Directives,
       values => Variants,
       resolve_module => Mod }};
mk(_Map,
   #p_input_object { id = ID,
                     description = Desc,
                     directives = Directives,
                     defs = Ds }) ->
    Name = name(ID),
    Defs = input_defs(Ds),
    {input_object,
     #{
       id => Name,
       description => description(Desc),
       directives => Directives,
       fields => Defs }};
mk(#{ interfaces := IF },
   #p_interface { id = ID,
                  description = Description,
                  directives = Directives,
                  fields = FS }) ->
    Name = name(ID),
    Mod = mapping(Name, IF),
    Fields = fields(FS),
    {interface,
     #{
       id => Name,
       description => description(Description),
       resolve_module => Mod,
       directives => Directives,
       fields => Fields
      }}.

fields(Raw) ->
    maps:from_list([field(R) || R <- Raw]).

input_defs(Raw) ->
    maps:from_list([input_def(D) || D <- Raw]).

inject(Def) ->
    case graphql:insert_schema_definition(Def) of
        ok ->
            ok;
        {error, {already_exists, Entry}} ->
            exit({entry_already_exists_in_schema, Entry})
    end.

schema_defn(#p_object{}) -> true;
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

description(undefined) -> <<"No description provided">>;
description(D) -> D.

input_def(#p_input_value { id = ID,
                           description = Desc,
                           directives = Directives,
                           default = Default,
                           type = Type }) ->
    Name = name(ID),
    K = binary_to_atom(Name, utf8),
    V = #{ type => handle_type(Type),
           default => Default,
           directives => Directives,
           description =>description(Desc) },
    {K, V}.

field(#p_field_def{ id = ID,
                    description = Desc,
                    directives = Directives,
                    type = T,
                    args = Args }) ->
    Name = name(ID),
    %% We assume schemas are always under our control, so this is safe
    K = binary_to_atom(Name, utf8),
    V = #{ type => handle_type(T),
           description => description(Desc),
           directives => Directives,
           args => handle_args(Args)},
    {K, V}.

root_op(#p_root_operation { op_type = OpType,
                            name = Name }) ->
    case OpType of
        {query, _} -> {query, name(Name)};
        {mutation, _} -> {mutation, name(Name)};
        {subscription, _} -> {subscription, name(Name)}
    end.

handle_args(Args) ->
    maps:from_list([input_def(A) || A <- Args]).

variants(Vs) ->
    F = fun
            (#p_enum_value { id = V,
                             description = Desc,
                             directives = Directives }, I) ->
                K = binary_to_atom(V, utf8),
                {K, #{ value => I,
                       directives => Directives,
                       description => description(Desc) }}
        end,
    maps:from_list(mapi(F, Vs)).

mapi(F, L) -> mapi(F, L, 0).

mapi(_F, [], _) -> [];
mapi(F,  [X|Xs], K) -> [F(X, K) | mapi(F, Xs, K+1)].

handle_type({non_null, T}) -> {non_null, handle_type(T)};
handle_type({list, T}) -> {list, handle_type(T)};
handle_type({name, _, T}) -> binary_to_atom(T, utf8);
handle_type({scalar, Name}) ->
    #scalar_type{} = Ty = graphql_schema:get(Name),
    handle_type(Ty);
handle_type(#scalar_type{ id = Id }) -> binary_to_atom(Id, utf8).

mapping(Name, Map) ->
    case maps:get(Name, Map, undefined) of
        undefined ->
            maps:get(default, Map);
        X -> X
    end.
