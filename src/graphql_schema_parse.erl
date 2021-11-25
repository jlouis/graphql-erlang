-module(graphql_schema_parse).

-include("graphql_internal.hrl").
-include("graphql_schema.hrl").

-export([inject/3]).

inject(Ep, BaseMapping, {ok, #document { definitions = Entries}}) ->
    Mapping = handle_mapping(BaseMapping),
    {SchemaEntries, Other} = lists:partition(fun schema_defn/1, Entries),
    report_other_entries(Other),
    Defs = [mk(Ep, Mapping, E) || E <- SchemaEntries],
    case lists:keytake(schema, 1, Defs) of
        false ->
            [inject(Ep, Def) || Def <- Defs];
        {value, Root, Rest} ->
            %% Guard against multiple schema defs
            false = lists:keytake(schema, 1, Rest),
            [inject(Ep, R) || R <- Rest],
            inject_root(Ep, Root)
    end,
    ok.

mk(_Ep, #{}, #p_schema_definition { directives = Directives,
                                       defs = RootOps }) ->
    {schema,
     #{ defs => maps:from_list([root_op(R) || R <- RootOps]),
        directives => Directives }};
mk(_Ep,
   #{ scalars := Sc },
   #p_scalar { description = Desc,
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
mk(Ep,
   #{ unions := Us }, 
   #p_union { id = ID,
              directives = Directives,
              description = Desc,
              members = Ms }) ->
    Name = name(ID),
    Mod = mapping(Name, Us),
    Types = [handle_type(Ep, M) || M <- Ms],
    {union, #{
       id => Name,
       description => description(Desc),
       directives => Directives,
       resolve_module => Mod,
       types => Types }};
mk(Ep,
   #{ objects := OM },
   #p_object { id = ID,
               description = Desc,
               directives = Directives,
               fields = Fs,
               interfaces = Impls }) ->
    Name = name(ID),
    Mod = mapping(Name, OM),
    Fields = fields(Ep, Fs),
    Implements = [name(I) || I <- Impls],
    {object, #{
       id => Name,
       description => description(Desc),
       fields => Fields,
       directives => Directives,
       resolve_module => Mod,
       interfaces => Implements }};
mk(_Ep,
   #{ enums := En },
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
mk(Ep,
   _Map,
   #p_input_object { id = ID,
                     description = Desc,
                     directives = Directives,
                     defs = Ds }) ->
    Name = name(ID),
    Defs = input_defs(Ep, Ds),
    {input_object,
     #{
       id => Name,
       description => description(Desc),
       directives => Directives,
       fields => Defs }};
mk(Ep, 
   #{ interfaces := IF },
   #p_interface { id = ID,
                  description = Description,
                  directives = Directives,
                  fields = FS }) ->
    Name = name(ID),
    Mod = mapping(Name, IF),
    Fields = fields(Ep, FS),
    {interface,
     #{
       id => Name,
       description => description(Description),
       resolve_module => Mod,
       directives => Directives,
       fields => Fields
      }};
mk(Ep,
   #{},
    #p_directive{ id = ID,
                  description = Description,
                  args = Args,
                  locations = Locations }) ->
    Name = name(ID),
    {directive,
        #{
            id => Name,
            description => description(Description),
            args => handle_args(Ep, Args),
            locations => Locations
        }}.


fields(Ep, Raw) ->
    maps:from_list([field(Ep, R) || R <- Raw]).

input_defs(Ep, Raw) ->
    maps:from_list([input_def(Ep, D) || D <- Raw]).

inject_root(Ep, Root) ->
    graphql:insert_root(Ep, Root).

inject(Ep, Def) ->
    case graphql:insert_schema_definition(Ep, Def) of
        ok ->
            ok;
        {error, {already_exists, Entry}} ->
            exit({entry_already_exists_in_schema, Entry})
    end.

schema_defn(#p_schema_definition{}) -> true;
schema_defn(#p_object{}) -> true;
schema_defn(#p_input_object{}) -> true;
schema_defn(#p_interface{}) -> true;
schema_defn(#p_union{}) -> true;
schema_defn(#p_scalar{}) -> true;
schema_defn(#p_enum{}) -> true;
schema_defn(#p_directive{}) -> true;
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

input_def(Ep, #p_input_value { id = ID,
                           description = Desc,
                           directives = Directives,
                           default = Default,
                           type = Type }) ->
    Name = name(ID),
    K = binary_to_atom(Name, utf8),
    V = #{ type => handle_type(Ep, Type),
           default => Default,
           directives => Directives,
           description =>description(Desc) },
    {K, V}.

field(Ep, #p_field_def{ id = ID,
                    description = Desc,
                    directives = Directives,
                    type = T,
                    args = Args }) ->
    Name = name(ID),
    %% We assume schemas are always under our control, so this is safe
    K = binary_to_atom(Name, utf8),
    V = #{ type => handle_type(Ep, T),
           description => description(Desc),
           directives => Directives,
           args => handle_args(Ep, Args)},
    {K, V}.

root_op(#p_root_operation { op_type = OpType,
                            type = Name }) ->
    case OpType of
        {query, _} -> {query, name(Name)};
        {mutation, _} -> {mutation, name(Name)};
        {subscription, _} -> {subscription, name(Name)}
    end.

handle_args(Ep, Args) ->
    maps:from_list([input_def(Ep, A) || A <- Args]).

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

handle_type(Ep, {non_null, T}) -> {non_null, handle_type(Ep, T)};
handle_type(Ep, {list, T}) -> {list, handle_type(Ep, T)};
handle_type(_Ep, {name, _, T}) -> binary_to_atom(T, utf8);
handle_type(Ep, {scalar, Name}) ->
    #scalar_type{} = Ty = graphql_schema:get(Ep, Name),
    handle_type(Ep, Ty);
handle_type(_Ep, #scalar_type{ id = Id }) -> binary_to_atom(Id, utf8).

mapping(Name, Map) ->
    case maps:get(Name, Map, undefined) of
        undefined ->
            maps:get(default, Map);
        X -> X
    end.

