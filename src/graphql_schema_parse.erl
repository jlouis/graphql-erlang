-module(graphql_schema_parse).

-include("graphql_internal.hrl").

-export([inject/2]).
inject(Mapping, {ok, {document, Entries}}) ->
    {SchemaEntries, Other} = lists:partition(fun schema_defn/1, Entries),
    report_other_entries(Other),
    [inject_e(Mapping, E) || E <- SchemaEntries],
    ok.

inject_e(Mapping, E) ->
    ok.

schema_defn(#p_field_def{}) -> true;
schema_defn(#p_type{}) -> true;
schema_defn(#p_input_object{}) -> true;
schema_defn(#p_interface{}) -> true;
schema_defn(#p_union{}) -> true;
schema_defn(#p_scalar{}) -> true;
schema_defn(#p_enum{}) -> true;
schema_defn(_) -> false.

report_other_entries([]) ->
    ok;
report_other_entries(Es) ->
    lager:warning("Loading graphql schema from file, but it contains non-schema entries: ~p", [Es]),
    ok.


