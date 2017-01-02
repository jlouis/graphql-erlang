-module(relay_helper).

-export([
	input/3
]).

-spec input(Name, InputFields, PayloadFields) -> ok
    when
        Name :: atom(),
        InputFields :: #{ atom() => any() },
        PayloadFields :: #{ atom() => any() }.

input(Ty, InputFields, PayloadFields) ->
    TyBin = atom_to_binary(Ty, utf8),

    Input =
        {input_object, #{
            id => <<TyBin/binary, "Input">>,
            description => ["Input object for ", TyBin],
            fields => maps:merge(InputFields,
                #{ clientMutationId => #{
                        type => 'string',
                        description => "Mutation ID for the client, if any" }})
        }},
    ok = graphql:insert_schema_definition(Input),

    Payload =
        {object, #{
            id => <<TyBin/binary, "Payload">>,
            description => ["Payload for ", TyBin],
            fields => maps:merge(PayloadFields,
                #{
                    clientMutationId => #{
                        type => string,
                        description => "Mutation ID for the client, if any" },
                  errors => #{
                      type => ['Error'],
                      description => "Errors from the mutation" }
                })
        }},
    ok = graphql:insert_schema_definition(Payload),
    ok.        