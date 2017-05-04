# A GraphQL Server library - in Erlang

This project contains the necessary support code to implement GraphQL
servers in Erlang. Its major use is on top of some other existing
transport library, for instance the cowboy web server. When a request
arrives, it can be processed by the GraphQL support library and a
GraphQL answer can be given. In a way, this replaces all of your REST
endpoints with a single endpoint: one for Graph Queries.

This README provides the system overview and its mode of operation.

## What is GraphQL

GraphQL is a query language for the web. It allows a client to tell
the server what it wants in a declarative way. The server then
materializes a response based on the clients query. This makes your
development client-centric and client-driven, which tend to be a lot
faster from a development perspective. A project is usually driven
from the top-of-the-iceberg and down, so shuffling more onus on the
client side is a wise move in modern system design.

GraphQL is also a *contract*. Queries and responses are *typed* and
contract-verified on both the input and output side. That is, GraphQL
also acts as a contract-checker. This ensures:

* No client can provide illegal queries to the server backend. These
  are filtered out by the GraphQL layer.
* No server can provide illegal respones to the client. These are
  altered such that the client gets a valid response according to the
  schema by replacing failing nodes with null-values.
* The contract documents the API
* The contract describes how the client can query data. This is the
  closest to HATEOAS we will probably get without going there.

Finally, GraphQL supports *introspection* of its endpoint. This allows
systems to query the server in order to learn what the schema is. In
turn, tooling can be built on top of GraphQL servers to provide
development-debug user interfaces. Also, languages with static types
can use the introspection to derive a type model in code which matches
the contract. Either by static code generation, or by type providers.

## Whirlwind tour

The GraphQL world specifies a typed *schema* definition. For instance
the following taken from the Relay Modern specification:

	interface Node {
	  id: ID!
	}

	type Faction : Node {
	  id: ID!
	  name: String
	  ships: ShipConnection
	}

	type Ship : Node {
	  id: ID!
	  name: String
	}

	type ShipConnection {
	  edges: [ShipEdge]
	  pageInfo: PageInfo!
	}

	type ShipEdge {
	  cursor: String!
	  node: Ship
	}

	type PageInfo {
	  hasNextPage: Boolean!
	  hasPreviousPage: Boolean!
	  startCursor: String
	  endCursor: String
	}

	type Query {
	  rebels: Faction
	  empire: Faction
	  node(id: ID!): Node
	}

	input IntroduceShipInput {
	  factionId: String!
	  shipNamed: String!
	  clientMutationId: String!
	}

	type IntroduceShipPayload {
	  faction: Faction
	  ship: Ship
	  clientMutationId: String!
	}

	type Mutation {
	  introduceShip(input: IntroduceShipInput!): IntroduceShipPayload
	}

The schema is a subset of the Star Wars schema given as the typical
GraphQL example all over the web. The GraphQL world roughly splits the
world into *input objects* and *output objects*. Input objects are
given as part of a query request by the client. Output objects are
sent back from the server to the client.

This Erlang implementation contains a schema parser for schemas like
the above. Once parsed, a mapping is provided by the programmer which
maps an output type in the schema to an Erlang module. This module
must implement a function

    -spec execute(Context, Object, Field, Args) ->
        {ok, Response}
      | {error, Reason}.

which is used to materialize said object. That is, when you request a
*field* `F` in the object `O`, a call is made to
`execute(Context, O, F, Args)`. The value `Context` provides a global
context for the query. It is used for authentication data, for origin
IP addresses and so on. The context is extensible by the developer
with any field they need. The `Args` provides arguments for the field.
Look, for instance at the type `Mutation` and the `introduceShip`
field, which takes an argument `input` of type `IntroduceShipInput!`.

Materialization is thus simply a function call in the Erlang world.
These calls tend to be used in two ways: Either they *acquire* a piece
of data from a database (e.g., mnesia) and return that data as an
`Object`. Or they materialize fields on an already loaded object. When
execution of a query is processed, you can imagine having a "cursor"
which is being moved around in the result set and is used to
materialize each part of the query.

For example, look at the following query:

    query Q {
      node(id: "12098141") {
         ... on Ship {
           id
           name
         }
      }
    }

When this query executes, it will start by a developer provided
initial object. Typically the empty map `#{}`. Since the `node` field
is requested, a call is performed to match:

    -module(query).

    ...
    execute(Ctx, #{}, <<"node">>, #{ <<"id">> := ID }) ->
       {ok, Obj} = load_object(ID).

Now, since you are requesting the `id` and `name` fields on a `Ship`
inside the node, the system will make a callback to a type-resolver
for the `Obj` in order to determine what type it is. We omit that part
here, but if it was something else, a faction say, then the rest of
the query would not trigger. Once we know that id "12098141" is a
Ship, we "move the cursor" to a ship and calls the execute function
there:

    -module(ship).

    -record(ship, { id, name }).

    execute(Ctx, #ship{ id = Id }, <<"id">>, _Args) ->
        {ok, ID};
    execute(Ctx, #ship{ name = Name }, <<"name">>, _Args) ->
        {ok, Name}.

Two materialization calls will be made. One for the field `<<"id">>`
and one for the field `<<"name">>`. The end result is then
materialized as a response to the caller.

## Architecture

Most other GraphQL servers provide no type->module mapping. Rather,
they rely on binding of individual functions to fields. The
implementation began with the same setup, but it turns out pattern
matching is a good fit for the notion of requesting different fields
inside an object. Thus, we use pattern matching as a destructuring
mechanism for incoming queries.

### Schema

Internally, the system parses the schema into an ETS table, on which
it can perform queries in parallel to satisfy multiple requests at the
same time.

A schema *injector* allows the developer to parse a schema from a file
or from memory, then bind exeuction modules to the schemas types. Once
finishes, the schema is *finalized* which runs a lint check over the
schema and rejects schemas which are nonsensical.

### Query

A query is treated as a compiler chain:

* A *lexer* tokenizes the query
* A *parser* constructs an AST of the query from the token stream
* An *elaborator* walks the AST and attaches type information to the
  AST by looking up data in the schema ETS table. This vastly
  simplifies later phases as the necessary information is often
  directly available in a pattern match. The elaborator also performs
  an early exit for obviously malformed queries.
* A *type checker* validates the query from a type perspective.
* A *validator* performs additional linting. Many queries are
  type-correct and thus *executable*, but are still malformed because
  they have nonsensical parts in them. The validator phase rejects
  such queries.
* An *executor* runs the query.

Of these tasks, only the execution phase in the end is
performance-critical. Clients can pre-load query documents to the
server, which means the document acts as a stored procedure on the
server side. The server can then do parsing, elaboration, type
checking and validation once and for all at load time. In addition it
provides a security measure: clients in production can only call a
pre-validated set of queries if such desired.
