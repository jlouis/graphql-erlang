[![Build Status](https://travis-ci.org/shopgun/graphql-erlang.svg?branch=develop)](https://travis-ci.org/shopgun/graphql-erlang)

# A GraphQL Server library - in Erlang

This project contains the necessary support code to implement GraphQL
servers in Erlang. Its major use is on top of some other existing
transport library, for instance the cowboy web server. When a request
arrives, it can be processed by the GraphQL support library and a
GraphQL answer can be given. In a way, this replaces all of your REST
endpoints with a single endpoint: one for Graph Queries.

This README provides the system overview and its mode of operation.

# Changelog

See the file `CHANGELOG.md` in the root of the project.

# Status

Currently, the code implements all of the October 2016 GraphQL
specification, except for a few areas:

* Some validators are missing and pending implementation. The
  important validators are present, however. Missing stuff are all
  tracked as issues in this repository.
* Parametrization inside fragments are not yet implemented fully.

In addition, we are working towards June 2018 compliance. We already
implemented many of the changes in the system. But we are still
missing some parts. The implementation plan is on a demand driven
basis for Shopgun currently, in that we tend to implement things when
there is a need for them.

# Documentation

This is a big library. In order to ease development, we have provided
a complete tutorial for GraphQL Erlang:

https://github.com/shopgun/graphql-erlang-tutorial

Also, the tutorial has a book which describes how the tutorial example
is implemented in detail:

https://shopgun.github.io/graphql-erlang-tutorial/

*NOTE:* Read the tutorial before reading on in this repository if you
haven't already. This README gives a very quick overview, but the
canonical documentation is the book at the moment.


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
* No server can provide illegal responses to the client. These are
  altered such that the client gets a valid response according to the
  schema by replacing failing nodes with null-values.
* The contract documents the API
* The contract describes how the client can query data. This is the
  closest to HATEOAS we will probably get without going there.
* Queries tend to be large and all-encompassing. This means we don't
  pay the round-trip-time for a request/response like you do in e.g.,
  HTTP and HTTP/2 based systems where multiple queries are executed
  back to back and depends on each other. Almost every query can be
  handled in a single round trip.

Finally, GraphQL supports *introspection* of its endpoint. This allows
systems to query the server in order to learn what the schema is. In
turn, tooling can be built on top of GraphQL servers to provide
development-debug user interfaces. Also, languages with static types
can use the introspection to derive a type model in code which matches
the contract. Either by static code generation, or by type providers.

## Whirlwind tour

The GraphQL world specifies a typed *schema* definition. For instance
the following taken from the Relay Modern specification:

```graphql
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
```

The schema is a subset of the Star Wars schema given as the typical
GraphQL example all over the web. The GraphQL world roughly splits the
world into *input objects* and *output objects*. Input objects are
given as part of a query request by the client. Output objects are
sent back from the server to the client.

This Erlang implementation contains a schema parser for schemas like
the above. Once parsed, a mapping is provided by the programmer which
maps an output type in the schema to an Erlang module. This module
must implement a function

```erlang
-spec execute(Context, Object, Field, Args) ->
    {ok, Response}
  | {error, Reason}.
```

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

```graphql
query Q {
  node(id: "12098141") {
      ... on Ship {
        id
        name
      }
  }
}
```

When this query executes, it will start by a developer provided
initial object. Typically the empty map `#{}`. Since the `node` field
is requested, a call is performed to match:

```erlang
-module(query).

...
execute(Ctx, #{}, <<"node">>, #{ <<"id">> := ID }) ->
    {ok, Obj} = load_object(ID).
```

Now, since you are requesting the `id` and `name` fields on a `Ship`
inside the node, the system will make a callback to a type-resolver
for the `Obj` in order to determine what type it is. We omit that part
here, but if it was something else, a faction say, then the rest of
the query would not trigger. Once we know that id "12098141" is a
Ship, we "move the cursor" to a ship and calls the execute function
there:

```erlang
-module(ship).

-record(ship, { id, name }).

execute(Ctx, #ship{ id = Id }, <<"id">>, _Args) ->
    {ok, ID};
execute(Ctx, #ship{ name = Name }, <<"name">>, _Args) ->
    {ok, Name}.
```

Two materialization calls will be made. One for the field `<<"id">>`
and one for the field `<<"name">>`. The end result is then
materialized as a response to the caller.

### Materilization through derivation

A common use of the functions is to *derive* data from existing data.
Suppose we extend the ship in the following way:

```graphql
type Ship {
  ...
  capacity : float!
  load : float!
  loadRatio : float!
}
```

so a ship has a certain capacity and a current load in its cargo bay.
We could store the `loadRatio` in the database and keep it up to date.
But a more efficient way to handle this is to compute it from other
data:

```erlang
-module(ship).

-record(ship,
    { id,
      name,
      capacity,
      load }).

execute(...) ->
  ...;
execute(Ctx, #ship {
                capacity = Cap,
                load = Load }, <<"loadRatio">>, _) ->
    {ok, Load / Cap };
...
```

This will compute that field if it is requested, but not compute it
when it is not requested by a client. Many fields in a data set are
derivable in this fashion. Especially when a schema changes and grows
over time. Old fields can be derived for backwards compatibility and
new fields can be added next to it.

In addition, it tends to be more efficient. A sizable portion of
modern web work is about moving data around. If you have to move less
data, you decrease the memory and network pressure, which can
translate to faster service.

### Materializing JOINs

If we take a look at the `Faction` type, we see the following:

```graphql
type Faction : Node {
  id: ID!
  name: String
  ships: ShipConnection
}
```

in this, `ships` is a field referring to a `ShipConnection`. A
Connection type is Relay Modern standard of how to handle a
paginated set of objects in GraphQL. Like "Materialization by
derivation" we would derive this field by looking up the data in the
database for the join and then producing an object which the
`ship_connection_resource` can handle. For instance:

```erlang
execute(Ctx, #faction { id = ID }, <<"ships">>, _Args) ->
    {ok, Ships} = ship:lookup_by_faction(ID),
    pagination:build_pagination(Ships).
```

where the `build_pagination` function returns some object which is a
generic connection object. It will probably look something along the
lines of

```erlang
#{
  '$type' => <<"ShipConnection">>,
  <<"pageInfo">> => #{
      <<"hasNextPage">> => false,
      ...
  },
  <<"edges">> => [
      #{ <<"cursor">> => base64:encode(<<"edge:1">>),
          <<"node">> => #ship{ ... } },
      ...]
}
```

which can then be processed further by other resources. Note how we
are eagerly constructing several objects at once and then exploiting the
cursor moves of the GraphQL system to materialize the fields which the
client requests. The alternative is to lazily construct
materializations on demand, but when data is readily available anyway,
it is often more efficient to just pass pointers along.

## API

The GraphQL API is defined in the module `graphql`. Every
functionality is exported in that module. Do not call inside other
modules as their functionality can change at any point in time even
between major releases.

The system deliberately splits each phase and hands it over to the
programmer. This allows you to debug a bit easier and gives the
programmer more control over the parts. A typical implementation will
start by using the schema loader:

```erlang
inject() ->
  {ok, File} = application:get_env(myapp, schema_file),
  Priv = code:priv_dir(myapp),
  FName = filename:join([Priv, File]),
  {ok, SchemaData} = file:read_file(FName),
  Map = #{
    scalars => #{ default => scalar_resource },
    interfaces => #{ default => resolve_resource },
    unions => #{ default => resolve_resource },
    objects => #{
      'Ship' => ship_resource,
      'Faction' => faction_resource,
      ...
      'Query' => query_resource,
      'Mutation' => mutation_resource,
      'Subscription' => subscription_resource
    }
  },
  ok = graphql:load_schema(Map, SchemaData),
      Root = {root,
      #{
        query => 'Query',
        mutation => 'Mutation',
        subscription => 'Subscription',
        interfaces => []
      }},
  ok = graphql:insert_schema_definition(Root),
  ok = graphql:validate_schema(),
  ok.
```

This will set up the schema in the code by reading it from a file on
disk. Each of the `_resource` names refers to modules which implements
the backend code.

In order to execute queries on the schema, code such as the following
can be used. We have a query document in `Doc` and we have a requested
operation name in `OpName` and parameter variables for the given op in
`Vars`. The variables `Req` and `State` are standard cowboy request
and state tracking variables from `cowboy_rest`.

```erlang
run(Doc, OpName, Vars, Req, State) ->
  case graphql:parse(Doc) of
    {ok, AST} ->
      try
          {ok, #{fun_env := FunEnv,
                ast := AST2 }} = graphql:type_check(AST),
          ok = graphql:validate(AST2),
          Coerced = graphql:type_check_params(FunEnv, OpName, Vars),
          Ctx = #{ params => Coerced, operation_name => OpName },
          case graphql:execute(Ctx, AST2) of
              #{subscription := {Subscription, SubscriptionContext}} ->
                  {cowboy_loop, Req, store_subscription(Subscription, SubscriptionContext, State)};
              Response ->
                  Req2 = cowboy_req:set_resp_body(encode_json(Response), Req),
                  {ok, Reply} = cowboy_req:reply(200, Req2),
                  {halt, Reply, State}
          end
      catch
            throw:Err ->
                err(400, Err, Req, State)
      end;
    {error, Error} ->
        err(400, {parser_error, Error}, Req, State)
  end.
```

## Conventions

In this GraphQL implementation, the default value for keys are type
`binary()`. This choice is deliberate, since it makes the code more
resistent to `atom()` overflow and also avoids some conversions
between `binary()` and `atom()` values in the system. A later version
of the library might redesign this aspect, but we are somewhat stuck
with it for now.

However, there are many places where you can input atom values and
then have them converted internally by the library into binary values.
This greatly simplifies a large number of data entry tasks for the
programmer. The general rules are:

* If you supply a value to the system and it is an atom(), the
  internal representation is a binary value.
* If the system hands you a value, it is a binary() value and not an
  atom().

## Middlewares

This GraphQL system does not support middlewares, because it turns out
the systems design is flexible enough middlewares can be implemented
by developers themselves. The observation is that any query runs
through the `Query` type and thus a `query_resource`. Likewise, any
`Mutation` factors through the `mutation_resource`.

As a result, you can implement middlewares by using the `execute/4`
function as a wrapper. For instance you could define a mutation
function as:

```erlang
execute(Ctx, Obj, Field, Args) ->
    AnnotCtx = perform_authentication(Ctx),
    execute_field(AnnotCtx, Obj, Field, Args).
```

The reason this works so well is because we are able to use pattern
matching on `execute/4` functions and then specialize them. If we had
an individual function for each field, then we would have been forced
to implement middlewares in the system, which incurs more code lines
to support.

More complex systems will define a stack of middlewares in the list
and run them one by one. As an example, a `clientMutationId` is part
of the Relay Modern specification and must be present in every
mutation. You can build your `mutation_resource` such that it runs a
`maps:take/2` on the argument input, runs the underlying mutation, and
then adds back the `clientMutationId` afterwards.

## Schema Definitions

This GraphQL implementation follows the Jun2018 specification for
defining a schema. In this format, one writes the schema according to
specification, including doc-strings. What was represented as `tags`
in an earlier implementation of GraphQL for Erlang is now represented
as a `@directive` annotation, as per the specification.

As an example, you can write something along the lines of:

```graphql
"""
A Ship from the Star Wars universe
"""
type Ship : Node {
  "Unique identity of the ship"
  id: ID!

  "The name of the ship"
  name: String
}
```

And the schema parser knows how to transform this into documentation
for introspection.

## Resource modules

The following section documents the layout of resource modules as they
are used in GraphQL, and what they are needed for in the
implementation.

### Scalar Resources

GraphQL contains two major kinds of data: objects and scalars. Objects
are product types where each element in the product is a field. Raw
data are represented as *scalar* values. GraphQL defines a number of
standard scalar values: boolean, integers, floating point numbers,
enumerations, strings, identifiers and so on. But you can extend the
set of scalars yourself. The spec will contain something along the
lines of

```graphql
scalar Color
scalar DateTime
```

and so on. These are mapped onto resource modules handling scalars. It
is often enough to provide a default scalar module in the mapping and
then implement two functions to handle the scalars:

```erlang
-module(scalar_resource).

-export(
  [input/2,
    output/2]).

-spec input(Type, Value) -> {ok, Coerced} | {error, Reason}
  when
    Type :: binary(),
    Value :: binary(),
    Coerced :: any(),
    Reason :: term().
input(<<"Color">>, C) -> color:coerce(C);
input(<<"DateTime">>, DT) -> datetime:coerce(DT);
input(Ty, V) ->
    error_logger:info_report({coercing_generic_scalar, Ty, V}),
    {ok, V}.

-spec output(Type, Value) -> {ok, Coerced} | {error, Reason}
  when
    Type :: binary(),
    Value :: binary(),
    Coerced :: any(),
    Reason :: term().
output(<<"Color">>, C) -> color:as_binary(C);
output(<<"DateTime">>, DT) -> datetime:as_binary(DT);
output(Ty, V) ->
    error_logger:info_report({output_generic_scalar, Ty, V}),
    {ok, V}.
```

Scalar Mappings allow you to have an internal and external
representation of values. You could for instance read a color such as
`#aabbcc`, convert it into `#{ r => 0.66, g => 0.73, b => 0.8 }`
internally and back again when outputting it. Likewise a datetime
object can be converted to a UNIX timestamp and a timezone internally
if you want. You can also handle multiple different ways of
coercing input data, or have multiple internal data representations.

### Type resolution Resources

For GraphQL to function correctly, we must be able to resolve types of
concrete objects. This is because the GraphQL system allows you to
specify abstract interfaces and unions. An example from the above
schema is the `Node` interface which is implemented by `Ship` and
`Faction` among other things. If we are trying to materialize a node,
the GraphQL must have a way to figure out the type of the object it is
materializing. This is handled by the type resolution mapping:

```erlang
-module(resolve_resource).

-export([execute/1]).

%% The following is probably included from a header file in a real
%% implementation
-record(ship, {id, name}).
-record(faction, {id, name}).

execute(#ship{}) -> {ok, <<"Ship">>};
execute(#faction{}) -> {ok, <<"Faction">>};
execute(Obj) ->
    {error, unknown_type}.
```


### Output object Resources

Each (output) object is mapped onto an Erlang module responsible for
handling field requests in that object. The module looks like:

```erlang
-module(object_resource).

-export([execute/4]).

execute(Ctx, SrcObj, <<"f">>, Args) ->
    {ok, 42};
execute(Ctx, SrcObj, <<"heavy">>, Args) ->
    Token = graphql:token(Ctxt),
    Worker = spawn_link(fun() ->
      Result = calculate_heavy_value(Args),
      graphql:reply_cast(Token, {ok, Result})
    end),
    {defer, Token, #{timeout => 5000, worker => Worker}};
execute(Ctx, SrcObj, Field, Args) ->
    default...
```

The only function which is needed is the `execute/4` function which is
called by the system whenever a field is requested in that object. The
4 parameters are as follows:

* `Ctx` - The *context* of the query. It contains information
  pertaining to the current position in the Graph, as well as
  user-supplied information from the start of the request. It is
  commonly used as a read-only store for authentication/authorization
  data, so you can limit what certain users can see.
* `SrcObj` - The *current* object on which we are operating. Imagine
  we have two ships, a B-wing and an X-wing. Even if we request the
  same fields on the two ships, the `SrcObj` is going to be different.
  GraphQL often proceeds by having certain fields *fetch* objects out
  of a backing store and then moving the *cursor* onto that object and
  calling the correct object resource for that type. The `SrcObj` is
  set to point to the object that is currently being operated upon.
  For `query` and `mutation` the **root** field `SrcObj` is set to `none`, for
  `subscription` it is set to `{subscription_value(), Event}`.
* `Field` - The field in the object which is requested.
* `Args` - A map of field arguments. See the next section.

Possible return values of `execute/4` callback are:

* `{ok, Value}` - the value of the field (either scalar for scalar fields or arbitrarily
  complex object for object fields); `Value` can be `null` for nullable fields
* `{ok, [{ok, Value}, ..]}` for array fields
* `{ok, Val, [Auxilary, ..]}` all the `Auxilary` lists returned during operation execution will be
  concatenated and returned as a `.aux` field in the result map.
* `{error, Reason}` for execution errors (`Reason` is either `atom()` or `binary()` or record
  that will be used to construct the resulting `.errors` field)
* `{defer, Token, undefined | #{timeout => timeout(), worker => pid()}}` tells graphql execution
  engine that the value of the field is calculated asynchronously and will be delivered later to it
  via `graphql:reply_cast/2`.
  *  `timeout` - tells graphql engine to give-up waiting for the value after this many milliseconds
  *  `worker` - tells graphql engine that the value is calculated by this Erlang process (worker),
     so the engine will monitor this process and will report error if the process crashes.
* `{defer, Token}` same as `{defer, Token, undefined}`

Any value can be either returned from the function or thrown using `graphql:throw/1`.

### Subscription resolution modules

Subscription root object is mapped onto an Erlang module responsible for
initiation of the subscription and for handling field requests in received events.
The module is similar to [Output object Resources](#output-object-resources) but
they have one extra callback.

```erlang
-module(object_resource).

-export([subscribe/3, execute/4]).

subscribe(Ctx, <<"commentAdded">>, #{<<"topic">> := TopicId}) ->
    Subscription = #my_subscription{} = topic:subscribe_new_comments(TopicId),
    {subscription, Subscription}.

execute(Ctx, {#my_subscription{}, Comment}, <<"commentAdded">>, _Args) ->
    {ok, Comment}.

```

The `subscribe/3` callback is called whenever `subscription` operation is
executed. This callback should initialize the subscription and return `{subscription, Subscription}`
tuple, where `Subscription` is whatever could be necessary to properly map
the events received from pub-sub system to the correct subscription. The
`graphql:execute/2` function will then return `#{subscription := {Subscription, OpaqueCtx}}`.

Whenever the new event `Event` is received from pubs-sub system,
`graphql:handle_subscription_event(ExtraCtx, Subscription, OpaqueCtx, Event)`
must be called which will call `execute/4` callback which will act the same
way as described in [Output object Resources](#output-object-resources).

#### Field Argument rules

In GraphQL, field arguments follow a specific pattern:

* Clients has no way to input a `null` value. The only thing they can
  do is to omit a given field in the input. In particular, clients
  *must* supply a field which is non-null.
* Servers *always* see every field in the input, even if the client
  doesn't supply it. If the client does not supply a field, and it has
  no default value, the server sees a `null` value for that field.

This pattern means there is a clear way for the client to specify "no
value" and a clear way for the server to work with the case where the
client specified "no value. It eliminates corner cases where you have
to figure out what the client meant.

Resolution follows a rather simple pattern in GraphQL. If a client
omits a field and it has a default value, the default value is input.
Otherwise `null` is input. Clients *must* supply every non-null field.

_Note:_ This limitation is lifted in the Jun2018 GraphQL
specification, but this server doesn't implement that detail yet.

On the server side, we handle arguments by supplying a map of KV pairs
to the execute function. Suppose we have an input such as

```graphql
input Point {
    x = 4.0 float
    y float
}
```

The server can handle this input by matching directly:

```erlang
execute(Ctx, SrcObj, Field,
    #{ <<"x">> := XVal, <<"y">> := YVal }) ->
  ...
```

This will always match. If the client provides the input `{}` which is
the empty input, `XVal` will be `4.0` due to the default value. And
`YVal` will be `null`. If the client supplies, e.g., `{ x: 2.0, y: 7.0
}` the map `#{ <<"x">> => 2.0, <<"y">> => 7.0 }` will be provided.

#### Tips & Tricks

The execute function allows you to make object-level generic handling
of fields. If, for example, your `SrcObj` is a map, you can do generic
lookups by using the following handler:

```erlang
execute(_Ctx, Obj, Field, _Args) ->
    case maps:get(Field, Obj, not_found) of
      not_found -> {ok, null};
      Val -> {ok, Val}
    end.
```

Another trick is to use generic execution to handle "middlewares" -
See the appropriate section on Middlewares.

# System Architecture

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

A query is treated as a compiler chain, which is a design that fits
Erlang well. Compilers rely a lot on pattern matching, so we can
process a query symbolically by matching on it and gradually
transforming it into a *query plan* which can then be executed.

* A *lexer* tokenizes the query
* A *parser* constructs an AST of the query from the token stream
* An *type checker* walks the AST and attaches type information to the
  AST by looking up data in the schema ETS table. The pass also
  detects type and validation errors. The type checker is written in a
  bi-directional style so it flips between *inference*, in which we
  deduce the type of a term, and *checking* in which we verify a term
  has a given type.
* A *validator* performs additional linting. Many queries are
  type-correct and thus *executable*, but are still malformed because
  they have nonsensical parts in them. The validator phase rejects
  such queries.
* A *query plan* is formed from the AST.
* An *executor* runs the *query plan*.

Of these tasks, only the execution phase in the end is
performance-critical. Clients can pre-load query documents to the
server, which means the document acts as a stored procedure on the
server side. The server can then do parsing, elaboration, type
checking and validation once and for all at load time. In addition it
provides a security measure: clients in production can only call a
pre-validated set of queries if such desired.

# User Interface

GraphQL has some very neat Javascript tooling which plugs into the
introspection of a GraphQL server and provides additional
functionality:

* GraphiQL - Provides a query interface with autocompletion,
  documentation, debugging, ways to execute queries and so on. It is
  highly recommended you add such a system in staging and production
  as it is indispensable if you are trying to figure out a new query
  or why a given query returned a specific kind of error.

Additionally, Relay Modern provides specifications for cache
refreshing, pagination, mutation specifications and so on. It is
recommended you implement those parts in your system as it is part of
a de-facto standard for how GraphQL servers tend to operate.

# Tests

The GraphQL project has an extensive test suite. We prefer adding
regressions to the suite as we experience them. Some of the tests are
taken from the the official GraphQL repository and translated. More
work is definitely needed, but in general new functionality should be
provided together with a test case that demonstrates the new
functionality.

The general tests are:

* `dungeon_SUITE` which implements a "MUD" style dungeon backend. It
  is used as a way to handle most of the test cases we cook up
  ourselves. It is driven by a schema and uses a query document for
  its queries. If you don't know where to add a test, this is a good
  place.
* `enum_SUITE` Taken from the official Node.js de-facto
  implementation, this suite uses the "colors" schema in order to
  verify certain hard-to-get-right properties about enumerated data
  types.
* `graphql_SUITE` Main suite for things which doesn't fit elsewhere.
  Checks lexing/parsing, "Hello World" style queries and also
  introspection.
* `star_wars_SUITE` An implementation from the specification of the
  Star Wars universe. Allows us to verify queries from the
  specification in our own code. Also part of the Node.js de-facto
  implementation, so it is easy for us to transplant a test from there
  to here.
* `validation_SUITE` GraphQL contains a lot of different validation
  checks. We handle some of these in the type checker and some in a
  validation pass. The tests here are mostly verifying parts of the
  specification. It uses the "Pet" schema as a base.

