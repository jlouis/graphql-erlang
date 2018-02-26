# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/)
and this project adheres to [Semantic Versioning](http://semver.org/spec/v2.0.0.html).

Versioning generally follows semantic versioning, but breaks it for
releases less than 1.0.0 in certain situations. The changelog mentions
the compatibility issues you are likely to encounter.

## [Unreleased]

### Fix

- The `key` field in errors are now an atom so it can be JSON encoded.

## [0.12.0] 2018-02-23 Flag day release, broke compatibility backwards!

### Changed
- *IMPORTANT:* We've altered the way errors are propagated in the
  system and this is a backwards incompatible change. However, since
  we haven't reached version 1.0 yet, no guarantees are given.
  
  To update your system, you have to do the following:
  
  If the validation, elaboration or type_check step fails with a throw
  containing `{error, E}`, then you have to send that error through
  the formatter. For instance by calling `graphql:format_errors(Ctx,
  E)`. You can also supply your own error module:
  `graphql:format_errors(Ctx#{error_module => Mod}, E)`.
  
  Execution assumes the presence of `#{ error_module := Mod } = Ctx`
  in the context and uses that for formatting.
  
  To implement your own error module, implement the functions
  
  `crash(Ctx, Path, Err) -> JSON`
  `err(Ctx, Path, Err) -> JSON`
  
  The rationale: by giving API users explicit control over error
  formatting, it is far easier to embed the engine in another system.
  You can decide what to do with errors. In particular, if you have
  unique request IDs for each request, it is usually worthwhile to
  embed those into the error path itself.
  
  Crashes are something you might not want to report in detail to the
  user. In particular, they could contain sensitive information on
  error. So one can hide that information.
  
  While here, also remove most of the internal error reporting from
  the engine and punt it to the above formatter functions. The only
  thing which is reported are wrong resolver returns which is always a
  bug in the system.

### Added
- You can now annotate enum values with descriptions
- You can now use `graphql:throw(Result)` to exit early inside
  resolver functions. The intended use is to be able to provide better
  error messages in the GraphQL engine when things doesn't work as
  expected.

### Fixed
- Fix a case where you have a nested variable expansion. These went
  through to the execution and then they came out un-expanded. The fix
  correctly traverses deep into parameter checking and properly
  expands the variables as needed.
- Heed Oct2016 spec section 3.1.7 w.r.t input coercion of scalar
  values. Suppose we have a `DOG` of type `Pet`. Then, coercion of a
  value `DOG` into a context `[Pet]` is now regarded as having
  supplied `[DOG]`.
- Sometimes, errors were not leading to `null` values, but were
  propagated to the parent objects. This has now been fixed such that
  errors occurs on the innermost object that can be nullable as per
  the specification.
- Fix evaluation order of non-null checks. Suppose we have a document
  such as `query Q($v : T) { field(arg: $v) }` where `arg : T!`. In
  this case, the query is illegal since `$v` is nullable but `arg` is
  not. The type checker incorrectly fails to reject this, so the error
  occurs deep in the execution phase, which isn't good UX.

  The fix provides superior UX.

### Changed
- Format type values as binary() in error's messages
  (Minor fix)
- Introduce Validation 5.7.1
- Introduce Validation 5.5.1
- Introduce Validation 5.6.1
- Introduce Validation 5.6.2
- Introduce Validation 5.6.3
- Introduce a checker for fragment expansions. This disallows any
  fragment expansion which can never match in a given context as an
  error. Fragments which *may* be expanded are accepted.
- Introduce validation checks for fragment expansions from the
  specification.

## [0.11.1] 2017-10-26

### Changed
- Coercion of enums have been changed. Now an enum is presented to the
  coercer as `input(Type, Str)` where `Str` is a `binary()` value.
  The function default to returning `{enum, Str}` for the enumerated
  value. The change is to handle the fact that enumerated values might
  enter the graphql system in many formats, so by this change we make sure
  they enter in a canonical way. The change also allows one to use
  something such as `binary_to_existing_atom` to handle enumerations
  in the system (in a safe way to boot!).
- Major restructuring of the test suite. Split tests into their own
  suites where applicable.

### Fixed
- The errors returned when type checking fails have been changed. The
  change better reflects the type of error which occur, but it
  requires change if you rely on error messages having a specific
  form. See the test updates for `errors.unknown_variable` and
  `errors.invalid_enums` for errors which have changed. The path
  component is more precise for unknown variables and invalid enums.

  This is a buff.
- Default values was not being input coerced at all. This error
  specifically affects enumerated types.
- A document of the form `query { ... }` is now accepted, as it should
  be. The parser was missing a production.

### Refactored
- Refactor all of the elaborator phase in the system. Improve and
  simplify its flow.
- Refactor the type checker phase in the system. Improve and simplify
  its flow as well.
- Rewrite the central judgement in the type checker engine to a more
  direct flow which is easier to handle and follow.

### Removed
- The default resolver, which has never been in official documentation
  has been removed from the source code. This simplifies the execution
  engine a fair bit.
- The default handling of ?LAZY. If you need to handle lazy
  evaluation, you can now do that yourself by returning a lazy
  function in an object and then updating your resolve modules to
  handle lazy evaluation by forcing the computation when you hit your
  lazy-construction.

## [0.11.0] 2017-10-09

### Added
- Feature: Support monitoring in the experimental concurrent /
  parallel query engine.

### Fixed
- When a resolver fails, the error type is now more
  streamlined. The new format is `{error, {Type, Data}}` rather than
  `{error, {Type, Data1}}` and also `{error, {Type, Data1, Data2}}`
  and so on.

## [0.10.0] New feature update
### Added
- Feature: Support 3-tuple responses in GraphQL resolvers. Tuples of the form
  `{ok, Result, [Terms]}` are now allowed, and the given Terms will
  be collected in the final response in an unspecified order. This
  allows resolvers to send back auxilliary data through the graph
  system and handle these at the top level later on. This
  functionality is currently *experimental* until we figure out if
  there is a simpler solution to the problem of getting side-channel
  data out of the Graph and back to the execution engine.
- Feature: Support *experimental* concurrent and parallel query in
  the execution engine. This allows different parts of the query to
  execute individually which improves latency of the system as a
  whole. The feature is currently tested in a way to make sure it is
  in "symbiosis" with the rest of the system. Once it has proven to
  be stable, it will be documented.

### Changed
- Refactor the dungeon SUITE test cases so they are easier to
  maintain in the future.
- Refactor: Replace special built-in resolves with injected resolvers to make
  them closer to user-defined resolvers in their operation. This
  also simplifies code.
- Refactor: Rework the way Scalars are resolved in the system. Streamline
  different variants of scalar resolution into the same code path.
  This paves the way for lots of future code removal in the system
  and lots of simplification down the road.

### Fixed
- null-value resolution. In some cases, execution could not
  correctly resolve `null` values in the system in the right way.

## [0.9.0] Stability Update

### Added
- Build is now on Travis CI
- Erlang/OTP 19.3 and 20.0 support

### Changed
- Lager is not a dependency anymore. The library is completely
  independent of anything but `kernel` and `stdlib` now.
- `jsx` is only used for testing
- *POTENTIAL INCOMPATIBILITY:* Error messages reported the path in the
  query in the wrong order (bottom-up. The right order is top-down).
  This was fixed (@gausby)
- The `dungeon` tests now use the schema parser to test it more.
- *POTENTIAL INCOMPATIBILITY:* The error reporting system has been
  streamlined and now properly tracks errors in the same way all
  over the system. This paves the way for even better error handling
  in the future, but clients who already rely on the erroneous
  behavior needs to change. Hopefully, the new structure is more
  consistent, so the work of handling errors should be simpler in
  the client.
- *POTENTIAL INCOMPATIBILITY:* If the backend provides an *Int* in
  a *Float* context, then the integer is automatically converted
  into a float

### Fixed
- Fix: Correctly handle the case where a query document contains one
  operation. In this case, we can implicitly deduce the client wants
  to call the (single) operation. This brings us into convergence
  with the spec.
- Enumerated types are now supported in the same way as Scalar types
  (work by @CallumRoberts). The feature has not been extensively
  documented yet since it was subject to change, but if your system
  relies on Enumerated types, the code changed around it. The
  feature was somewhat "experimental" since it lacked documentation.
  It still needs an example of its use in the tutorial.

## [0.8.0] Initial Release

First Open Source Release. The version is deliberately set a bit
before 1.0 in order to be able to do some changes to the API before
releasing a more official version with full backwards compatibility
ensured.

