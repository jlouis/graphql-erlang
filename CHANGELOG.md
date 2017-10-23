# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/)
and this project adheres to [Semantic Versioning](http://semver.org/spec/v2.0.0.html).

Versioning generally follows semantic versioning, but breaks it for
releases less than 1.0.0 in certain situations. The changelog mentions
the compatibility issues you are likely to encounter.

## [Unreleased]

### Changed
- Coercion of enums have been changed. Now an enum is presented to the
  coercer as `input(Type, Str)` where `Str` is a `binary()` value.
  The function default to returning `{enum, Str}` for the enumerated
  value. The change is to handle the fact that enumerated values might
  enter the coercer in many formats, so by this change we make sure
  they enter in a valid way. The change also allows one to use
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

