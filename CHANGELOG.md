# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/)
and this project adheres to [Semantic Versioning](http://semver.org/spec/v2.0.0.html).

Versioning generally follows semantic versioning, but breaks it for
releases less than 1.0.0 in certain situations. The changelog mentions
the compatibility issues you are likely to encounter.

## [Unreleased]

Nothing yet

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

