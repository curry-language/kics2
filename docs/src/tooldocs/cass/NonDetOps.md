Analysis of non-deterministic operations
----------------------------------------

This analysis infers types of operations which indicate
their (non-)determinism behavior when they are applied.
For this purpose, an expression is considered as _deterministic_
if it will be evaluated in a deterministic manner, i.e.,
it does not refer (directly or indirectly) to the choice
operation `?` defined in the prelude, free variables,
or operations defined by rules with overlapping left-hand sides.

The analysis returns for each unary operation one of the following types:

* `D -> D`: if the operation is applied to a deterministic expression,
  it is deterministic, otherwise the result might be non-deterministic

* `ND -> D`: the operation is deterministic independent of the kind of
  argument

* `D -> ND`: the operation is non-deterministic also when it its applied
  to some deterministic argument

Furthermore, operations without arguments can have the types `D` or `ND`.
Operations with more than one argument have corresponding types.

For instance, here are some types of operations defined in the prelude:

    ?       : D -> D -> ND
    const   : D -> ND -> D
    id      : D -> D
    reverse : D -> D
    unknown : (D -> D) -> ND

Note that these are types for the operations translated to FlatCurry.
For instance, the first argument of `unknown` is the abstraction
of the `Data` dictionary.

The analysis also considers the effect of operations to encapsulate
non-deterministic search. For instance, here are some types
of operations defined in module `Control.AllValues` of package `allvalues`:

    allValues : ND -> D
    oneValue  : ND -> D
    someValue : ND -> D
