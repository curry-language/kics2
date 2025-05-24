Analysis of deterministic operations
------------------------------------

This analysis checks whether an operation is deterministically defined.
Intuitively, an operation is deterministic if the evaluation of
this operation applied to ground terms does not cause any non-determinism.
The determinism analysis returns `nondeterministic` for a given operation
if its definition contains overlapping left-hand sides or free variables,
or if it depends on some non-deterministic operation.

If calls to non-deterministic operations are encapsulated (by the
use of set functions or operations from the modules
`Control.Search.AllValues` or `Control.Search.Unsafe`,
then it is classified as deterministic since the non-determinism
does not occur at the top-level.
