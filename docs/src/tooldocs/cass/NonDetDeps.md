Analysis of dependencies on non-deterministic operations
--------------------------------------------------------

This analysis is useful if some operation has a non-deterministic
behavior and one wants to find the reason for this behavior.

For this purpose, the analysis computes for each operation the set of
operations with a non-deterministic definition that might be called
by this operation. An operation has a non-deterministic definition
if its definition contains overlapping left-hand sides or free variables.
Non-deterministic operations that are called by other
non-deterministic operations are ignored so that only the first
(w.r.t. the call sequence) non-deterministic operations are returned.
If the non-determinism of an operation is encapsulated by
a set function or an encapsulated search operation of the modules
`Control.Search.AllValues` or `Control.Search.Unsafe`,
it is considered as deterministic.

For instance, consider the operations

    last xs | _ ++ [x] == xs = x where x free
    
    coin = 0 ? 1
    
    lastCoin = id (last [coin])
    
Then the operation `lastCoin` depends on the non-deterministic
operations `last` and `coin`. Now consider the operations

    f x = x ? lastCoin

    g x = f x

Then the operation `g` depends on the non-deterministic operation `f`,
but the dependency on the non-deterministic
operations `last` and `coin` is not reported.

In the long analysis output (produced by CASS in batch mode),
the non-deterministic operations are shown together with
the operation which directly calls the non-deterministic operation.
