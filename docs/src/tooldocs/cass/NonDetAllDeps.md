Analysis of dependencies on all non-deterministic operations
------------------------------------------------------------

This analysis is useful if some operation has a non-deterministic
behavior and one wants to find the reason for this behavior.

For this purpose, the analysis computes for each operation the set of
operations with a non-deterministic definition that might be called
by this operation. An operation has a non-deterministic definition
if its definition contains overlapping left-hand sides or free variables.
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
and also on the non-deterministic operations `last` and `coin`.

In the long analysis output (produced by CASS in batch mode),
the non-deterministic operations are shown together with
the sequence of operations (limited to a length of 10)
which calls the non-deterministic operation.
