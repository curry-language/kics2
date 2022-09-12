Analysis of functionally defined operations
-------------------------------------------

This analysis checks whether an operation is defined in a purely
functional manner.

An operation is functionally defined if its definition does not contain
overlapping left-hand sides or free variables, and it depends only
on functionally defined operations.

This analysis is stronger than the `Deterministic` analysis,
since the latter classifies an operation as deterministic
if calls to possibly non-deterministic operations are wrapped
with encapsulated search operators, whereas this analysis
does not allow the use of any logic features.
