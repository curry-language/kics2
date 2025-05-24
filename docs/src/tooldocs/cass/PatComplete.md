Pattern completeness analysis
-----------------------------

This analysis analyzes an operation for a pattern-complete definition.
An operation is pattern complete if each pattern match is
defined for all constructors.

For instance, the operation

    not True  = False
    not False = True

is pattern complete, whereas the operation

    head (x:_) = x

is incomplete. If an operation is defined by overlapping rules,
it is complete if there is one alternative with complete pattern matching.
For instance, the operation

    por True  x     = True
    por x     True  = True
    por False False = False

is not complete, since it corresponds to the following definition:

    por x y = por1 x y ? por2 x y
    
    por1 True  _     = True
    por1 False False = True
    
    por2 _     True  = True

Hence, each alternative is incomplete.


Failure branches:
-----------------

Since a front-end reading Curry programs might complete missing patterns
with the failure branch, such branches are not considered as definitions.
For instance, the operation

    head (x:_) = x
    head []    = Prelude.failed

is considered as incomplete, although there is a branch for each pattern.
