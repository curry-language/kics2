Required values analysis
------------------------

This analysis checks for each operation in a Curry program whether
the arguments must have a particular shape in order to
compute some value.
For instance, the negation operation `not` defined by

    not True  = False
    not False = True

requires the argument value `False` in order to compute the result
`True` and it requires the argument `True` to compute the result
`False`. This property is expressed by the following abstract type:

    not : ({True} -> {False}) | ({False} -> {True})

Hence, each abstract type is a set of constructors which represents
all expressions rooted by one of the constructors in this set.
Moreover, the abstract type `any` denotes any expression.
The empty list denotes an impossible required type, i.e.,
an argument which is required but for which no applicable value exists.
For instance, the operation

    f x = solve (x && not x)

has the required value typing

    f: ({} -> True)

A detailed description of this analysis and its application can be found
in the paper on
[transforming Boolean equalities into constraints](http://doi.org/10.1007/s00165-016-0399-6).
