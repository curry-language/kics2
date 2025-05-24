Result values analysis
----------------------

This analysis approximates the result values of operations in a Curry program.
Since there might be infinitely many possible values, the analysis
approximates only the outermost constructors of the result values.
Thus, a result could be `{c1,...,ck}` (where `c1,...,ck` are constructors
of some data type) or `_` (which means that there is no precise
information about the outermost constructors available).

For instance, consider the operations

    not False = True
    not True  = False

    id x = x

    loop = loop

Then the analysis results are:

    not  : {False, True}
    id   : _
    loop : {}

The empty set as a result of `loop` indicates that this operation
does not return any value.
