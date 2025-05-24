Groundness/non-determinism effect analysis
------------------------------------------

This analysis assigns to each operation the conditions under which
the evaluation of this operation might perform non-deterministic steps.
The non-deterministic steps might be due to a `choice` (overlapping rules)
or narrowing steps, where the latter might depend on the non-groundness
of particular arguments.

For instance, the operation

    not True  = False
    not False = True

performs non-deterministic steps if the first argument is non-ground.

The idea and details of this analysis can be found in the
[ICLP 2005 paper](https://doi.org/10.1007/11562931_21).
