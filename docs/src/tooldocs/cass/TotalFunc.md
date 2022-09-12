Totally and functionally definedness analysis
---------------------------------------------

This analysis assigns to each operation a flag which is `True`
if this operation is completely and functionally defined on its input types,
i.e., deterministically reducible for all ground data terms as inputs.

Thus, an operation is totally and functionally defined if

* it is pattern complete
* it does not contain overlapping left-hand sides or free variables
* it depends only on totally and functionally defined operations.

Hence, this analysis combines the analyses `Functional` and `Total`.
