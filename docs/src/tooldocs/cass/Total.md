Totally definedness analysis
----------------------------

This analysis assigns to each operation a flag which is `True`
if this operation is completely defined on its input types,
i.e., reducible for all ground data terms.
Thus, an operation is totally defined if it is pattern complete
(see analysis `PatComplete`) and depends only on totally defined operations.
