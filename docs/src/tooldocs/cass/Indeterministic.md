Indeterminism analysis
----------------------

This analysis assigns to each operation a flag which is `True`
if this operation might be indeterministic, i.e., calls directly or indirectly
an indeterministic encapsulation operator or an operation
from the module `System.IO.Unsafe`, like `unsafePerformIO`.
Thus, an indeterministic is not referentially transparent
since it might deliver different results on different program runs.
