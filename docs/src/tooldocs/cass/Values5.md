Result values analysis
----------------------

This analysis approximates the result values of operations in a Curry program.
Since there might be infinitely many possible values, the analysis
approximates only the outermost constructors of the result values
*up to a depth of 5*, i.e., deeper constructor terms are ignored.

In the output of the analysis, alternatives are enclosed in curly brackets
and `_` indicates any possible value, i.e., no precise information
about the outermost constructors.

For instance, consider the operations

    not False = True
    not True  = False
    
    f x = Just (not x)
    
    g x = Just (f x)
    
    bools = (False?True) : bools

Then the analysis results are:

    not : {False, True}
    f : Just {False, True}
    g : Just (Just {False, True})
    bools : {False, True}:({False, True}:({False, True}:({False, True}:(_:_))))
    
Thus, the results at positions exceeding the depth of 5 are always `_`.

