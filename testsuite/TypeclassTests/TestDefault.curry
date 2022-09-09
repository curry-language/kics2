-- Testing the effect of default declarations to resolve overloading
-- w.r.t. class `Num`
-- (compare Sect. 4.3.4 of the Haskell 2010 Language Report)

import Test.Prop

-- If a default declaration is not provided, the following
-- declaration will be assumed:
default (Int,Float)

-- Due to this default declaration, the following definiton will
-- be accepted and the `x` and `y` will be defaulted to type `Int`:
withDefault | x===3 & x+x === y = () where x,y free

-- If we omit defaulting, which can be obtained by setting
--
--   default ()
--
-- then an ambigous type variable will be reported due to the
-- internal unresolved overloading.

testDefault = withDefault -=- ()
