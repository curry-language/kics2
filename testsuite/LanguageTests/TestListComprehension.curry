-- Testing list comprehensions

import Test.Prop

testSquareOdds =
  [ x*x | x <- [1..20], odd x ] -=- [1,9,25,49,81,121,169,225,289,361]

testPairs =
  [ (i,j) | i <- [1..3], j <- [2..4], i /= j ]
  -=- [(1,2),(1,3),(1,4),(2,3),(2,4),(3,2),(3,4)]

testFacs =
  take 10 (map (foldr (*) 1) [ [1..n] | n <- [1..] ])
  -=- [1,2,6,24,120,720,5040,40320,362880,3628800]

testLetComp =
  [ (x,y,z) | x <- [1 .. 4], y <- [2 .. 6], let z = x+y, x /= y ]
  -=- [(1,2,3),(1,3,4),(1,4,5),(1,5,6),(1,6,7),(2,3,5),(2,4,6),(2,5,7),(2,6,8)
      ,(3,2,5),(3,4,7),(3,5,8),(3,6,9),(4,2,6),(4,3,7),(4,5,9),(4,6,10)]

testConcat :: (Eq a, Show a) =>[[a]] -> Prop
testConcat xss = concat xss -=- [y | ys <- xss, y <- ys]
