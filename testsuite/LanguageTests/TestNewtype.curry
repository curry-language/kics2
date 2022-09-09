
-- Testing newtype semantics:

import Test.Prop

newtype LIST a = LIST [a]
 deriving (Eq, Read, Show)

fromLIST :: LIST a -> [a]
fromLIST (LIST xs) = xs

aLIST :: LIST Bool
aLIST = LIST [True]

headIfTrue :: LIST Int -> Bool -> Int
headIfTrue (LIST xs) b = if b then head xs else 42

testNonStrictNT :: Prop
testNonStrictNT = headIfTrue failed False -=- 42

testShowNT :: [Int] -> Prop
testShowNT xs = show (LIST xs) -=- "LIST " ++ show xs

testReadShowNT :: [Int] -> Prop
testReadShowNT xs = LIST xs -=- read ("LIST " ++ show xs)


-- An example for the partial application of a newtype constructor
newtype Double a = Double
  {
    runDouble :: a -> (a,a)
  }

xDouble :: Double a
xDouble = Double $ \x -> (x,x)

runXD :: a -> (a,a)
runXD = runDouble xDouble

testXD :: Bool -> Prop
testXD b = runXD b -=- (b,b)
