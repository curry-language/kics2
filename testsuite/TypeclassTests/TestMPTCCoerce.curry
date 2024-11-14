-- Test for multi-parameter type classes with functional dependencies

{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

import Test.Prop

-- Unique coerce between two types:
class Coerce a b | a -> b where
  coerce :: a -> b

instance Coerce Bool Int where
  coerce False = 0
  coerce True  = 1

testCoerce1 :: Prop
testCoerce1 = coerce True -=- 1

instance Coerce Int Bool where
  coerce n = if n == 0 then False else True

testCoerce2 :: Prop
testCoerce2 = coerce (1::Int) -=- True

-- Compose coerce
compCoerce :: (Coerce a c, Coerce c b) => a -> b
compCoerce = coerce . coerce

testCoerce3 :: Bool -> Prop
testCoerce3 b = compCoerce b -=- b
