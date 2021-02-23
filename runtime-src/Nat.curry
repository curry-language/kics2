module Nat where

-- ---------------------------------------------------------------------------
-- Test Cases
-- ---------------------------------------------------------------------------

{-
import Test.EasyCheck

fromNat :: Nat -> Int
fromNat IHi   = 1
fromNat (O n) = 2 * fromNat n
fromNat (I n) = 2 * fromNat n + 1

fromBinInt :: BinInt -> Int
fromBinInt (Neg n) = - (fromNat n)
fromBinInt Zero    = 0
fromBinInt (Pos n) = fromNat n

test_cmpNat :: Nat -> Nat -> Prop
test_cmpNat x y = cmpNat x y -=- compare (fromNat x) (fromNat y)

test_succ :: Nat -> Prop
test_succ x = fromNat (succ x) -=- fromNat x + 1

test_pred :: Nat -> Prop
test_pred x = x /= IHi ==> fromNat (pred x) -=- fromNat x - 1

test_addNat :: Nat -> Nat -> Prop
test_addNat x y = fromNat (x +^ y) -=- fromNat x + fromNat y

test_subNat :: Nat -> Nat -> Prop
test_subNat x y = fromBinInt (x -^ y) -=- fromNat x - fromNat y

test_mult2 :: BinInt -> Prop
test_mult2 x = fromBinInt (mult2 x) -=- fromBinInt x * 2

test_multNat :: Nat -> Nat -> Prop
test_multNat x y = fromNat (x *^ y) -=- fromNat x * fromNat y

test_div2 :: Nat -> Prop
test_div2 x = x /= IHi ==> fromNat (div2 x) -=- fromNat x `div` 2

test_mod2 :: Nat -> Prop
test_mod2 x = fromBinInt (mod2 x) -=- fromNat x `mod` 2

test_quotRemNat :: Nat -> Nat -> Prop
test_quotRemNat x y =
  let (q, r) = quotRemNat x y
  in  (fromBinInt q, fromBinInt r) -=- quotRem (fromNat x) (fromNat y)

test_lteqInteger :: BinInt -> BinInt -> Prop
test_lteqInteger x y = lteqInteger x y -=- fromBinInt x <= fromBinInt y

test_cmpInteger :: BinInt -> BinInt -> Prop
test_cmpInteger x y = cmpInteger x y -=- fromBinInt x `compare` fromBinInt y

test_neg :: BinInt -> Prop
test_neg x = fromBinInt (neg x) -=- - (fromBinInt x)

test_inc :: BinInt -> Prop
test_inc x = fromBinInt (inc x) -=- fromBinInt x + 1

test_dec :: BinInt -> Prop
test_dec x = fromBinInt (dec x) -=- fromBinInt x - 1

test_add :: BinInt -> BinInt -> Prop
test_add x y = fromBinInt (x +# y) -=- fromBinInt x + fromBinInt y

test_sub :: BinInt -> BinInt -> Prop
test_sub x y = fromBinInt (x -# y) -=- fromBinInt x - fromBinInt y

test_mult :: BinInt -> BinInt -> Prop
test_mult x y = fromBinInt (x *# y) -=- fromBinInt x * fromBinInt y

test_quotRem :: BinInt -> BinInt -> Prop
test_quotRem x y
  = y /= Zero ==>
    let (q, r) = quotRemInteger x y
    in  (fromBinInt q, fromBinInt r) -=- quotRem (fromBinInt x) (fromBinInt y)

test_divMod :: BinInt -> BinInt -> Prop
test_divMod x y
  = y /= Zero ==>
    let (d, m) = divModInteger x y
    in  (fromBinInt d, fromBinInt m) -=- divMod (fromBinInt x) (fromBinInt y)

test_div :: BinInt -> BinInt -> Prop
test_div x y
  = y /= Zero ==>
    fromBinInt (divInteger x y) -=- div (fromBinInt x) (fromBinInt y)

test_mod :: BinInt -> BinInt -> Prop
test_mod x y
  = y /= Zero ==>
    fromBinInt (modInteger x y) -=- mod (fromBinInt x) (fromBinInt y)

test_quot :: BinInt -> BinInt -> Prop
test_quot x y
  = y /= Zero ==>
    fromBinInt (quotInteger x y) -=- quot (fromBinInt x) (fromBinInt y)

test_rem :: BinInt -> BinInt -> Prop
test_rem x y
  = y /= Zero ==>
    fromBinInt (remInteger x y) -=- rem (fromBinInt x) (fromBinInt y)

-}

-- ---------------------------------------------------------------------------
-- Nat
-- ---------------------------------------------------------------------------

--- Algebraic data type to represent natural numbers
data Nat = IHi | O Nat | I Nat

--- comparison, O(min (m,n))
cmpNat :: Nat -> Nat -> Ordering
cmpNat IHi   IHi   = EQ
cmpNat IHi   (O _) = LT
cmpNat IHi   (I _) = LT
cmpNat (O _) IHi   = GT
cmpNat (O x) (O y) = cmpNat x y
cmpNat (O x) (I y) = case cmpNat x y of
  EQ    -> LT
  cmpxy -> cmpxy
cmpNat (I _) IHi   = GT
cmpNat (I x) (O y) = case cmpNat x y of
  EQ    -> GT
  cmpxy -> cmpxy
cmpNat (I x) (I y) = cmpNat x y

--- equality, O(min (m, n))
eqNat :: Nat -> Nat -> Bool
eqNat m n = case (m, n) of
  (IHi, IHi) -> True
  (I x, I y) -> eqNat x y
  (O x, O y) -> eqNat x y
  _          -> False

--- successor, O(n)
succ :: Nat -> Nat
succ IHi    = O IHi        -- 1       + 1 = 2
succ (O bs) = I bs         -- 2*n     + 1 = 2*n + 1
succ (I bs) = O (succ bs)  -- 2*n + 1 + 1 = 2*(n+1)

--- predecessor, O(n)
pred :: Nat -> Nat
pred IHi         = failed     -- 1 has no predecessor
pred (O IHi)     = IHi        -- 2           - 1 = 1
pred (O x@(O _)) = I (pred x) -- 2*2*n       - 1 = 2*(2*n-1) + 1
pred (O (I x))   = I (O x)    -- 2*(2*n + 1) - 1 = 2*2*n + 1
pred (I x)       = O x        -- 2*n + 1      -1 = 2*n

--- addition, O(max (m, n))
(+^) :: Nat -> Nat -> Nat
IHi +^ y   = succ y           -- 1  +  n   = n + 1
O x +^ IHi = I x              -- 2*n + 1   = 2*n + 1
O x +^ O y = O (x +^ y)       -- 2*m + 2*n = 2*(m+n)
O x +^ I y = I (x +^ y)
I x +^ IHi = O (succ x)
I x +^ O y = I (x +^ y)
I x +^ I y = O (succ x +^ y)

--- subtraction
(-^) :: Nat -> Nat -> BinInt
IHi     -^ y     = inc (Neg y)           -- 1-n = 1+(-n)
x@(O _) -^ IHi   = Pos (pred x)          --
(O x)   -^ (O y) = mult2 (x -^ y)
(O x)   -^ (I y) = dec (mult2 (x -^ y))
(I x)   -^ IHi   = Pos (O x)
(I x)   -^ (O y) = inc (mult2 (x -^ y))  -- 2*n+1 - 2*m = 1+2*(n-m)
(I x)   -^ (I y) = mult2 (x -^ y)        -- 2*n+1 - (2*m+1) = 2*(n-m)

--- multiplication by 2
mult2 :: BinInt -> BinInt
mult2 (Pos n) = Pos (O n)
mult2 Zero    = Zero
mult2 (Neg n) = Neg (O n)

--- multiplication, O(m*n)
(*^) :: Nat -> Nat -> Nat
IHi   *^ y = y
(O x) *^ y = O (x *^ y)
(I x) *^ y = y +^ (O (x *^ y))
-- (I x) *^ IHi = I x
-- (I x) *^ (O y) = (O y) +^ (O (x *^ (O y))) = O (y +^ (x *^ (O y)))
-- (I x) *^ (I y) = (I y) +^ (O (x *^ (I y))) = I (y +^ (x *^ (I y)))

--- division by 2
div2 :: Nat -> Nat
div2 IHi   = failed -- 1 div 2 is not defined for Nat
div2 (O x) = x
div2 (I x) = x

--- modulo by 2
mod2 :: Nat -> BinInt
mod2 IHi   = Pos IHi
mod2 (O _) = Zero
mod2 (I _) = Pos IHi

--- quotient and remainder
quotRemNat :: Nat -> Nat -> (BinInt, BinInt)
quotRemNat x y
  | eqNat y IHi = (Pos x, Zero   ) -- quotRemNat x 1 = (x, 0)
  | eqNat x IHi = (Zero , Pos IHi) -- quotRemNat 1 y = (0, 1)
  | otherwise = case cmpNat x y of
      EQ -> (Pos IHi, Zero )   -- x = y : quotRemNat x y = (1, 0)
      LT -> (Zero   , Pos x)   -- x < y : quotRemNat x y = (0, x)
      GT -> case quotRemNat (div2 x) y of
        (Neg _, _    ) -> error "quotRemNat: negative quotient"
        (Zero , _    ) -> (Pos IHi  , x -^ y) -- x > y, x/2 < y  : quotRemNat x y = (1, x - y)
        (Pos _, Neg _) -> error "quotRemNat: negative remainder"
        (Pos d, Zero ) -> (Pos (O d), mod2 x)
        (Pos d, Pos m) -> case quotRemNat (shift x m) y of
          (Neg _ , _ ) -> error "quotRemNat: negative quotient"
          (Zero  , m') -> (Pos (O d)      , m')
          (Pos d', m') -> (Pos (O d +^ d'), m')
  where
    shift IHi   _ = error "quotRemNat.shift: IHi"
    shift (O _) n = O n
    shift (I _) n = I n

-- ---------------------------------------------------------------------------
-- Integer
-- ---------------------------------------------------------------------------

--- Algebraic data type to represent integers
data BinInt = Neg Nat | Zero | Pos Nat

--- less-than-or-equal on BinInt
lteqInteger :: BinInt -> BinInt -> Bool
lteqInteger x y = cmpInteger x y /= GT

--- comparison on BinInt, O(min (m, n))
cmpInteger :: BinInt -> BinInt -> Ordering
cmpInteger Zero    Zero    = EQ
cmpInteger Zero    (Pos _) = LT
cmpInteger Zero    (Neg _) = GT
cmpInteger (Pos _) Zero    = GT
cmpInteger (Pos x) (Pos y) = cmpNat x y
cmpInteger (Pos _) (Neg _) = GT
cmpInteger (Neg _) Zero    = LT
cmpInteger (Neg _) (Pos _) = LT
cmpInteger (Neg x) (Neg y) = cmpNat y x

--- Unary minus. Usually written as "- e".
neg :: BinInt -> BinInt
neg Zero    = Zero
neg (Pos x) = Neg x
neg (Neg x) = Pos x

--- increment
inc :: BinInt -> BinInt
inc Zero        = Pos IHi
inc (Pos n)     = Pos (succ n)
inc (Neg IHi)   = Zero
inc (Neg (O n)) = Neg (pred (O n))
inc (Neg (I n)) = Neg (O n)

--- decrement
dec :: BinInt -> BinInt
dec Zero        = Neg IHi
dec (Pos IHi)   = Zero
dec (Pos (O n)) = Pos (pred (O n))
dec (Pos (I n)) = Pos (O n)
dec (Neg n)     = Neg (succ n)

--- Adds two BinInts.
(+#)   :: BinInt -> BinInt -> BinInt
Zero      +# x     = x
x@(Pos _) +# Zero  = x
Pos x     +# Pos y = Pos (x +^ y)
Pos x     +# Neg y = x -^ y
x@(Neg _) +# Zero  = x
Neg x     +# Pos y = y -^ x
Neg x     +# Neg y = Neg (x +^ y)

--- Subtracts two BinInts.
(-#)   :: BinInt -> BinInt -> BinInt
x -# Zero  = x
x -# Pos y = x +# Neg y
x -# Neg y = x +# Pos y

--- Multiplies two BinInts.
(*#)   :: BinInt -> BinInt -> BinInt
Zero  *# _     = Zero
Pos _ *# Zero  = Zero
Pos x *# Pos y = Pos (x *^ y)
Pos x *# Neg y = Neg (x *^ y)
Neg _ *# Zero  = Zero
Neg x *# Pos y = Neg (x *^ y)
Neg x *# Neg y = Pos (x *^ y)

--- Quotient and Remainder, truncated against zero
quotRemInteger :: BinInt -> BinInt -> (BinInt, BinInt)
quotRemInteger _       Zero    = failed -- division by zero is not defined
quotRemInteger Zero    (Pos _) = (Zero, Zero)
quotRemInteger Zero    (Neg _) = (Zero, Zero)
quotRemInteger (Pos x) (Pos y) = quotRemNat x y
quotRemInteger (Neg x) (Pos y) = let (d, m) = quotRemNat x y in (neg d, neg m)
quotRemInteger (Pos x) (Neg y) = let (d, m) = quotRemNat x y in (neg d,     m)
quotRemInteger (Neg x) (Neg y) = let (d, m) = quotRemNat x y in (d    , neg m)

--- Quotient and Remainder, truncated against negative infinity
divModInteger :: BinInt -> BinInt -> (BinInt, BinInt)
divModInteger _       Zero    = failed -- division by zero is not defined
divModInteger Zero    (Pos _) = (Zero, Zero)
divModInteger Zero    (Neg _) = (Zero, Zero)
divModInteger (Pos x) (Pos y) = quotRemNat x y
divModInteger (Neg x) (Pos y) = let (d, m) = quotRemNat x y in case m of
  Zero -> (neg d, m)
  _    -> (neg (inc d), (Pos y) -# m)
divModInteger (Pos x) (Neg y) = let (d, m) = quotRemNat x y in case m of
  Zero -> (neg d, m)
  _    -> (neg (inc d), m -# (Pos y))
divModInteger (Neg x) (Neg y) = let (d, m) = quotRemNat x y in (d, neg m)

--- Integer divisor, truncated towards negative infinity.
divInteger :: BinInt -> BinInt -> BinInt
divInteger x y = fst (divModInteger x y)

--- Integer modulo, truncated towards negative infinity.
modInteger :: BinInt -> BinInt -> BinInt
modInteger x y = snd (divModInteger x y)

--- Integer quotient, truncated towards zero.
quotInteger :: BinInt -> BinInt -> BinInt
quotInteger x y = fst (quotRemInteger x y)

--- Integer remainder, truncated towards zero.
remInteger :: BinInt -> BinInt -> BinInt
remInteger x y = snd (quotRemInteger x y)
