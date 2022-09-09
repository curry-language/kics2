---------------------------------------------------------------------------------
-- Test the extension of Curry for functionak patterns as described in:
-- Sergio Antoy, Michael Hanus:
-- Declarative Programming with Function Patterns
-- Proceedings of the International Symposium on Logic-based Program Synthesis
-- and Transformation (LOPSTR'05), appeared in Springer LNCS 3901, 2006
-- http://www.informatik.uni-kiel.de/~mh/papers/LOPSTR05.html
--
-- Note: this requires the setting "curryextensions=yes" in
--  ~/.kics2rc or ~/.pakcsrc
--------------------------------------------------------------------------------

import Data.Maybe
import Test.Prop

--------------------------------------------------------------------------------
-- define operation last by a function pattern:
last :: Data a => [a] -> a
last (_ ++ [x]) = x

testLast1 = (last (map (+1) [1..200]))                 -=- 201

testLast2 = (last (take 10000 (repeat failed) ++ [1])) -=- 1

--------------------------------------------------------------------------------
-- define a palindrome constraint:
pali :: Data a => [a] -> Bool
pali (xs ++ reverse xs) = True

testPalindrome1 = always (pali "otto")
testPalindrome2 = failing (pali "toto")

--------------------------------------------------------------------------------
-- define tree transformations and search by function patterns:
data Exp = Lit Int | Var [Char] | Add Exp Exp | Mul Exp Exp
 deriving (Eq,Show)

evalTo e = Add (Lit 0) e
         ? Add e (Lit 0)
         ? Mul (Lit 1) e
         ? Mul e (Lit 1)

replace _         []    x = x
replace (Add l r) (1:p) x = Add (replace l p x) r
replace (Add l r) (2:p) x = Add l (replace r p x)
replace (Mul l r) (1:p) x = Mul (replace l p x) r
replace (Mul l r) (2:p) x = Mul l (replace r p x)

simplify :: Exp -> Exp
simplify (replace c p (evalTo x)) = replace c p x

testSimplify1 = (simplify (Mul (Lit 1) (Var "x"))) -=- (Var "x")


exp = Mul (Lit 1) (Add (Var "x") (Lit 0))

exp_n n e = if n==0 then e else Add (exp_n (n-1) e) (exp_n (n-1) e)

bigexp | e =:= exp_n 8 exp = e  where e free

-- return some variable occurring in an expression:
varInExp :: Exp -> String
varInExp (replace _ _ (Var v)) = v

testVars1 = (varInExp bigexp) <~> "x"

testVars2 = (varInExp bigexp) <~~> foldr1 (?) (take 256 (repeat "x"))


--------------------------------------------------------------------------------
-- Dijsktra's Dutch National Flag problem with function patterns

data Color = Red | White | Blue
 deriving (Eq,Show)

solve (x++[White]++y++[Red  ]++z) = solve (x++[Red]++y++[White]++z)
solve (x++[Blue ]++y++[Red  ]++z) = solve (x++[Red]++y++[Blue]++z)
solve (x++[Blue ]++y++[White]++z) = solve (x++[White]++y++[Blue]++z)
solve flag | isDutchFlag flag = flag
 where
  isDutchFlag (uni Red ++ uni White ++ uni Blue) = success

uni _ = []
uni color = color : uni color

testDutchFlag =
  solve [White,Red,White,Blue,Red,Blue,White] ~>
  [Red,Red,White,White,White,Blue,Blue]

--------------------------------------------------------------------------------
-- Some more specific tests:

mkSamePair :: a -> (a,a)
mkSamePair x = (x,x)

fpair :: Data a => (a,a) -> a
fpair (mkSamePair x) = x

gpair :: Data a => (a,a) -> b -> b
gpair (mkSamePair x) y = y

-- This yields (Just 0):
justZero   = let y,z free in fpair (y, gpair (y, Just 0) z)
testJustZero = isJust justZero -=- True

-- However, this does not yield (Just failed) due to strict unification
-- of non-linear patterns:
justFailed :: Maybe Int
justFailed = let y,z free in fpair (y, gpair (y, Just failed) z)

testJustFailed = failing (isJust justFailed)


data Nat = O | S Nat

g :: Nat -> Nat -> (Nat,Nat)
g x y = (x,y)

pair :: (Nat,Nat) -> Bool
pair (g y y) = True

testPair00 = (pair (O, O)) -=- True

testPair01 = failing (pair (O, S O))


-- This call should fail due to an occur check.
pairCyclic = let x free in pair (x, S x)
-- However, an occur check is not yet implemented in KiCS2.
--testPairCyclic = failing pairCyclic

-- This should fail due to strict unification of non-linear patterns:
testPairFailed = failing (pair (_,failed))

f (g (const O y) y) = True

-- This should not fail since the non-linearity does not occur in the
-- actually evaluated pattern:
testF = f (_,failed) -=- True

h (g (id y) y) = True

-- This should fail since the actually evaluated pattern is non-linear:
testH = failing (h (_,failed))


-- This call tests whether the implementation is too lazy:

singletonFail :: Int -> [Int]
singletonFail _ = [failed]

failingPattern (singletonFail x) = True

testFailingPattern = failing (failingPattern _)

