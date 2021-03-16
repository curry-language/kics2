{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
module PrimTypes where

import System.IO (Handle)

import KiCS2Debug
import FailInfo
import Types

type C_Apply a b = a b

-- -----------------------------------------------------------------------------
-- Nat
-- -----------------------------------------------------------------------------

data Nat
  = IHi
  | O Nat
  | I Nat
  | Choice_Nat Cover ID Nat Nat
  | Choices_Nat Cover ID [Nat]
  | Fail_Nat Cover FailInfo
  | Guard_Nat Cover Constraints Nat

instance Show Nat where
  showsPrec d (Choice_Nat cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_Nat cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_Nat cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_Nat cd info) = showChar '!'
#ifdef BinaryInt
  showsPrec _ IHi = showString "IHi"
  showsPrec _ (O x1) = showString "(O" . (showChar ' ' . (shows x1 . showChar
    ')'))
  showsPrec _ (I x1) = showString "(I" . (showChar ' ' . (shows x1 . showChar
    ')'))
#else
  showsPrec d x1                    = showTerm 1 0 x1
    where
    showTerm :: Integer -> Integer -> Nat -> String -> String
    showTerm a c IHi   = shows    (a + c)
    showTerm a c (O n) = showTerm (2 * a) c       n
    showTerm a c (I n) = showTerm (2 * a) (c + a) n
    showTerm a c x
      | a <= 1           = showsPrec d x
      | c == 0           = showChar '(' . shows a . showString " * "
                         . showsPrec d x . showChar ')'
      | otherwise        = showChar '(' . shows a . showString " * "
                         . showsPrec d x . showString " + "
                         . shows c . showChar ')'
#endif

instance Read Nat where
  readsPrec d s = readParen False (\r -> [(IHi, r0) | (_, r0) <- readQualified
    "Prelude" "IHi" r]) s ++ (readParen ((>) d 10) (\r -> [(O x1, r1) | (_, r0) <-
    readQualified "Prelude" "O" r, (x1, r1) <- readsPrec 11 r0]) s ++ readParen ((>)
    d 10) (\r -> [(I x1, r1) | (_, r0) <- readQualified "Prelude" "I" r, (x1, r1)
    <- readsPrec 11 r0]) s)

instance NonDet Nat where
  choiceCons = Choice_Nat
  choicesCons = Choices_Nat
  failCons = Fail_Nat
  guardCons = Guard_Nat
  try (Choice_Nat cd i x y) = tryChoice cd i x y
  try (Choices_Nat cd i xs) = tryChoices cd i xs
  try (Fail_Nat cd info) = Fail cd info
  try (Guard_Nat cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_Nat cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_Nat cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_Nat cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_Nat cd i _) = error
    ("Prelude.Nat.match: Choices with ChoiceID " ++ show i)
  match _ _ _ f _ _ (Fail_Nat cd info) = f cd info
  match _ _ _ _ f _ (Guard_Nat cd c e) = f cd c e
  match _ _ _ _ _ f x = f x

instance Generable Nat where
  generate s c = Choices_Nat c (freeID [0, 1, 1] s) [IHi, O (generate
    (leftSupply s) c), I (generate (leftSupply s) c)]

instance NormalForm Nat where
  ($!!) cont IHi d cs = cont IHi d cs
  ($!!) cont (O x1) d cs = ($!!) (\y1 d cs -> cont (O y1) d cs) x1 d cs
  ($!!) cont (I x1) d cs = ($!!) (\y1 d cs -> cont (I y1) d cs) x1 d cs
  ($!!) cont (Choice_Nat cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_Nat cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_Nat cd c e) d cs = guardCons cd c (($!!) cont e d (addCs c
    cs))
  ($!!) _ (Fail_Nat cd info) _ _ = failCons cd info
  ($##) cont IHi d cs = cont IHi d cs
  ($##) cont (O x1) d cs = ($##) (\y1 d cs -> cont (O y1) d cs) x1 d cs
  ($##) cont (I x1) d cs = ($##) (\y1 d cs -> cont (I y1) d cs) x1 d cs
  ($##) cont (Choice_Nat cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_Nat cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_Nat cd c e) d cs = guardCons cd c (($##) cont e d (addCs c
    cs))
  ($##) _ (Fail_Nat cd info) _ _ = failCons cd info
  showCons IHi = "Prelude.IHi"
  showCons (O _) = "Prelude.O _"
  showCons (I _) = "Prelude.I _"
  showCons x = error ("Prelude.Nat.showCons: no constructor: " ++ show x)
  searchNF _ cont IHi = cont IHi
  searchNF search cont (O x1) = search (\y1 -> cont (O y1)) x1
  searchNF search cont (I x1) = search (\y1 -> cont (I y1)) x1
  searchNF _ _ x = error ("Prelude.Nat.searchNF: no constructor: " ++ show x)

instance Unifiable Nat where
  (=.=) IHi IHi d cs = C_True
  (=.=) (O x1) (O y1) d cs = (=:=) x1 y1 d cs
  (=.=) (I x1) (I y1) d cs = (=:=) x1 y1 d cs
  (=.=) a b cd _ = Fail_C_Bool cd (unificationFail (showCons a) (showCons b))
  (=.<=) IHi IHi d cs = C_True
  (=.<=) (O x1) (O y1) d cs = (=:<=) x1 y1 d cs
  (=.<=) (I x1) (I y1) d cs = (=:<=) x1 y1 d cs
  (=.<=) a b cd _ = Fail_C_Bool cd (unificationFail (showCons a) (showCons b))
  bind cd i IHi = (i :=: ChooseN 0 0) : concat []
  bind cd i (O x3) = (i :=: ChooseN 1 1) : concat [bind cd (leftID i) x3]
  bind cd i (I x3) = (i :=: ChooseN 2 1) : concat [bind cd (leftID i) x3]
  bind d i (Choice_Nat cd j x y) = [ConstraintChoice cd j (bind d i x) (bind d
    i y)]
  bind d i (Choices_Nat cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_Nat cd j@(NarrowedID _ _) xs) = [ConstraintChoices cd j
    (map (bind d i) xs)]
  bind _ _ (Choices_Nat cd i _) = error
    ("Prelude.Nat.bind: Choices with ChoiceID: " ++ show i)
  bind _ _ (Fail_Nat cd info) = [Unsolvable info]
  bind d i (Guard_Nat cd c e) = getConstrList c ++ bind d i e
  lazyBind cd i IHi = (i :=: ChooseN 0 0) : []
  lazyBind cd i (O x3) = (i :=: ChooseN 1 1) : [leftID i :=: LazyBind
    (lazyBind cd (leftID i) x3)]
  lazyBind cd i (I x3) = (i :=: ChooseN 2 1) : [leftID i :=: LazyBind
    (lazyBind cd (leftID i) x3)]
  lazyBind d i (Choice_Nat cd j x y) = [ConstraintChoice cd j (lazyBind d i x)
    (lazyBind d i y)]
  lazyBind d i (Choices_Nat cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j
    xs
  lazyBind d i (Choices_Nat cd j@(NarrowedID _ _) xs) = [ConstraintChoices cd
    j (map (lazyBind d i) xs)]
  lazyBind _ _ (Choices_Nat cd i _) = error
    ("Prelude.Nat.lazyBind: Choices with ChoiceID: " ++ show i)
  lazyBind _ _ (Fail_Nat cd info) = [Unsolvable info]
  lazyBind d i (Guard_Nat cd c e) = getConstrList c ++ [i :=: LazyBind
    (lazyBind d i e)]

instance Curry Nat

d_C_prim_eqNat :: Nat -> Nat -> Cover -> ConstStore -> C_Bool
d_C_prim_eqNat (Choice_Nat cd i x y) z d cs =
  narrow cd i (((x `d_C_prim_eqNat` z) d) cs) (((y `d_C_prim_eqNat` z) d) cs)
d_C_prim_eqNat (Choices_Nat  cd i xs) y d cs =
  narrows cs cd i (\x -> ((x `d_C_prim_eqNat` y) d) cs) xs
d_C_prim_eqNat (Guard_Nat  cd c e) y d cs =
  guardCons cd c (((e `d_C_prim_eqNat` y) d) (addCs c cs))
d_C_prim_eqNat (Fail_Nat  cd info) _ _ _ = failCons cd info
d_C_prim_eqNat z (Choice_Nat  cd i x y) d cs =
  narrow cd i (((z `d_C_prim_eqNat` x) d) cs) (((z `d_C_prim_eqNat` y) d) cs)
d_C_prim_eqNat y (Choices_Nat  cd i xs) d cs =
  narrows cs cd i (\x -> ((y `d_C_prim_eqNat` x) d) cs) xs
d_C_prim_eqNat y (Guard_Nat  cd c e) d cs =
  guardCons cd c (((y `d_C_prim_eqNat` e) d) (addCs c cs))
d_C_prim_eqNat _ (Fail_Nat  cd info) _ _ = failCons cd info
d_C_prim_eqNat IHi IHi d cs = C_True
d_C_prim_eqNat (O x1) (O y1) d cs = ((x1 `d_C_prim_eqNat` y1) d) cs
d_C_prim_eqNat (I x1) (I y1) d cs = ((x1 `d_C_prim_eqNat` y1) d) cs
d_C_prim_eqNat _ _ d _ = C_False

d_C_prim_ltEqNat :: Nat -> Nat -> Cover -> ConstStore -> C_Bool
d_C_prim_ltEqNat (Choice_Nat  cd i x y) z d cs =
  narrow cd i (((x `d_C_prim_ltEqNat` z) d) cs) (((y `d_C_prim_ltEqNat` z) d) cs)
d_C_prim_ltEqNat (Choices_Nat  cd i xs) y d cs =
  narrows cs cd i (\x -> ((x `d_C_prim_ltEqNat` y) d) cs) xs
d_C_prim_ltEqNat (Guard_Nat  cd c e) y d cs =
  guardCons cd c (((e `d_C_prim_ltEqNat` y) d) (addCs c cs))
d_C_prim_ltEqNat (Fail_Nat  cd info) _ _ _ = failCons cd info
d_C_prim_ltEqNat z (Choice_Nat  cd i x y) d cs =
  narrow cd i (((z `d_C_prim_ltEqNat` x) d) cs) (((z `d_C_prim_ltEqNat` y) d) cs)
d_C_prim_ltEqNat y (Choices_Nat  cd i xs) d cs =
  narrows cs cd i (\x -> ((y `d_C_prim_ltEqNat` x) d) cs) xs
d_C_prim_ltEqNat y (Guard_Nat  cd c e) d cs =
  guardCons cd c (((y `d_C_prim_ltEqNat` e) d) (addCs c cs))
d_C_prim_ltEqNat _ (Fail_Nat  cd info) _ _ = failCons cd info
d_C_prim_ltEqNat IHi IHi d cs = C_True
d_C_prim_ltEqNat IHi (O _) _ _ = C_True
d_C_prim_ltEqNat IHi (I _) _ _ = C_True
d_C_prim_ltEqNat (O x1) (O y1) d cs = ((x1 `d_C_prim_ltEqNat` y1) d) cs
d_C_prim_ltEqNat (O _) (I _) _ _ = C_True
d_C_prim_ltEqNat (I x1) (I y1) d cs = ((x1 `d_C_prim_ltEqNat` y1) d) cs
d_C_prim_ltEqNat _ _ d _ = C_False

-- -----------------------------------------------------------------------------
-- BinInt
-- -----------------------------------------------------------------------------

data BinInt
  = Neg Nat
  | Zero
  | Pos Nat
  | Choice_BinInt Cover ID BinInt BinInt
  | Choices_BinInt Cover ID [BinInt]
  | Fail_BinInt Cover FailInfo
  | Guard_BinInt Cover Constraints BinInt

instance Show BinInt where
  showsPrec d (Choice_BinInt cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_BinInt cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_BinInt cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_BinInt cd info) = showChar '!'
#ifdef BinaryInt
  showsPrec _ (Neg x1) = showString "(Neg" . (showChar ' ' . (shows x1 .
    showChar ')'))
  showsPrec _ Zero = showString "Zero"
  showsPrec _ (Pos x1) = showString "(Pos" . (showChar ' ' . (shows x1 .
    showChar ')'))
#else
  showsPrec d (Neg                x) = showString "(-" . showsPrec d x
                                       . showChar ')'
  showsPrec _ Zero                   = showChar '0'
  showsPrec d (Pos                x) = showsPrec d x
#endif

instance Read BinInt where
  readsPrec d s = readParen ((>) d 10) (\r -> [(Neg x1, r1) | (_, r0) <-
    readQualified "Prelude" "Neg" r, (x1, r1) <- readsPrec 11 r0]) s ++ (readParen
    False (\r -> [(Zero, r0) | (_, r0) <- readQualified "Prelude" "Zero" r]) s ++
    readParen ((>) d 10) (\r -> [(Pos x1, r1) | (_, r0) <- readQualified "Prelude"
    "Pos" r, (x1, r1) <- readsPrec 11 r0]) s)

instance NonDet BinInt where
  choiceCons = Choice_BinInt
  choicesCons = Choices_BinInt
  failCons = Fail_BinInt
  guardCons = Guard_BinInt
  try (Choice_BinInt cd i x y) = tryChoice cd i x y
  try (Choices_BinInt cd i xs) = tryChoices cd i xs
  try (Fail_BinInt cd info) = Fail cd info
  try (Guard_BinInt cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_BinInt cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_BinInt cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_BinInt cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_BinInt cd i _) = error
    ("Prelude.BinInt.match: Choices with ChoiceID " ++ show i)
  match _ _ _ f _ _ (Fail_BinInt cd info) = f cd info
  match _ _ _ _ f _ (Guard_BinInt cd c e) = f cd c e
  match _ _ _ _ _ f x = f x

instance Generable BinInt where
  generate s c = Choices_BinInt c (freeID [1, 0, 1] s) [Neg (generate
    (leftSupply s) c), Zero, Pos (generate (leftSupply s) c)]

instance NormalForm BinInt where
  ($!!) cont (Neg x1) d cs = ($!!) (\y1 d cs -> cont (Neg y1) d cs) x1 d cs
  ($!!) cont Zero d cs = cont Zero d cs
  ($!!) cont (Pos x1) d cs = ($!!) (\y1 d cs -> cont (Pos y1) d cs) x1 d cs
  ($!!) cont (Choice_BinInt cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_BinInt cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_BinInt cd c e) d cs = guardCons cd c (($!!) cont e d
    (addCs c cs))
  ($!!) _ (Fail_BinInt cd info) _ _ = failCons cd info
  ($##) cont (Neg x1) d cs = ($##) (\y1 d cs -> cont (Neg y1) d cs) x1 d cs
  ($##) cont Zero d cs = cont Zero d cs
  ($##) cont (Pos x1) d cs = ($##) (\y1 d cs -> cont (Pos y1) d cs) x1 d cs
  ($##) cont (Choice_BinInt cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_BinInt cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_BinInt cd c e) d cs = guardCons cd c (($##) cont e d
    (addCs c cs))
  ($##) _ (Fail_BinInt cd info) _ _ = failCons cd info
  showCons (Neg _) = "Prelude.Neg _"
  showCons Zero = "Prelude.Zero"
  showCons (Pos _) = "Prelude.Pos _"
  showCons x = error ("Prelude.BinInt.showCons: no constructor: " ++ show x)
  searchNF search cont (Neg x1) = search (\y1 -> cont (Neg y1)) x1
  searchNF _ cont Zero = cont Zero
  searchNF search cont (Pos x1) = search (\y1 -> cont (Pos y1)) x1
  searchNF _ _ x = error ("Prelude.BinInt.searchNF: no constructor: " ++ show x)

instance Unifiable BinInt where
  (=.=) (Neg x1) (Neg y1) d cs = (=:=) x1 y1 d cs
  (=.=) Zero Zero d cs = C_True
  (=.=) (Pos x1) (Pos y1) d cs = (=:=) x1 y1 d cs
  (=.=) a b cd _ = Fail_C_Bool cd (unificationFail (showCons a) (showCons b))
  (=.<=) (Neg x1) (Neg y1) d cs = (=:<=) x1 y1 d cs
  (=.<=) Zero Zero d cs = C_True
  (=.<=) (Pos x1) (Pos y1) d cs = (=:<=) x1 y1 d cs
  (=.<=) a b cd _ = Fail_C_Bool cd (unificationFail (showCons a) (showCons b))
  bind cd i (Neg x3) = (i :=: ChooseN 0 1) : concat [bind cd (leftID i) x3]
  bind cd i Zero = (i :=: ChooseN 1 0) : concat []
  bind cd i (Pos x3) = (i :=: ChooseN 2 1) : concat [bind cd (leftID i) x3]
  bind d i (Choice_BinInt cd j x y) = [ConstraintChoice cd j (bind d i x)
    (bind d i y)]
  bind d i (Choices_BinInt cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_BinInt cd j@(NarrowedID _ _) xs) = [ConstraintChoices cd j
    (map (bind d i) xs)]
  bind _ _ (Choices_BinInt cd i _) = error
    ("Prelude.BinInt.bind: Choices with ChoiceID: " ++ show i)
  bind _ _ (Fail_BinInt cd info) = [Unsolvable info]
  bind d i (Guard_BinInt cd c e) = getConstrList c ++ bind d i e
  lazyBind cd i (Neg x3) = (i :=: ChooseN 0 1) : [leftID i :=: LazyBind
    (lazyBind cd (leftID i) x3)]
  lazyBind cd i Zero = (i :=: ChooseN 1 0) : []
  lazyBind cd i (Pos x3) = (i :=: ChooseN 2 1) : [leftID i :=: LazyBind
    (lazyBind cd (leftID i) x3)]
  lazyBind d i (Choice_BinInt cd j x y) = [ConstraintChoice cd j (lazyBind d i
    x) (lazyBind d i y)]
  lazyBind d i (Choices_BinInt cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd
    j xs
  lazyBind d i (Choices_BinInt cd j@(NarrowedID _ _) xs) = [ConstraintChoices
    cd j (map (lazyBind d i) xs)]
  lazyBind _ _ (Choices_BinInt cd i _) = error
    ("Prelude.BinInt.lazyBind: Choices with ChoiceID: " ++ show i)
  lazyBind _ _ (Fail_BinInt cd info) = [Unsolvable info]
  lazyBind d i (Guard_BinInt cd c e) = getConstrList c ++ [i :=: LazyBind
    (lazyBind d i e)]

instance Curry BinInt

d_C_prim_eqBinInt :: BinInt -> BinInt -> Cover -> ConstStore -> C_Bool
d_C_prim_eqBinInt (Choice_BinInt cd i x y) z d cs =
  narrow cd i (((x `d_C_prim_eqBinInt` z) d) cs) (((y `d_C_prim_eqBinInt` z) d) cs)
d_C_prim_eqBinInt (Choices_BinInt cd i xs) y d cs =
  narrows cs cd i (\x -> ((x `d_C_prim_eqBinInt` y) d) cs) xs
d_C_prim_eqBinInt (Guard_BinInt cd c e) y d cs =
  guardCons cd c (((e `d_C_prim_eqBinInt` y) d) (addCs c cs))
d_C_prim_eqBinInt (Fail_BinInt cd info) _ _ _ = failCons cd info
d_C_prim_eqBinInt z (Choice_BinInt cd i x y) d cs =
  narrow cd i (((z `d_C_prim_eqBinInt` x) d) cs) (((z `d_C_prim_eqBinInt` y) d) cs)
d_C_prim_eqBinInt y (Choices_BinInt cd i xs) d cs =
  narrows cs cd i (\x -> ((y `d_C_prim_eqBinInt` x) d) cs) xs
d_C_prim_eqBinInt y (Guard_BinInt cd c e) d cs =
  guardCons cd c (((y `d_C_prim_eqBinInt` e) d) (addCs c cs))
d_C_prim_eqBinInt _ (Fail_BinInt cd info) _ _ = failCons cd info
d_C_prim_eqBinInt (Neg x1) (Neg y1) d cs = ((x1 `d_C_prim_eqNat` y1) d) cs
d_C_prim_eqBinInt Zero Zero d cs = C_True
d_C_prim_eqBinInt (Pos x1) (Pos y1) d cs = ((x1 `d_C_prim_eqNat` y1) d) cs
d_C_prim_eqBinInt _ _ d _ = C_False

d_C_prim_ltEqBinInt :: BinInt -> BinInt -> Cover -> ConstStore -> C_Bool
d_C_prim_ltEqBinInt (Choice_BinInt cd i x y) z d cs =
  narrow cd i (((x `d_C_prim_ltEqBinInt` z) d) cs) (((y `d_C_prim_ltEqBinInt` z) d) cs)
d_C_prim_ltEqBinInt (Choices_BinInt cd i xs) y d cs =
  narrows cs cd i (\x -> ((x `d_C_prim_ltEqBinInt` y) d) cs) xs
d_C_prim_ltEqBinInt (Guard_BinInt cd c e) y d cs =
  guardCons cd c (((e `d_C_prim_ltEqBinInt` y) d) (addCs c cs))
d_C_prim_ltEqBinInt (Fail_BinInt cd info) _ _ _ = failCons cd info
d_C_prim_ltEqBinInt z (Choice_BinInt cd i x y) d cs =
  narrow cd i (((z `d_C_prim_ltEqBinInt` x) d) cs) (((z `d_C_prim_ltEqBinInt` y) d) cs)
d_C_prim_ltEqBinInt y (Choices_BinInt cd i xs) d cs =
  narrows cs cd i (\x -> ((y `d_C_prim_ltEqBinInt` x) d) cs) xs
d_C_prim_ltEqBinInt y (Guard_BinInt cd c e) d cs =
  guardCons cd c (((y `d_C_prim_ltEqBinInt` e) d) (addCs c cs))
d_C_prim_ltEqBinInt _ (Fail_BinInt cd info) _ _ = failCons cd info
d_C_prim_ltEqBinInt (Neg x1) (Neg y1) d cs = ((x1 `d_C_prim_ltEqNat` y1) d) cs
d_C_prim_ltEqBinInt (Neg _) Zero _ _ = C_True
d_C_prim_ltEqBinInt (Neg _) (Pos _) _ _ = C_True
d_C_prim_ltEqBinInt Zero Zero d cs = C_True
d_C_prim_ltEqBinInt Zero (Pos _) _ _ = C_True
d_C_prim_ltEqBinInt (Pos x1) (Pos y1) d cs = ((x1 `d_C_prim_ltEqNat` y1) d) cs
d_C_prim_ltEqBinInt _ _ d _ = C_False

-- -----------------------------------------------------------------------------
-- Higher Order Funcs
-- -----------------------------------------------------------------------------

-- BEGIN GENERATED FROM PrimTypes.curry
data Func t0 t1
     = Func (t0 -> IDSupply -> Cover -> ConstStore -> t1)
     | Choice_Func Cover ID (Func t0 t1) (Func t0 t1)
     | Choices_Func Cover ID ([Func t0 t1])
     | Fail_Func Cover FailInfo
     | Guard_Func Cover Constraints (Func t0 t1)

instance Show (Func a b) where
 showsPrec d (Choice_Func cd i f1 f2) = showsChoice d cd i f1 f2
 showsPrec d (Choices_Func cd i fs)   = showsChoices d cd i fs
 showsPrec _ (Fail_Func _ _)          = showChar '!'
 showsPrec d (Guard_Func cd c f)      = showsGuard d cd c f
 showsPrec _ (Func _)                 = showString "<<function>>"

instance Read (Func a b) where readsPrec = internalError "readsPrec for Func"

instance NonDet (Func t0 t1) where
  choiceCons = Choice_Func
  choicesCons = Choices_Func
  failCons = Fail_Func
  guardCons = Guard_Func
  try (Choice_Func cd i x y) = tryChoice cd i x y
  try (Choices_Func cd i xs) = tryChoices cd i xs
  try (Fail_Func cd info) = Fail cd info
  try (Guard_Func cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_Func cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_Func cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_Func cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_Func _ i _) = internalError ("Prelude.Func.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_Func cd info) = f cd info
  match _ _ _ _ f _ (Guard_Func cd cs e) = f cd cs e
  match _ _ _ _ _ f x = f x

instance Generable (Func a b) where generate _ = internalError "generate for Func"

instance (NormalForm t0,NormalForm t1) => NormalForm (Func t0 t1) where
  ($!!) cont f@(Func _) cd cs = cont f cd cs
  ($!!) cont (Choice_Func d i x y) cd cs = nfChoice cont d i x y cd cs
  ($!!) cont (Choices_Func d i xs) cd cs = nfChoices cont d i xs cd cs
  ($!!) cont (Guard_Func d c x) cd cs = guardCons d c ((cont $!! x) cd $! addCs c cs)
  ($!!) _ (Fail_Func d info) _ _ = failCons d info
  ($##) cont f@(Func _) cd cs = cont f cd cs
  ($##) cont (Choice_Func d i x y) cd cs = gnfChoice cont d i x y cd cs
  ($##) cont (Choices_Func d i xs) cd cs = gnfChoices cont d i xs cd cs
  ($##) cont (Guard_Func d c x) cd cs = guardCons d c ((cont $## x) cd $! addCs c cs)
  ($##) _ (Fail_Func d info) _ _ = failCons d info
  searchNF search cont (Func x1) = search (\y1 -> cont (Func y1)) x1
  searchNF _ _ x = internalError ("Prelude.Func.searchNF: no constructor: " ++ (show x))

instance (Unifiable t0,Unifiable t1) => Unifiable (Func t0 t1) where
  (=.=) _ _ cd _ = Fail_C_Bool cd defFailInfo
  (=.<=) _ _ cd _ = Fail_C_Bool cd defFailInfo
  bind _  _ (Func _) = internalError "can not bind a Func"
  bind cd i (Choice_Func d j l r) = [(ConstraintChoice d j (bind cd i l) (bind cd i r))]
  bind cd i (Choices_Func d j@(FreeID _ _) xs) = bindOrNarrow cd i d j xs
  bind cd i (Choices_Func d j@(NarrowedID _ _) xs) = [(ConstraintChoices d j (map (bind cd i) xs))]
  bind _  _ (Choices_Func _ i@(ChoiceID _) _) = internalError ("Prelude.Func.bind: Choices with ChoiceID: " ++ (show i))
  bind _  _ (Fail_Func _ info) = [Unsolvable info]
  bind cd i (Guard_Func _ cs e) = (getConstrList cs) ++ (bind cd i e)
  lazyBind _  _ (Func _) = internalError "can not lazily bind a Func"
  lazyBind cd i (Choice_Func d j l r) = [(ConstraintChoice d j (lazyBind cd i l) (lazyBind cd i r))]
  lazyBind cd i (Choices_Func d j@(FreeID _ _) xs) = lazyBindOrNarrow cd i d j xs
  lazyBind cd i (Choices_Func d j@(NarrowedID _ _) xs) = [(ConstraintChoices d j (map (lazyBind cd i) xs))]
  lazyBind _  _ (Choices_Func _ i _) = internalError ("Prelude.Func.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_Func _ info) = [Unsolvable info]
  lazyBind cd i (Guard_Func _ cs e) = (getConstrList cs) ++ [(i :=: (LazyBind (lazyBind cd i e)))]

instance (Curry t0, Curry t1) => Curry (Func t0 t1)
-- END GENERATED FROM PrimTypes.curry

-- -----------------------------------------------------------------------------
-- IO
-- -----------------------------------------------------------------------------

-- BEGIN GENERATED FROM PrimTypes.curry
data C_IO t0
     = C_IO (IO (Either FailInfo t0))
     | HO_C_IO (IDSupply -> Cover -> ConstStore -> IO (Either FailInfo t0))
     | Choice_C_IO Cover ID (C_IO t0) (C_IO t0)
     | Choices_C_IO Cover ID ([C_IO t0])
     | Fail_C_IO Cover FailInfo
     | Guard_C_IO Cover Constraints (C_IO t0)

instance Show (C_IO a) where
  show _ = "<<IO action>>"

instance Read (C_IO a) where readsPrec = internalError "readsPrec for C_IO"

instance NonDet (C_IO t0) where
  choiceCons = Choice_C_IO
  choicesCons = Choices_C_IO
  failCons = Fail_C_IO
  guardCons = Guard_C_IO
  try (Choice_C_IO cd i x y) = tryChoice cd i x y
  try (Choices_C_IO cd i xs) = tryChoices cd i xs
  try (Fail_C_IO cd info) = Fail cd info
  try (Guard_C_IO cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_IO cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_IO cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_IO cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_IO _ i _) = internalError ("Prelude.IO.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_IO cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_IO cd cs e) = f cd cs e
  match _ _ _ _ _ f x = f x

instance Generable (C_IO a) where generate _ _ = internalError "generate for C_IO"

instance (NormalForm t0) => NormalForm (C_IO t0) where
  ($!!) cont io@(C_IO    _) cd cs = cont io cd cs
  ($!!) cont io@(HO_C_IO _) cd cs = cont io cd cs
  ($!!) cont (Choice_C_IO d i x y) cd cs = nfChoice cont d i x y cd cs
  ($!!) cont (Choices_C_IO d i xs) cd cs = nfChoices cont d i xs cd cs
  ($!!) cont (Guard_C_IO d c x) cd cs = guardCons d c ((cont $!! x) cd $! addCs c cs)
  ($!!) _ (Fail_C_IO d info) _ _ = failCons d info
  ($##) cont io@(C_IO    _) cd cs = cont io cd cs
  ($##) cont io@(HO_C_IO _) cd cs = cont io cd cs
  ($##) cont (Choice_C_IO d i x y) cd cs = gnfChoice cont d i x y cd cs
  ($##) cont (Choices_C_IO d i xs) cd cs = gnfChoices cont d i xs cd cs
  ($##) cont (Guard_C_IO d c x) cd cs = guardCons d c ((cont $## x) cd $! addCs c cs)
  ($##) _ (Fail_C_IO d info) _ _ = failCons d info
  searchNF _ cont io@(C_IO    _) = cont io
  searchNF _ cont io@(HO_C_IO _) = cont io
  searchNF _ _ x = internalError ("Prelude.IO.searchNF: no constructor: " ++ (show x))

instance Unifiable t0 => Unifiable (C_IO t0) where
  (=.=) _ _ cd _ = Fail_C_Bool cd defFailInfo
  (=.<=) _ _ cd _ = Fail_C_Bool cd defFailInfo
  bind _  _(C_IO _) = internalError "can not bind IO"
  bind _  _(HO_C_IO _) = internalError "can not bind IO"
  bind cd i (Choice_C_IO d j l r) = [(ConstraintChoice d j (bind cd i l) (bind cd i r))]
  bind cd i (Choices_C_IO d j@(FreeID _ _) xs) = bindOrNarrow cd i d j xs
  bind cd i (Choices_C_IO d j@(NarrowedID _ _) xs) = [(ConstraintChoices d j (map (bind cd i) xs))]
  bind _  _ (Choices_C_IO _ i _) = internalError ("Prelude.IO.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_IO _ info) = [Unsolvable info]
  bind cd i (Guard_C_IO _ cs e) = (getConstrList cs) ++ (bind cd i e)
  lazyBind _  _ (C_IO _)            = internalError "can not lazily bind IO"
  lazyBind _  _ (HO_C_IO _)         = internalError "can not lazily bind IO"
  lazyBind cd i (Choice_C_IO d j l r) = [(ConstraintChoice d j (lazyBind cd i l) (lazyBind cd i r))]
  lazyBind cd i (Choices_C_IO d j@(FreeID _ _) xs) = lazyBindOrNarrow cd i d j xs
  lazyBind cd i (Choices_C_IO d j@(NarrowedID _ _) xs) = [(ConstraintChoices d j (map (lazyBind cd i) xs))]
  lazyBind _  _ (Choices_C_IO _ i@(ChoiceID _) _) = internalError ("Prelude.IO.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _  _ (Fail_C_IO _ info) = [Unsolvable info]
  lazyBind cd i (Guard_C_IO _ cs e) = (getConstrList cs) ++ [(i :=: (LazyBind (lazyBind cd i e)))]

instance Curry t0 => Curry (C_IO t0)
-- END GENERATED FROM PrimTypes.curry

-- Convert an IO action to a Curry IO action without converting the result.
fromIO :: IO a -> C_IO a
fromIO io = C_IO (io >>= return . Right)

instance ConvertCurryHaskell ca ha => ConvertCurryHaskell (C_IO ca) (IO ha)
  where
  toCurry io  = C_IO (io >>= return . Right . toCurry)
  fromCurry _ = internalError "C_IO.fromCurry: Use top-level search instead."

-- ---------------------------------------------------------------------------
-- Primitive data that is built-in (e.g., Handle, IORefs,...)
-- ---------------------------------------------------------------------------

-- BEGIN GENERATED FROM PrimTypes.curry
data PrimData t0
     = PrimData t0
     | Choice_PrimData Cover ID (PrimData t0) (PrimData t0)
     | Choices_PrimData Cover ID ([PrimData t0])
     | Fail_PrimData Cover FailInfo
     | Guard_PrimData Cover (Constraints) (PrimData t0)

instance Show (PrimData a) where show = internalError "show for PrimData"

instance Read (PrimData a) where readsPrec = internalError "readsPrec for PrimData"

instance NonDet (PrimData t0) where
  choiceCons = Choice_PrimData
  choicesCons = Choices_PrimData
  failCons = Fail_PrimData
  guardCons = Guard_PrimData
  try (Choice_PrimData cd i x y) = tryChoice cd i x y
  try (Choices_PrimData cd i xs) = tryChoices cd i xs
  try (Fail_PrimData cd info) = Fail cd info
  try (Guard_PrimData cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_PrimData cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_PrimData cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_PrimData cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_PrimData _ i@(ChoiceID _) _) = internalError ("Prelude.PrimData.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_PrimData cd info) = f cd info
  match _ _ _ _ f _ (Guard_PrimData cd cs e) = f cd cs e
  match _ _ _ _ _ f x = f x

instance Generable (PrimData a) where generate _ _ = internalError "generate for PrimData"

instance NormalForm (PrimData a) where
  ($!!) cont p@(PrimData _) cd cs = cont p cd cs
  ($!!) cont (Choice_PrimData d i x y) cd cs = nfChoice cont d i x y cd cs
  ($!!) cont (Choices_PrimData d i xs) cd cs = nfChoices cont d i xs cd cs
  ($!!) cont (Guard_PrimData d c x) cd cs = guardCons d c ((cont $!! x) cd $! addCs c cs)
  ($!!) _ (Fail_PrimData d info) _ _ = failCons d info
  ($##) cont p@(PrimData _) cd cs = cont p cd cs
  ($##) cont (Choice_PrimData d i x y) cd cs = gnfChoice cont d i x y cd cs
  ($##) cont (Choices_PrimData d i xs) cd cs = gnfChoices cont d i xs cd cs
  ($##) cont (Guard_PrimData d c x) cd cs = guardCons d c ((cont $## x) cd $! addCs c cs)
  ($##) _ (Fail_PrimData d info) _ _ = failCons d info
  -- no search inside argument of PrimData since it is primitive:
  searchNF _ cont (PrimData x) = cont (PrimData x)
  searchNF _ _ x = internalError ("Prelude.PrimData.searchNF: no constructor: " ++ (show x))

instance Unifiable (PrimData t0) where
  (=.=) _ _ cd _ = Fail_C_Bool cd defFailInfo
  (=.<=) _ _ cd _ = Fail_C_Bool cd  defFailInfo
  bind _  _ (PrimData _) = internalError "can not bind PrimData"
  bind cd i (Choice_PrimData d j l r) = [(ConstraintChoice d j (bind cd i l) (bind cd i r))]
  bind cd i (Choices_PrimData d j@(FreeID _ _) xs) = bindOrNarrow cd i d j xs
  bind cd i (Choices_PrimData d j@(NarrowedID _ _) xs) = [(ConstraintChoices d j (map (bind cd i) xs))]
  bind _  _ (Choices_PrimData _ i _) = internalError ("Prelude.PrimData.bind: Choices with ChoiceID: " ++ (show i))
  bind _  _ (Fail_PrimData _ info) = [Unsolvable info]
  bind cd i (Guard_PrimData _ cs e) = (getConstrList cs) ++ (bind cd i e)
  lazyBind _  _ (PrimData _) = internalError "can not lazily bind PrimData"
  lazyBind cd i (Choice_PrimData d j l r) = [(ConstraintChoice d j (lazyBind cd i l) (lazyBind cd i r))]
  lazyBind cd i (Choices_PrimData d j@(FreeID _ _) xs) = lazyBindOrNarrow cd i d j xs
  lazyBind cd i (Choices_PrimData d j@(NarrowedID _ _) xs) = [(ConstraintChoices d j (map (lazyBind cd i) xs))]
  lazyBind _  _ (Choices_PrimData _ i@(ChoiceID _) _) = internalError ("Prelude.PrimData.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _  _ (Fail_PrimData _ info) = [Unsolvable info]
  lazyBind cd i (Guard_PrimData _ cs e) = (getConstrList cs) ++ [(i :=: (LazyBind (lazyBind cd i e)))]

instance Curry (PrimData a)

-- END GENERATED FROM PrimTypes.curry

instance ConvertCurryHaskell (PrimData a) a where -- needs FlexibleInstances
  fromCurry (PrimData a) = a
  fromCurry _            = internalError "PrimData with no ground term occurred"
  toCurry a = PrimData a

-- --------------------------------------------------------------------------
-- Our own implemenation of file handles (put here since used in various
-- libraries)
-- --------------------------------------------------------------------------

-- since the operation IOExts.connectToCmd uses one handle for reading and
-- writing, we implement handles either as a single handle or two handles:
data CurryHandle = OneHandle Handle | InOutHandle Handle Handle
  deriving Eq

inputHandle :: CurryHandle -> Handle
inputHandle (OneHandle h)     = h
inputHandle (InOutHandle h _) = h

outputHandle :: CurryHandle -> Handle
outputHandle (OneHandle h)     = h
outputHandle (InOutHandle _ h) = h
