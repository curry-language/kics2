--- --------------------------------------------------------------------------
--- Various utility functions
---
--- This module should be further divided if it contains too much unrelated
--- things.
---
--- @author  Bjoern Peemoeller, Finn Teegen
--- @version September 2024
--- --------------------------------------------------------------------------
module KiCS2.Utils
  ( showMonoTypeExpr, showMonoQualTypeExpr
  , notNull, strip, lpad, rpad
  ) where

import AbstractCurry.Types
import Data.List           (intercalate)
import Data.Char           (isSpace)


--------------------------------------------------------------------------
--- Shows an AbstractCurry type expression in standard Curry syntax.
--- If the first argument is True, all occurrences of type variables
--- are replaced by "()".
showMonoQualTypeExpr :: Bool -> CQualTypeExpr -> String
showMonoQualTypeExpr mono (CQualType cx ty) =
  showContext mono cx ++ showMonoTypeExpr mono ty

--- Shows an AbstractCurry context in standard Curry syntax.
--- If the first argument is True, no context is shown.
showContext :: Bool -> CContext -> String
showContext False (CContext cs)
  | null cs
  = ""
  | otherwise
  = parens (length cs > 1) (intercalate ", " (map showConstraint cs)) ++ " => "
showContext True  _             = ""

--- Shows an AbstractCurry constraint in standard Curry syntax.
showConstraint :: CConstraint -> String
showConstraint ((_, name), ts) = unwords $
  showIdentifier name : map (showMonoTypeExpr' False 2) ts

--- Shows an AbstractCurry type expression in standard Curry syntax.
--- If the first argument is True, all occurrences of type variables
--- are replaced by "()".
showMonoTypeExpr :: Bool -> CTypeExpr -> String
showMonoTypeExpr mono ty = showMonoTypeExpr' mono 0 ty

showMonoTypeExpr' :: Bool -> Int -> CTypeExpr -> String
showMonoTypeExpr' mono _ (CTVar             (_,name)) =
  if mono then "()" else showIdentifier name
showMonoTypeExpr' mono p (CFuncType     domain range) = parens (p > 0) $
  showMonoTypeExpr' mono 1 domain ++ " -> " ++ showMonoTypeExpr' mono 0 range
showMonoTypeExpr' _    _ (CTCons            (_,name)) = name
showMonoTypeExpr' mono p texp@(CTApply     tcon targ) = maybe
  (parens (p > 1) $ showMonoTypeExpr' mono 2 tcon ++ " " ++
                    showMonoTypeExpr' mono 2 targ)
  (\ (mname,name) -> parens (p > 0) $
                     showTypeCons mono mname name (argsOfApply texp))
  (funOfApply texp)
 where
  funOfApply te = case te of CTApply (CTCons qn) _ -> Just qn
                             CTApply tc _          -> funOfApply tc
                             _                     -> Nothing
  argsOfApply te = case te of
    CTApply (CTCons _) ta -> [ta]
    CTApply tc         ta -> argsOfApply tc ++ [ta]
    _                     -> []

showTypeCons :: Bool -> String -> String -> [CTypeExpr] -> String
showTypeCons _    _   name []       = name
showTypeCons mono mname name ts@(_:_)
  | mname == "Prelude" = showPreludeTypeCons mono name ts
  | otherwise          = name ++ prefixMap (showMonoTypeExpr' mono 2) ts " "

showPreludeTypeCons :: Bool -> String -> [CTypeExpr] -> String
showPreludeTypeCons mono name typelist
  | name == "[]" && head typelist == CTCons (pre "Char")
  = "String"
  | name == "[]"
  = "[" ++ showMonoTypeExpr' mono 0 (head typelist) ++ "]"
  | isTuple name
  = "(" ++ combineMap (showMonoTypeExpr' mono 0) typelist "," ++ ")"
  | otherwise
  = name ++ prefixMap (showMonoTypeExpr' mono 2) typelist " "

-- Remove characters '<' and '>' from identifiers since these characters
-- are sometimes introduced in new identifiers generated by the front end
-- (for sections)
showIdentifier :: String -> String
showIdentifier = filter (`notElem` "<>")

-- enclose string with parentheses if required by first argument
parens :: Bool -> String -> String
parens True  s = '(' : s ++ ")"
parens False s = s

prefixMap :: (a -> [b]) -> [a] -> [b] -> [b]
prefixMap f xs s = concatMap (s ++) (map f xs)

combineMap :: (a -> [b]) -> [a] -> [b] -> [b]
combineMap _ []     _ = []
combineMap f (x:xs) s = f x ++ prefixMap f xs s

isTuple :: String -> Bool
isTuple []     = False
isTuple (x:xs) = x == '(' && p1_isTuple xs
  where
  p1_isTuple []         = False
  p1_isTuple (z:[])     = z == ')'
  p1_isTuple (z1:z2:zs) = z1 == ',' && p1_isTuple (z2:zs)

---------------------------------------------------------------------------

notNull :: [a] -> Bool
notNull = not . null

--- Remove leading and trailing whitespace
strip :: String -> String
strip = reverse . dropWhile isSpace . reverse . dropWhile isSpace

--- Extend a String to a given minimal length by adding *leading* spaces.
lpad :: Int -> String -> String
lpad n s = replicate (n - length s) ' ' ++ s

--- Extend a String to a given minimal length by adding *trailing* spaces.
rpad :: Int -> String -> String
rpad n s = s ++ replicate (n - length s) ' '

---------------------------------------------------------------------------
