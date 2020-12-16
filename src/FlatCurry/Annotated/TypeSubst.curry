--- ----------------------------------------------------------------------------
--- Type substitutions on type-annotated AnnotatedFlatCurry
---
--- @author Bjoern Peemoeller
--- @version September 2014
--- @category meta
--- ----------------------------------------------------------------------------
module FlatCurry.Annotated.TypeSubst where

import qualified Data.Map as Map
import FlatCurry.Annotated.Types

--- The (abstract) data type for substitutions on TypeExpr.
type AFCSubst = Map.Map TVarIndex TypeExpr

showAFCSubst :: AFCSubst -> String
showAFCSubst = unlines . map showOne . Map.toList
  where showOne (k, v) = show k ++ " -> " ++ show v

--- The empty substitution
emptyAFCSubst :: AFCSubst
emptyAFCSubst = Map.empty

--- Searches the substitution for a mapping from the given variable index
--- to a term.
---
--- @param subst - the substitution to search
--- @param i - the index to search for
--- @return the found type expression or Nothing
lookupAFCSubst :: AFCSubst -> TVarIndex -> Maybe TypeExpr
lookupAFCSubst = flip Map.lookup

-- -----------------------------------------------------------------------------
-- Functions for applying substitutions to expressions
-- -----------------------------------------------------------------------------

--- Applies a substitution to a function.
---
--- @param sub - the substitution
--- @param f - the function
--- @return the function with the substitution applied
substFunc :: AFCSubst -> AFuncDecl TypeExpr -> AFuncDecl TypeExpr
substFunc sub (AFunc f a v ty r) = AFunc f a v (subst sub ty) (substRule sub r)

--- Applies a substitution to a type expression.
---
--- @param sub - the substitution
--- @param r - the rule
--- @return the rule with the substitution applied
substRule :: AFCSubst -> ARule TypeExpr -> ARule TypeExpr
substRule sub (ARule  ty vs e) = ARule (subst sub ty) (map (substSnd sub) vs)
                                       (substExpr sub e)
substRule sub (AExternal ty s) = AExternal (subst sub ty) s

--- Applies a substitution to a type expression.
---
--- @param sub - the substitution
--- @param ex - the expression
--- @return the expression with the substitution applied
substExpr :: AFCSubst -> AExpr TypeExpr -> AExpr TypeExpr
substExpr sub (AComb ty t f ps) = AComb  (subst sub ty) t (substSnd sub f)
                                         (map (substExpr sub) ps)
substExpr sub (AVar  ty      k) = AVar   (subst sub ty) k
substExpr sub (ACase ty t e bs) = ACase  (subst sub ty) t (substExpr sub e)
                                         (map (substBranch sub) bs)
substExpr sub (ALit  ty      l) = ALit   (subst sub ty) l
substExpr sub (AOr   ty    a b) = AOr    (subst sub ty) (substExpr sub a)
                                                        (substExpr sub b)
substExpr sub (ALet  ty   bs e) = ALet   (subst sub ty) (map substBinding bs)
                                         (substExpr sub e)
  where substBinding (v, b) = (substSnd sub v, substExpr sub b)
substExpr sub (AFree ty   vs e) = AFree  (subst sub ty) (map (substSnd sub) vs)
                                         (substExpr sub e)
substExpr sub (ATyped ty e ty') = ATyped (subst sub ty) (substExpr sub e)
                                                        (subst sub ty')

substSnd :: AFCSubst -> (a, TypeExpr) -> (a, TypeExpr)
substSnd sub (a, ty) = (a, subst sub ty)

--- Applies a substitution to a branch expression.
---
--- @param sub - the substitution
--- @param b - the branch
--- @return the branch with the substitution applied
substBranch :: AFCSubst -> ABranchExpr TypeExpr -> ABranchExpr TypeExpr
substBranch sub (ABranch p e) = ABranch (substPattern sub p) (substExpr sub e)

--- Applies a substitution to a pattern.
---
--- @param sub - the substitution
--- @param p - the pattern
--- @return the pattern with the substitution applied
substPattern :: AFCSubst -> APattern TypeExpr -> APattern TypeExpr
substPattern sub (APattern  t f vs) = APattern  (subst sub t) (substSnd sub f)
                                                (map (substSnd sub) vs)
substPattern sub (ALPattern t    l) = ALPattern (subst sub t) l

--- Looks up a type in a substitution and converts the resulting Term
--- to a TypeExpr. Returns a given default value if the lookup fails.
---
--- @param t - the type to look up
--- @param e - the default value
--- @param sub - the substitution to search in
--- @return either the looked-up and converted type or the default type
subst :: AFCSubst -> TypeExpr -> TypeExpr
subst sub e@(TVar        n) = maybe e id (lookupAFCSubst sub n)
subst sub (TCons     t tys) = TCons t (map (subst sub) tys)
subst sub (FuncType    a b) = FuncType (subst sub a) (subst sub b)
subst sub (ForallType ns t) = ForallType ns (subst sub t)
