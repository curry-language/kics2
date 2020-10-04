--- --------------------------------------------------------------------------
--- This module defines how Curry contexts are generated in Haskell code
--- during the translation of type signatures in function, data and instance
--- declarations.
---
--- @author Kai-Oliver Prott, Fredrik Wieczerkowski
--- @version October 2020
--- --------------------------------------------------------------------------
module GenContext (genContext, mkContext) where

import Data.List (nub, partition)

import qualified AbstractHaskell.Types as AH
import Names

-- Note: all types are fully quantified and in weak prenex form.
-- To generate Curry contexts, we pass down the kind of bound type
-- variables and pass up type variables from the child TypeExprs together
-- with their kinds and the updated TypeExpr.
--
-- When traversing back up, we add Curry contexts to ForallTypes
-- as far up the TypeExpr as possible while still having all
-- required type variables in scope.
genContext :: [(AH.TVarIName, AH.Kind)] -> AH.TypeExpr -> AH.TypeExpr
genContext bvs = snd . toTypeSig' bvs
  where
    toTypeSig' :: [(AH.TVarIName, AH.Kind)] -> AH.TypeExpr -> ([(AH.TypeExpr, AH.Kind)], AH.TypeExpr)
    toTypeSig' vs (AH.FuncType ty1 ty2) =
      let (cty1, ty1') = toTypeSig' vs ty1
          (cty2, ty2') = toTypeSig' vs ty2
      in (cty1++cty2, AH.FuncType ty1' ty2')
    toTypeSig' vs (AH.ForallType tvs cs ty) =
      let vs' = vs ++ tvs
          (cty, ty') = toTypeSig' vs' ty
          (here, before) = partition (isSaturatedWith tvs . fst) $ nub cty
      in (before, AH.ForallType tvs (nub $ cs ++ map (uncurry mkContext) here) ty')
    toTypeSig' vs t@(AH.TVar tv) = case Prelude.lookup tv vs of
      Just kind -> ([(t, kind)], t)
      Nothing   -> ([],          t)
    toTypeSig' vs t@(AH.TCons qname tys) =
      let (ctys, tys') = unzip $ map (toTypeSig' vs) tys
      in (concat ctys, AH.TCons qname tys')

    -- if any TypeVar in ty is quantified by vs, we have to add a Context for ty
    isSaturatedWith vs ty = any (elemFst vs) $ nub $ typeVars ty []

    typeVars (AH.TVar tv) vs = tv:vs
    typeVars (AH.TCons _ tys) vs = foldr typeVars vs tys
    typeVars (AH.ForallType _ _ ty) vs = typeVars ty vs
    typeVars (AH.FuncType ty1 ty2) vs = typeVars ty1 (typeVars ty2 vs)

    elemFst []         _ = False
    elemFst ((x,_):xs) e = x == e || elemFst xs e

    isTypeVar ty = case ty of
      AH.TVar _ -> True
      _         -> False

-- The Curry contexts are generated based on the type variable's
-- kind by using the QuantifiedConstraints language extension.
-- Consider the following examples (with simplified type variable names):
--
-- Curry a                                             when a :: *
-- forall x. Curry x => Curry (C_Apply a x)            when a :: * -> *
-- forall x y. ( Curry x
--             , Curry y
--             ) => Curry (C_Apply (C_Apply a x) y)    when a :: * -> * -> *
-- forall x y. ( forall z. Curry z => (C_Apply x z)
--             , Curry y
--             ) => Curry (C_Apply (C_Apply a x) y)    when a :: (* -> *) * -> -> *
mkContext :: AH.TypeExpr -> AH.Kind -> AH.Context
mkContext = snd . mkContext' 0
  where
    -- The integer tracks the fresh type variable index.
    mkContext' :: Int -> AH.TypeExpr -> AH.Kind -> (Int, AH.Context)
    mkContext' n ty kind = case kind of
      AH.KindStar        -> (n,   AH.Context []     []       (curryPrelude, "Curry") [ty])
      AH.KindArrow k1 k2 -> (n'', AH.Context (v:vs) (cx:cxs) (curryPrelude, "Curry") [ty'])
        where
          arityKind AH.KindStar        = 0
          arityKind (AH.KindArrow _ k) = arityKind k + 1

          arity     = arityKind kind
          (v:vs)    = map (\i -> (i, 'x':(show i))) $ take arity [n ..]
          n'        = n + arity
          ty'       = foldl (\t tv -> AH.TCons (curryPrelude, "C_Apply") [t, AH.TVar tv]) ty (v:vs)
          (n'', cx) = mkContext' n' (AH.TVar v) k1
          cxs       = map (\tv -> snd $ mkContext' n' (AH.TVar tv) AH.KindStar) $ vs
