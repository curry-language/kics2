--- ----------------------------------------------------------------------------
--- This module extends the import list of a program to cover all modules
--- of which types are used in type signatures.
---
--- Motivation: The type inference for FlatCurry may infer types for expressions
--- which cannot be denoted explicitly in the program because of missing
--- imports. If these expressions are lifted to top-level functions
--- during case lifting, an invalid type signature would be generated.
---
--- @author Björn Peemöller
--- @version December 2018
--- ----------------------------------------------------------------------------

module MissingImports (fixMissingImports) where

import FlatCurry.Annotated.Types
import FlatCurry.Annotated.Goodies
import Data.Set (Set, delete, empty, insert, toList)
import Control.Monad.Trans.State

type ModuleName = String

--- The Module Monad contains all modules used in the current module.
type MM a = State (Set ModuleName) a

--- Extend the import list of an AnnotatedFlatCurry program with modules
--- of which types are used in type signatures of the program
fixMissingImports :: AProg TypeExpr -> AProg TypeExpr
fixMissingImports p@(AProg m _ ts fs os)
  = let allModules = execState (vsProg p) Data.Set.empty
    in  AProg m (toList $ delete m allModules) ts fs os

addModule :: ModuleName -> MM ()
addModule m = modify $ \ s -> insert m s

vsProg :: AProg TypeExpr -> MM ()
vsProg (AProg _ is _ fs _) = mapM_ addModule is >> mapM_ vsFuncDecl fs

vsFuncDecl :: AFuncDecl TypeExpr -> MM ()
vsFuncDecl (AFunc _ _ _ ty r) = vsTypeExpr ty >> vsRule r

vsTypeExpr :: TypeExpr -> MM ()
vsTypeExpr (TVar           _) = return ()
vsTypeExpr (FuncType ty1 ty2) = vsTypeExpr ty1 >> vsTypeExpr ty2
vsTypeExpr (TCons (m, _) tys) = addModule m >> mapM_ vsTypeExpr tys
vsTypeExpr (ForallType  _ ty) = vsTypeExpr ty

vsRule :: ARule TypeExpr -> MM ()
vsRule (ARule     _ _ e) = vsExpr e
vsRule (AExternal   _ _) = return ()

vsExpr :: AExpr TypeExpr -> MM ()
vsExpr (AVar       _ _) = return ()
vsExpr (ALit       _ _) = return ()
vsExpr (AComb _ _ _ es) = mapM_ vsExpr es
vsExpr (ALet    _ ds e) = mapM_ (vsExpr . snd) ds >> vsExpr e
vsExpr (AFree    _ _ e) = vsExpr e
vsExpr (AOr    _ e1 e2) = vsExpr e1 >> vsExpr e2
vsExpr (ACase _ _ e bs) = vsExpr e >> mapM_ (vsExpr . branchExpr) bs
-- Relevant part: Add modules of which types are used in 'ty' to set
vsExpr (ATyped  _ e ty) = vsExpr e >> vsTypeExpr ty
