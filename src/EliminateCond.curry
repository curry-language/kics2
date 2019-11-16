---------------------------------------------------------------------
--- This module provides a transformation on FlatCurry programs.
--- It replaces all calls to cond that stem from guarded expressions
--- by calls to auxiliary functions that are created by this
--- transformation.
--- A call to cond like
---
--- cond (x =:= 5) (f y (g z) + x)
---
--- where x, y, z are Variables and f,g are defined functions
--- will be transformed to a call
---
--- gen_name x y z (x =:= 5)
---
--- where gen_name is a generated function with a fresh name that
--- is defined as
---
--- gen_name x' y' z' s =
---   case s of
---     Success -> f y' (g z') + x'
---
--- This transformation can be beneficial for the optimization
--- of bindings for logic variables done by the KiCS2 compiler
---
--- @author fre
--- @version January 2012
---------------------------------------------------------------------


module EliminateCond (eliminateCond) where

import FlatCurry.Annotated.Types
import FlatCurry.Annotated.Goodies
import Control.Monad.Trans.State
import Data.List (elemIndex,nub)

-- main elimination function that eliminates cond calls in the whole
-- program
eliminateCond :: AProg TypeExpr -> AProg TypeExpr
eliminateCond p = updProgFuncs (concatMap transFunc) p

-- eliminates cond calls in a function by introducing new functions
-- that perform pattern matching. The transformed function is
-- returned along with the auxiliary functions
transFunc :: AFuncDecl TypeExpr -> [AFuncDecl TypeExpr]
transFunc f@(AFunc _ _ _ _ (AExternal _ _)) = [f]
transFunc (AFunc f k v t (ARule ty vs e)) =
  let (e', (newFuns, _)) = runState (transExpr f e) ([], 0)
  in AFunc f k v t (ARule ty vs e') : newFuns

-- the transformation of an expression is done in the state monad.
-- the state consists of the introduced auxiliary functions and
-- a counter for name generation
transExpr :: QName -> AExpr TypeExpr
          -> State ([AFuncDecl TypeExpr], Int) (AExpr TypeExpr)
transExpr f e = trExpr transVar transLit transComb transLet
                transFree transOr transCase transBranch transTyped e
 where
  -- This is where the interesting stuff happens, if a call to
  -- cond is found, the call to an auxiliary function is generated
  transVar ty v = return $ AVar ty v
  transLit ty l = return $ ALit ty l
  transComb ty cType (cName, cTy) cargs =
    case (cName, cargs) of
     (("Prelude","cond"),[cond, expr])
       -> cond >>= \newArg1 ->
          expr >>= \newArg2 ->
          makeAuxFuncCall f ty newArg1 newArg2
     _ -> sequence cargs >>= \newArgs ->
          return (AComb ty cType (cName, cTy) newArgs)
  -- for other expressions the transformation has to be done
  -- for their subexpressions
  transLet ty bindings exp =
   let (vars, exps) = unzip bindings
   in sequence exps >>= \newExps ->
      exp            >>= \newExp  ->
      return (ALet ty (zip vars newExps) newExp)
  transFree ty vars exp =
    exp >>= \newExp ->
    return (AFree ty vars newExp)
  transOr ty exp1 exp2 =
   exp1 >>= \nExp1 ->
   exp2 >>= \nExp2 ->
   return (AOr ty nExp1 nExp2)
  transCase ty ct exp bexps =
    exp >>= \nExp ->
    sequence bexps >>= \nBexps ->
    return (ACase ty ct nExp nBexps)
  transBranch pat exp = exp >>= \ newExp ->
                        return (ABranch pat newExp)
  transTyped ty exp tyExp = exp >>= \e' -> return (ATyped ty e' tyExp)


-- This function creates a new function for a call to cond.
-- it takes the two arguments of the cond call and creates
-- a new function that takes as arguments all the
-- unbound variables in the second argument of the cond call
-- and the first argument (the Success-expression).
-- The created function performs pattern matching on the
-- Success-exression and return the second argument of
-- the cond-call with all unbound variables replaced
-- by the arguments of the newly created function
-- The created function is saved in the state, the
-- call to this function is returned.
makeAuxFuncCall :: QName -> TypeExpr -> AExpr TypeExpr -> AExpr TypeExpr
                -> State ([AFuncDecl TypeExpr], Int) (AExpr TypeExpr)
makeAuxFuncCall name ty cond newBody =
  get                            >>= \(funs,idx) ->
  put (newFun idx:funs, idx + 1) >>= \_          ->
  return $ AComb ty FuncCall (mkNewName name idx, funtype)
                              (map (uncurry (flip AVar)) typedVars ++ [cond])
 where
  newFun i = AFunc (mkNewName name i) numArgs Private funtype rule
  rule =  ARule funtype argVars
            (ACase ty Flex (AVar condType numArgs)
                  [ABranch (APattern condType (trueId, boolType) [])
                          (rnmAllVars renameVarFun newBody)])

  funtype = foldr FuncType ty (map snd argVars)
  condType = annExpr cond
  typedVars = unboundVars newBody
  argVars = zip [1 ..] (map snd typedVars ++ [condType])
  numArgs = length typedVars + 1
  renameVarFun v = maybe v (+1) (elemIndex v (map fst typedVars))
  mkNewName (mod,oldName) idx = (mod, "__cond_" ++ show idx ++ "_" ++ oldName)

boolType :: TypeExpr
boolType = TCons ("Prelude", "Bool") []

trueId :: QName
trueId = ("Prelude", "True")

--- Return all variables in an expression that are unbound in the expression.
unboundVars :: AExpr TypeExpr -> [(VarIndex, TypeExpr)]
unboundVars e = nub (trExpr var lit comb leT freE oR casE branch typed e)
 where
 var ty v = [(v, ty)]
 lit  _ _ = []
 comb _ _ _ = concat
 leT _ bs vars = let (vs, es) = unzip bs
                 in filter (\v -> fst v `notElem` map fst vs) (concat (vars:es))
 freE _ vs = filter (\v -> fst v `notElem` map fst vs)
 oR   _       = (++)
 casE _ _ vars  bVars  = concat (vars:bVars)
 branch pat vars = case pat of
   (APattern _ _ vs) -> filter (\v -> fst v `notElem` map fst vs) vars
   _                 -> error "EliminateCond.unboundVars"
 typed _ vs _ = vs
