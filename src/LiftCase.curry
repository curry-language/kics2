--------------------------------------------------------------------------------
--- This module provides a transformation on FlatCurry programs.
--- The result contains a new function for each nested case
--- contained in the original program.
--- The new functions have the names  "_case_ ... i" where i is a number
--- and there are as many underscores as necessary to avoid name clashes.
--- Example: _case_0.
---
--- In addition, all the cases are normalized, i.e., applied to a variable.
--- This variable is always the last argument of the newly generated function.
--- The first argument of liftCases is a boolean flag
--- with the following meaning:
---   False: introduce function for EACH case of the original program
---   True:  introduce function for nested and not normalized
---          cases only
---
--- @author  bbr,sebf, bjp
--- @version July 2013
--------------------------------------------------------------------------------

module LiftCase (isCaseAuxFuncName, liftCases) where

import FlatCurry.Annotated.Types
import FlatCurry.Annotated.Goodies
import Data.List
import Data.Maybe
import Data.Map

isCaseAuxFuncName :: String -> Bool
isCaseAuxFuncName n = take 6 n == "_case_"

--- @param - the program to be transformed
liftCases :: Bool -> AProg TypeExpr -> AProg TypeExpr
liftCases nestedOnly p =
  let fs          = progFuncs p
      aux         = genAuxName (map (snd . funcName) fs)
      (exts, ins) = partition isExternal fs
      (newFsf,_,auxFf) = foldr (liftCasesFunc nestedOnly (progName p) aux)
                               (id,0,id)
                               ins
   in updProgFuncs (const (newFsf (auxFf exts))) p

type FuncList = [AFuncDecl TypeExpr] -> [AFuncDecl TypeExpr]
type TypedVar = (VarIndex, TypeExpr)
type Result = (FuncList, Int, FuncList)

type M x = Int -> (x, Int, FuncList, [TypedVar])

sequence :: [M c] -> M [c]
sequence l i = foldr once ([],i,id,[]) l
  where
    once f (es,j,ff1,vs1) = let (e,k,ff2,vs2) = f j
                             in (e:es,k,ff1 . ff2,vs1++vs2)

liftCasesFunc :: Bool -> String -> String -> AFuncDecl TypeExpr -> Result
              -> Result
liftCasesFunc onlyNested mod aux f (esMain,i0,ffMain) =
  ((updFuncBody (const exp) f:) . esMain, iMain, ffMain . ffeMain')
  where
    body = funcBody f
    ffeMain' xs = xs ++ map (insertVars (funcType f)) (ffeMain [])
    (exp, iMain, ffeMain, _) =
      if onlyNested then case body of
        ACase ty cm (AVar ty' v) bs ->
          let (e' , i' , ffe , _) = trans (AVar ty' v) i0
              (bs', i'', ffbs, _) =
                sequence (map (\ (ABranch p be) -> branch p (trans be)) bs) i'
          in (ACase ty cm e' bs',  i'', ffe . ffbs, [])
        _                           -> trans body i0
      else trans body i0

    insertVars ty = updFunc id id id (updateType (collectVars ty)) id

    updateType vs ty = forallType ty $
      catMaybes $ map (lookupKeep vs) (nub $ allFreeVars ty)

    forallType ty vs = if Prelude.null vs then ty else ForallType vs ty

    lookupKeep vs v = fmap (\k -> (v,k)) $ Prelude.lookup v vs

    allFreeVars =
      trTypeExpr ((:[]) . fst) (const concat) (++) (flip (\\) . map fst)

    collectVars =
      trTypeExpr (const []) (const . const []) (const . const []) (++)

    trans = trExpr var lit comb leT freE or casE branch typed

    var :: TypeExpr -> VarIndex -> M (AExpr TypeExpr)
    var ty v i = (AVar ty v, i, id, [(v, ty)])

    lit :: TypeExpr -> Literal -> M (AExpr TypeExpr)
    lit ty l i = (ALit ty l, i, id, [])

    comb :: TypeExpr -> CombType -> (QName, TypeExpr) -> [M (AExpr TypeExpr)]
         -> M (AExpr TypeExpr)
    comb ty ct n args i = let (args', i', ff, vs) = sequence args i
                          in  (AComb ty ct n args', i', ff, vs)

    leT :: TypeExpr -> [((VarIndex, TypeExpr), M (AExpr TypeExpr))]
        -> M (AExpr TypeExpr) -> M (AExpr TypeExpr)
    leT ty bs e i =
      let (vs, es) = unzip bs
          (es',i',ffes,ves) = sequence es i
          (e',i'',ffe,ve) = e i'
      in (ALet ty (zip vs es') e',i'', ffes . ffe,
          filter (\v -> fst v `notElem` map fst vs) (ves ++ ve))

    freE :: TypeExpr -> [TypedVar] -> M (AExpr TypeExpr)
         -> M (AExpr TypeExpr)
    freE ty vs e i =
      let (e', i', ff, ve) = e i
      in (AFree ty vs e', i', ff, filter (\v -> fst v `notElem` map fst vs) ve)

    or :: TypeExpr -> M (AExpr TypeExpr) -> M (AExpr TypeExpr)
       -> M (AExpr TypeExpr)
    or ty e1 e2 i =
      let ([e1',e2'],i',ff,vs) = sequence [e1,e2] i
      in (AOr ty e1' e2',i',ff,vs)

    casE :: TypeExpr -> CaseType -> M (AExpr TypeExpr)
         -> [M (ABranchExpr TypeExpr)] -> M (AExpr TypeExpr)
    casE ty ct e bs i =
      let (e' , i' , ffe , ve ) = e i
          (bs', i'', ffbs, vbs) = sequence bs i'
          envRes :: [(VarIndex, TypeExpr)]
          envRes = nub (ve ++ vbs)
          env = case e' of
                  AVar ty' v -> Data.List.delete (v, ty') envRes
                  _          -> envRes
      in ( genFuncCall mod aux i'' ty env e'
         , i''+1
         , (genFunc mod aux i'' ty env e' ct bs':) . ffe . ffbs
         , envRes
         )

    branch :: APattern TypeExpr -> M (AExpr TypeExpr)
           -> M (ABranchExpr TypeExpr)
    branch p e i =
      let (e',i',ff,ve) = e i
      in (ABranch p e', i', ff, removePVars ve p)

    typed :: TypeExpr -> M (AExpr TypeExpr) -> TypeExpr -> M (AExpr TypeExpr)
    typed ty e ty' i = let (e', i', ff, ve) = e i
                       in (ATyped ty e' ty', i', ff, ve)

genFuncCall :: String -> String -> Int -> TypeExpr -> [TypedVar]
            -> AExpr TypeExpr -> AExpr TypeExpr
genFuncCall mod aux i ty env e =
  AComb ty FuncCall (newName mod aux i, funtype)
    (map (uncurry (flip AVar)) env ++ [e])
  where funtype = foldr FuncType (FuncType (annExpr e) ty) (map snd env)

genFunc :: String -> String -> Int -> TypeExpr
        -> [TypedVar] -> AExpr TypeExpr
        ->  CaseType -> [ABranchExpr TypeExpr] -> AFuncDecl TypeExpr
genFunc mod aux i ty env e ct bs =
  AFunc (newName mod aux i) (length args) Private funtype rule
  where
    ety = annExpr e
    args = env ++ [(v, ety)]
    rule = ARule funtype args (ACase ty ct (AVar ety v) bs)
    v = case e of
         AVar _ idx -> idx
         _          -> nextLocalName (map fst env ++ concatMap allVarsBranch bs)

    allVarsBranch (ABranch p pe) =
      trPattern (\ _ _ xs -> map fst xs) (\ _ _ -> []) p ++ allVars pe

    funtype = foldr FuncType ty (map snd args)

removePVars :: [TypedVar] -> APattern TypeExpr -> [TypedVar]
removePVars e = trPattern
  (\ _ _ vs -> filter (\v -> fst v `notElem` map fst vs) e)
  (\_ _ -> e)

genAuxName :: [String] -> String
genAuxName = foldl addUnderscores "_case_"

addUnderscores :: String -> String -> String
addUnderscores n m = if n `isPrefixOf` m then addUnderscores (n ++ "_") m else n

newName :: String -> String -> Int -> QName
newName m l i = (m, l ++ show i)

nextLocalName :: [VarIndex] -> VarIndex
nextLocalName vs = foldr max 0 vs + 1
