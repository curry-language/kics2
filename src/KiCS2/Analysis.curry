--- ----------------------------------------------------------------------------
--- This module contains the analysis of types, constructors and functions
---- for (non)determinism and first order/higher order.
---
--- @author  Michael Hanus, Björn Peemöller, Fabian Skrlac
--- @version December 2018
--- ----------------------------------------------------------------------------
module KiCS2.Analysis
  ( AnalysisResult (..)
  , TypeMap, initTypeMap, getTypeMap
  , Newtypes, initNewtypes, getNewtypes
  , NDResult, NDClass (..), initNDResult, analyseND
  , initHOResult
  , TypeHOResult, TypeHOClass (..), initTypeHOResult, analyseHOType
  , ConsHOResult, ConsHOClass (..), analyseHOCons
  , FuncHOResult, FuncHOClass (..), analyseHOFunc
  , Visibilities, analyzeVisibility
  , getPrivateType, getPrivateFunc, getPrivateCons
  ) where

import FlatCurry.Types
import FlatCurry.Goodies
import Data.Maybe        (fromJust, fromMaybe, mapMaybe)
import Data.List         (partition, isPrefixOf, isInfixOf)
import Data.Map as Map
import Data.Set as Set (Set, empty, union, insert, toList, fromList)
import Prelude  as P

import KiCS2.Classification
import KiCS2.Names

data AnalysisResult = AnalysisResult
  { arTypeMap      :: TypeMap
  , arNewtypes     :: Newtypes
  , arNDResult     :: NDResult
  , arHOResultType :: TypeHOResult
  , arHOResultCons :: ConsHOResult
  , arHOResultFunc :: FuncHOResult
  } deriving (Show, Read)

type Map a = Map.Map QName a

-- -----------------------------------------------------------------------------
-- Mapping from constructor names to the defining types
-- -----------------------------------------------------------------------------

-- The type map is used to lookup the type name for a given constructor
-- name to be able to add missing pattern matching alternatives like
-- Choice_<TypeName> etc.
-- This could also be done by inspecting the type signature of the respective
-- function, but it may not be accurate for various reasons.

type TypeMap = Map QName

initTypeMap :: TypeMap
initTypeMap = Map.fromList primTypes

--- List of constructors of known primitive types.
primTypes :: [(QName, QName)]
primTypes = map (\(x, y) -> ( renameQName (prelude, x)
                            , renameQName (prelude, y)
                            )) $
  [ ("True", "Bool"), ("False", "Bool"), ("Int", "Int")
  , ("Float", "Float"), ("Char", "Char")
  ]

--- Register the types names of constructors to be able to retrieve
--- the types for constructors used in pattern matching.
--- May be needless now because the case lifting now also creates correct types.
getTypeMap :: [TypeDecl] -> TypeMap
getTypeMap ts = Map.fromList
              $ concatMap extractConsNames
              $ ts
  where
    extractConsNames :: TypeDecl -> [(QName, QName)]
    extractConsNames t = case t of
      Type    qn _ _ cs -> map (\c -> (consName c, qn)) cs
      TypeNew qn _ _ c  -> [(newConsName c, qn)]
      _                 -> []

-- -----------------------------------------------------------------------------
-- Set of newtypes
-- -----------------------------------------------------------------------------

-- The set of newtypes is needed by TransFunctions during the generation
-- of type declarations, since higher-order newtypes use a separate type
-- in nondeterministic functions.

type Newtypes = Set.Set QName

initNewtypes :: Newtypes
initNewtypes = Set.empty

getNewtypes :: [TypeDecl] -> Newtypes
getNewtypes = Set.fromList . mapMaybe extractNewtypeName
  where
    extractNewtypeName :: TypeDecl -> Maybe QName
    extractNewtypeName t = case t of
      TypeNew qn _ _ _ -> Just qn
      _                -> Nothing

-- -----------------------------------------------------------------------------
-- Analysis using fix-point iteration
-- -----------------------------------------------------------------------------

type Analysis t a = Map a -> (t, [QName]) -> (QName, a)

fullIteration :: Eq a => Analysis t a -> [(t, [QName])] -> Map a -> Map a -> Map a
fullIteration analyze calls env start =
  let after = Map.fromList $ map (analyze (env `Map.union` start)) calls
  in if (start == after)
         then start
         else fullIteration analyze calls env after

-- -----------------------------------------------------------------------------
-- (Non-)Determinism analysis for functions.
-- (Note that Type constructors and data constructors are deterministic.)
-- -----------------------------------------------------------------------------

type NDResult = Map NDClass

--- Initial start value for the non-determinism analysis.
initNDResult :: NDResult
initNDResult = Map.fromList [(qmark, ND)]

--- Analyse a module for non-determinism using the information
--- for imported modules analysed before.
analyseND :: Prog -> NDResult -> NDResult
analyseND p importedInfo =
  let fs = progFuncs p
      start = Map.fromList $ map initValue fs
  in  fullIteration ndFunc (map getFunctionCalls fs) importedInfo start
  where
    initValue f =
      let name = funcName f
      in (name, if name == qmark || isDictWithArrow name then ND else D)
    getFunctionCalls f = (f, funcCalls f)
    -- we classify some typeclass functions involving (->) as ND,
    -- to avoid having to give a D version as that is not possible currently.
    isDictWithArrow (_, name) =
      "hash_lparen_minus_gt_rparen" `isInfixOf` name &&
      any (`isPrefixOf` name) dictNames
    dictNames = ["OP_uscore_impl_hash", "uscore_inst_hash"]

--- Analysis function for non-determinism analysis.
ndFunc:: Analysis FuncDecl NDClass
ndFunc ndInfo (f, called)
  | isRuleExternal rule      = dflt
  | isNDExpr (ruleBody rule) = (name, ND)
  | callsND                  = (name, ND)
  | otherwise                = dflt
  where
    name    = funcName f
    rule    = funcRule f
    callsND = any (== Just ND) $ map (`Map.lookup` ndInfo) called
    dflt    = (name, fromJust $ Map.lookup name ndInfo)

--- Check whether an expression is non-deterministic, i.e.,
--- whether it uses an OR (overlapping rule) or FREE (free variables).
isNDExpr :: Expr -> Bool
isNDExpr = trExpr cf cf combf letf freef orf casef branchf typedf
  where
    cf               x = const False x -- (variable / literal)
    combf    _ _ isNDs = or isNDs
    letf ndBinds ndExp = or (ndExp : map snd ndBinds)
    freef         vs _ = not (P.null vs)
    orf            _ _ = True
    casef       _ e bs = or (e : bs)
    branchf        _ e = e
    typedf         e _ = e

--- list of direct dependencies for a function
funcCalls :: FuncDecl -> [QName]
funcCalls (Func _ _ _ _ (Rule   _ e)) = Set.toList $ funcsInExp e
funcCalls (Func _ _ _ _ (External _)) = []

--- Gets the set of all functions (including partially applied functions)
--- directly called in an expression.
funcsInExp :: Expr -> Set.Set QName
funcsInExp (Var        _) = Set.empty
funcsInExp (Lit        _) = Set.empty
funcsInExp (Comb ct f es)
  | isFuncCall ct = f `Set.insert` unionMap funcsInExp es
  | otherwise     =                unionMap funcsInExp es
funcsInExp (Free     _ e) = funcsInExp e
funcsInExp (Let     bs e) = unionMap funcsInExp (e : map snd bs)
funcsInExp (Or     e1 e2) = funcsInExp e1 `Set.union` funcsInExp e2
funcsInExp (Case  _ e bs) = unionMap funcsInExp (e : map branchExpr bs)
funcsInExp (Typed    e _) = funcsInExp e

--- Is a combination a function call?
isFuncCall :: CombType -> Bool
isFuncCall ct = case ct of
  FuncCall       -> True
  FuncPartCall _ -> True
  _              -> False

-- -----------------------------------------------------------------------------
-- (first/higher)-order analysis of types and type constructors
-- -----------------------------------------------------------------------------

type TypeHOResult = Map TypeHOClass

initTypeHOResult :: TypeHOResult
initTypeHOResult = Map.fromList externalTypes

externalTypes :: [(QName, TypeHOClass)]
externalTypes = [(ioType, TypeIO), (successType, TypeFO)]

getHOResult :: QName -> TypeHOResult -> TypeHOClass
getHOResult qn hoResult = fromMaybe TypeFO (qn `Map.lookup` hoResult)

--- Analyse a module for the higher-order classification of types and type
--- constructors using the information for imported modules analysed before.
analyseHOType :: Prog -> TypeHOResult -> TypeHOResult
analyseHOType p importedInfo =
  let types = progTypes p
      start = Map.fromList $ map initValue types
  in  fullIteration hoType (map getUsedTypes types) importedInfo start
  where
    initValue    t = (typeName t, classifyHOTypeDecl t)
    getUsedTypes t = (t, usedTypes t)

hoType :: Analysis TypeDecl TypeHOClass
hoType typeInfo (t, deps) = (typeName t, maximumTypeHOClass depClasses)
  where depClasses = map (`getHOResult` typeInfo) (typeName t : deps)

classifyHOTypeDecl :: TypeDecl -> TypeHOClass
classifyHOTypeDecl (Type   qn _ _ cs)
  | qn == ioType = TypeIO
  | otherwise    = maximumTypeHOClass
                 $ map classifyHOType
                 $ concatMap consArgs cs
classifyHOTypeDecl (TypeSyn _ _ _ ty) = classifyHOType ty
classifyHOTypeDecl (TypeNew _ _ _ c)  = classifyHOType $ newConsArg c

classifyHOType :: TypeExpr -> TypeHOClass
classifyHOType (TVar       _) = TypeFO
classifyHOType (FuncType _ _) = TypeHO
classifyHOType (TCons qn tys)
  | qn == ioType = maximumTypeHOClass (TypeIO : map classifyHOType tys)
  | otherwise    = maximumTypeHOClass (map classifyHOType tys)
classifyHOType (ForallType _ t) = classifyHOType t

usedTypes :: TypeDecl -> [QName]
usedTypes (Type    _ _ _ cs) = Set.toList $ unionMap   typeCons
                                          $ concatMap  consArgs cs
usedTypes (TypeSyn _ _ _ ty) = Set.toList $ typeCons   ty
usedTypes (TypeNew _ _ _ c)  = Set.toList $ typeCons $ newConsArg c

typeCons :: TypeExpr -> Set.Set QName
typeCons (TVar       _) = Set.empty
typeCons (FuncType a b) = typeCons a `Set.union` typeCons b
typeCons (TCons qn tys) = qn `Set.insert` unionMap typeCons tys
typeCons (ForallType _ ty) = typeCons ty

-- -----------------------------------------------------------------------------
-- (first/higher)-order analysis of data constructors
-- -----------------------------------------------------------------------------

type ConsHOResult = Map ConsHOClass

initHOResult :: Map a
initHOResult = Map.empty

analyseHOCons :: Prog -> ConsHOResult
analyseHOCons p = Map.fromList $ externals ++ internals
  where
  externals = filter ((== progName p) . fst . fst) externalCons
  internals = concatMap extractConsOrders $ progTypes p
  
externalCons :: [(QName, ConsHOClass)]
externalCons = [(successType, ConsFO)]

extractConsOrders :: TypeDecl -> [(QName, ConsHOClass)]
extractConsOrders t = case t of
  Type    _ _ _ cs -> map consOrder cs
  TypeNew _ _ _ c  -> [newConsOrder c]
  _                -> []

consOrder :: ConsDecl -> (QName, ConsHOClass)
consOrder (Cons qn _ _ tys) = (qn, cls)
  where cls = typeToConsHOClass $ maximumTypeHOClass (map classifyHOType tys)

newConsOrder :: NewConsDecl -> (QName, ConsHOClass)
newConsOrder (NewCons qn _ ty) = (qn, cls)
  where cls = typeToConsHOClass $ classifyHOType ty

-- -----------------------------------------------------------------------------
-- (first/higher)-order analysis of functions
-- -----------------------------------------------------------------------------

type FuncHOResult = Map FuncHOClass

analyseHOFunc :: Prog -> TypeHOResult -> FuncHOResult
analyseHOFunc p typeInfo = Map.fromList $ map analyse (progFuncs p)
 where
  analyse f = (funcName f,
               isHOFunc (funcName f) typeInfo (funcArity f) (funcType f))

-- Determines if a function is higher order.
-- In our context, a function with arity n is higher order (HO),
-- if one of its arguments has a higher order type.
-- If the result is a m-ary function type and neither
-- the argument types nor the result type is a higher order type,
-- then the function has a higher order result with arity m (HORes m)
-- Otherwise, it is first order (FO)
isHOFunc :: QName -> TypeHOResult -> Int -> TypeExpr -> FuncHOClass
isHOFunc name typeInfo arity ty
  | arity == 0 = case reverse (splitFuncType ty) of
      []        -> error "Analysis.isHOFunc: no type"
      (lty:tys) -> maximumFuncHOClass
                     (initVal : map (isHOType True typeInfo) tys)
        where
          initVal = maxFuncHOClass
                      (if P.null tys then FuncFO else FuncHORes (length tys))
                      (isHOType False typeInfo lty)
  | otherwise  = case ty of
      FuncType x y     -> maxFuncHOClass (isHOType True typeInfo x)
                                         (isHOFunc name typeInfo (arity - 1) y)
      ForallType _ ty' -> isHOFunc name typeInfo arity ty'
      _                -> error $ "Analysis.isHOFunc " ++ show name ++ "/" ++
                                  show arity ++ ": " ++ show ty

--- Determines if a type expression involves a function type (->)
--- or a type that has a constructor which involves a function type.
isHOType :: Bool -> TypeHOResult -> TypeExpr -> FuncHOClass
isHOType ioAsHo typeInfo ty = case ty of
  TVar _          -> FuncFO
  FuncType   _ _  -> FuncHO
  TCons    qn tys -> maximumFuncHOClass $
                        typeToFuncHOClass ioAsHo (getHOResult qn typeInfo) :
                          map (isHOType ioAsHo typeInfo) tys
  ForallType _ t  -> isHOType ioAsHo typeInfo t

--- splits a function type into the type expressions
--- of the arguments and the result.
splitFuncType :: TypeExpr -> [TypeExpr]
splitFuncType t@(TVar         _) = [t]
splitFuncType (FuncType   at rt) = at : splitFuncType rt
splitFuncType t@(TCons      _ _) = [t]
splitFuncType t@(ForallType _ _) = [t]

-- -----------------------------------------------------------------------------
-- Visibility analysis
-- -----------------------------------------------------------------------------

data Visibilities = Vis ([QName],[QName]) ([QName],[QName]) ([QName],[QName])

getPrivateFunc :: Visibilities -> [QName]
getPrivateFunc (Vis (_, priv) _ _) = priv

getPrivateType :: Visibilities -> [QName]
getPrivateType (Vis _ (_, priv) _) = priv

getPrivateCons :: Visibilities -> [QName]
getPrivateCons (Vis _ _ (_, priv)) = priv

analyzeVisibility :: Prog -> Visibilities
analyzeVisibility p =
  Vis (splitVisibleFuncs (progFuncs p))
      (splitVisibleTypes types)
      (splitVisibleCons types)
 where
  types = progTypes p

splitVisibleFuncs :: [FuncDecl] -> ([QName],[QName])
splitVisibleFuncs funcs =
  let (pubs, privs) =  partition (\f -> funcVisibility f == Public) funcs
  in  (map funcName pubs, map funcName privs)

splitVisibleTypes :: [TypeDecl] -> ([QName],[QName])
splitVisibleTypes types =
  let (pubs, privs) = partition (\t -> typeVisibility t == Public) types
  in  (map typeName pubs, map typeName privs)

splitVisibleCons  :: [TypeDecl] -> ([QName],[QName])
splitVisibleCons = splitVisibleCons' . concatMap extractCons
  where
    extractCons :: TypeDecl -> [(Visibility, QName)]
    extractCons t = case t of
      Type    _ _ _ cs -> map (\c -> (consVisibility c, consName c)) cs
      TypeNew _ _ _ c  -> [(newConsVisibility c, newConsName c)]
      _                -> []
    splitVisibleCons' :: [(Visibility, QName)] -> ([QName], [QName])
    splitVisibleCons' cs = let (pubs, privs) = partition (\(v, _) -> v == Public) cs
                           in  (map snd pubs, map snd privs)

-- -----------------------------------------------------------------------------
-- Special Identifiers
-- -----------------------------------------------------------------------------

qmark :: QName
qmark = renameQName (prelude, "?")

successType :: QName
successType = renameQName (prelude, "Success")

ioType :: QName
ioType = renameQName (prelude, "IO")

unionMap :: Ord b => (a -> Set.Set b) -> [a] -> Set.Set b
unionMap f = foldr Set.union Set.empty . map f
