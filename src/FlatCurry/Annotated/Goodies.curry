--------------------------------------------------------------------------------
--- This library provides selector functions, test and update operations
--- as well as some useful auxiliary functions for FlatCurry data terms.
--- Most of the provided functions are based on general transformation
--- functions that replace constructors with user-defined
--- functions. For recursive datatypes the transformations are defined
--- inductively over the term structure. This is quite usual for
--- transformations on FlatCurry terms,
--- so the provided functions can be used to implement specific transformations
--- without having to explicitly state the recursion. Essentially, the tedious
--- part of such transformations - descend in fairly complex term structures -
--- is abstracted away, which hopefully makes the code more clear and brief.
---
--- @author Sebastian Fischer, Bjoern Peemoeller
--- @version October 2015
--- @category meta
--------------------------------------------------------------------------------

module FlatCurry.Annotated.Goodies where

import FlatCurry.Annotated.Types
import qualified FlatCurry.Types as FC

type Update a b = (b -> b) -> a -> a

-- Prog ------------------------------------------------------------------------

--- transform program
trProg :: (String -> [String] -> [TypeDecl] -> [AFuncDecl a] -> [OpDecl] -> b)
       -> AProg a -> b
trProg f (AProg name imps types funcs ops) = f name imps types funcs ops

-- Selectors

--- get name from program
progName :: AProg _ -> String
progName = trProg (\name _ _ _ _ -> name)

--- get imports from program
progImports :: AProg _ -> [String]
progImports = trProg (\_ imps _ _ _ -> imps)

--- get type declarations from program
progTypes :: AProg _ -> [TypeDecl]
progTypes = trProg (\_ _ types _ _ -> types)

--- get functions from program
progFuncs :: AProg a -> [AFuncDecl a]
progFuncs = trProg (\_ _ _ funcs _ -> funcs)

--- get infix operators from program
progOps :: AProg _ -> [OpDecl]
progOps = trProg (\_ _ _ _ ops -> ops)

-- Update Operations

--- update program
updProg :: (String        -> String)
        -> ([String]      -> [String])
        -> ([TypeDecl]    -> [TypeDecl])
        -> ([AFuncDecl a] -> [AFuncDecl a])
        -> ([OpDecl]      -> [OpDecl])
        -> AProg a -> AProg a
updProg fn fi ft ff fo = trProg prog
 where
  prog name imps types funcs ops
    = AProg (fn name) (fi imps) (ft types) (ff funcs) (fo ops)

--- update name of program
updProgName :: Update (AProg _) String
updProgName f = updProg f id id id id

--- update imports of program
updProgImports :: Update (AProg _) [String]
updProgImports f = updProg id f id id id

--- update type declarations of program
updProgTypes :: Update (AProg _) [TypeDecl]
updProgTypes f = updProg id id f id id

--- update functions of program
updProgFuncs :: Update (AProg a) [AFuncDecl a]
updProgFuncs f = updProg id id id f id

--- update infix operators of program
updProgOps :: Update (AProg _) [OpDecl]
updProgOps = updProg id id id id

-- Auxiliary Functions

--- get all program variables (also from patterns)
allVarsInProg :: AProg _ -> [VarIndex]
allVarsInProg = concatMap allVarsInFunc . progFuncs

--- lift transformation on expressions to program
updProgExps :: Update (AProg a) (AExpr a)
updProgExps = updProgFuncs . map . updFuncBody

--- rename programs variables
rnmAllVarsInProg :: Update (AProg _) VarIndex
rnmAllVarsInProg = updProgFuncs . map . rnmAllVarsInFunc

--- update all qualified names in program
updQNamesInProg :: Update (AProg _) QName
updQNamesInProg f = updProg id id
  (map (updQNamesInType f)) (map (updQNamesInFunc f)) (map (updOpName f))

--- rename program (update name of and all qualified names in program)
rnmProg :: String -> AProg a -> AProg a
rnmProg name p = updProgName (const name) (updQNamesInProg rnm p)
 where
  rnm (mod,n) | mod==progName p = (name,n)
              | otherwise = (mod,n)

-- TypeDecl ------------------------------------------------------------------

-- Selectors

--- transform type declaration
trType :: (QName -> Visibility -> [(TVarIndex, Kind)] -> [ConsDecl]  -> a) ->
          (QName -> Visibility -> [(TVarIndex, Kind)] -> TypeExpr    -> a) ->
          (QName -> Visibility -> [(TVarIndex, Kind)] -> NewConsDecl -> a) -> TypeDecl -> a
trType typ _ _     (Type    name vis params  cs) = typ name vis params cs
trType _ typesyn _ (TypeSyn name vis params syn) = typesyn name vis params syn
trType _ _ typenew (TypeNew name vis params   c) = typenew name vis params c

--- get name of type declaration
typeName :: TypeDecl -> QName
typeName = trType (\name _ _ _ -> name) (\name _ _ _ -> name) (\name _ _ _ -> name)

--- get visibility of type declaration
typeVisibility :: TypeDecl -> Visibility
typeVisibility = trType (\_ vis _ _ -> vis) (\_ vis _ _ -> vis) (\_ vis _ _ -> vis)

--- get type parameters of type declaration
typeParams :: TypeDecl -> [(TVarIndex, Kind)]
typeParams = trType (\_ _ params _ -> params) (\_ _ params _ -> params) (\_ _ params _ -> params)

--- get constructor declarations from type declaration
typeConsDecls :: TypeDecl -> [ConsDecl]
typeConsDecls = trType (\_ _ _ cs -> cs) failed failed

--- get synonym of type declaration
typeSyn :: TypeDecl -> TypeExpr
typeSyn = trType failed (\_ _ _ syn -> syn) failed

--- is type declaration a basic data type?
isTypeData :: TypeDecl -> Bool
isTypeData = trType (\_ _ _ _ -> True) (\_ _ _ _ -> False) (\_ _ _ _ -> False)

--- is type declaration a type synonym?
isTypeSyn :: TypeDecl -> Bool
isTypeSyn = trType (\_ _ _ _ -> False) (\_ _ _ _ -> True) (\_ _ _ _ -> False)

--- is type declaration a newtype?
isTypeNew :: TypeDecl -> Bool
isTypeNew = trType (\_ _ _ _ -> False) (\_ _ _ _ -> False) (\_ _ _ _ -> True)

-- Update Operations

--- update type declaration
updType :: (QName -> QName) ->
           (Visibility -> Visibility) ->
           ([(TVarIndex, Kind)] -> [(TVarIndex, Kind)]) ->
           ([ConsDecl] -> [ConsDecl]) ->
           (NewConsDecl -> NewConsDecl) ->
           (TypeExpr -> TypeExpr)     -> TypeDecl -> TypeDecl
updType fn fv fp fc fnc fs = trType typ typesyn typenew
 where
  typ name vis params cs = Type (fn name) (fv vis) (fp params) (fc cs)
  typesyn name vis params syn = TypeSyn (fn name) (fv vis) (fp params) (fs syn)
  typenew name vis params nc = TypeNew (fn name) (fv vis) (fp params) (fnc nc)

--- update name of type declaration
updTypeName :: Update TypeDecl QName
updTypeName f = updType f id id id id id

--- update visibility of type declaration
updTypeVisibility :: Update TypeDecl Visibility
updTypeVisibility f = updType id f id id id id

--- update type parameters of type declaration
updTypeParams :: Update TypeDecl [(TVarIndex, Kind)]
updTypeParams f = updType id id f id id id

--- update constructor declarations of type declaration
updTypeConsDecls :: Update TypeDecl [ConsDecl]
updTypeConsDecls f = updType id id id f id id

--- update newtype constructor declaration of type declaration
updTypeNewConsDecl :: Update TypeDecl NewConsDecl
updTypeNewConsDecl f = updType id id id id f id

--- update synonym of type declaration
updTypeSynonym :: Update TypeDecl TypeExpr
updTypeSynonym = updType id id id id id

-- Auxiliary Functions

--- update all qualified names in type declaration
updQNamesInType :: Update TypeDecl QName
updQNamesInType f
  = updType f id id (map (updQNamesInConsDecl f)) (updQNamesInNewConsDecl f) (updQNamesInTypeExpr f)

-- ConsDecl ------------------------------------------------------------------

-- Selectors

--- transform constructor declaration
trCons :: (QName -> Int -> Visibility -> [TypeExpr] -> a) -> ConsDecl -> a
trCons cons (Cons name arity vis args) = cons name arity vis args

--- get name of constructor declaration
consName :: ConsDecl -> QName
consName = trCons (\name _ _ _ -> name)

--- get arity of constructor declaration
consArity :: ConsDecl -> Int
consArity = trCons (\_ arity _ _ -> arity)

--- get visibility of constructor declaration
consVisibility :: ConsDecl -> Visibility
consVisibility = trCons (\_ _ vis _ -> vis)

--- get arguments of constructor declaration
consArgs :: ConsDecl -> [TypeExpr]
consArgs = trCons (\_ _ _ args -> args)

-- Update Operations

--- update constructor declaration
updCons :: (QName -> QName) ->
           (Int -> Int) ->
           (Visibility -> Visibility) ->
           ([TypeExpr] -> [TypeExpr]) -> ConsDecl -> ConsDecl
updCons fn fa fv fas = trCons cons
 where
  cons name arity vis args = Cons (fn name) (fa arity) (fv vis) (fas args)

--- update name of constructor declaration
updConsName :: Update ConsDecl QName
updConsName f = updCons f id id id

--- update arity of constructor declaration
updConsArity :: Update ConsDecl Int
updConsArity f = updCons id f id id

--- update visibility of constructor declaration
updConsVisibility :: Update ConsDecl Visibility
updConsVisibility f = updCons id id f id

--- update arguments of constructor declaration
updConsArgs :: Update ConsDecl [TypeExpr]
updConsArgs = updCons id id id

-- Auxiliary Functions

--- update all qualified names in constructor declaration
updQNamesInConsDecl :: Update ConsDecl QName
updQNamesInConsDecl f = updCons f id id (map (updQNamesInTypeExpr f))

-- NewConsDecl ------------------------------------------------------------------

--- transform newtype constructor declaration
trNewCons :: (QName -> Visibility -> TypeExpr -> a) -> NewConsDecl -> a
trNewCons cons (NewCons name vis arg) = cons name vis arg

-- get argument of newtype constructor declaration
newConsArg :: NewConsDecl -> TypeExpr
newConsArg = trNewCons (\_ _ arg -> arg)

-- get name of newtype constructor declaration
newConsName :: NewConsDecl -> QName
newConsName = trNewCons (\name _ _ -> name)

-- get visibility of newtype constructor declaration
newConsVisibility :: NewConsDecl -> Visibility
newConsVisibility = trNewCons (\_ vis _ -> vis)

-- Update Operations

--- update newtype constructor declaration
updNewCons :: (QName -> QName) ->
              (Visibility -> Visibility) ->
              (TypeExpr -> TypeExpr) -> NewConsDecl -> NewConsDecl
updNewCons fn fv fas = trNewCons newcons
 where
  newcons name vis args = NewCons (fn name) (fv vis) (fas args)

--- update name of newtype constructor declaration
updNewConsName :: Update NewConsDecl QName
updNewConsName f = updNewCons f id id

--- update visibility of newtype constructor declaration
updNewConsVisibility :: Update NewConsDecl Visibility
updNewConsVisibility f = updNewCons id f id

--- update argument of newtype constructor declaration
updNewConsArg :: Update NewConsDecl TypeExpr
updNewConsArg = updNewCons id id

-- Auxiliary Functions

updQNamesInNewConsDecl :: Update NewConsDecl QName
updQNamesInNewConsDecl f = updNewCons f id (updQNamesInTypeExpr f)

-- TypeExpr ------------------------------------------------------------------

-- Selectors

--- get index from type variable
tVarIndex :: TypeExpr -> TVarIndex
tVarIndex texpr = case texpr of
  (TVar n) -> n
  _        -> error "AnnotatedFlatCurryGoodies.tVarIndex: no type variable"

--- get domain from functional type
domain :: TypeExpr -> TypeExpr
domain texpr = case texpr of
  (FuncType dom _) -> dom
  _                -> error "AnnotatedFlatCurryGoodies.domain: no functional type"

--- get range from functional type
range :: TypeExpr -> TypeExpr
range texpr = case texpr of
  (FuncType _ ran) -> ran
  _                -> error "AnnotatedFlatCurryGoodies.range: no functional type"

--- get name from constructed type
tConsName :: TypeExpr -> QName
tConsName texpr = case texpr of
  (TCons name _) -> name
  _              -> error "AnnotatedFlatCurryGoodies.tConsName: no functional type"

--- get arguments from constructed type
tConsArgs :: TypeExpr -> [TypeExpr]
tConsArgs texpr = case texpr of
  (TCons _ args) -> args
  _              -> error "AnnotatedFlatCurryGoodies.tConsArgs: no functional type"

--- transform type expression
trTypeExpr :: (TVarIndex -> a) ->
              (QName -> [a] -> a) ->
              (a -> a -> a) ->
              ([(TVarIndex, Kind)] -> a -> a) -> TypeExpr -> a
trTypeExpr tvar _ _ _ (TVar tv) = tvar tv
trTypeExpr tvar tcons functype foralltype (TCons name args)
  = tcons name (map (trTypeExpr tvar tcons functype foralltype) args)
trTypeExpr tvar tcons functype foralltype (FuncType from to)
  = functype (f from) (f to)
 where
  f = trTypeExpr tvar tcons functype foralltype
trTypeExpr tvar tcons functype foralltype (ForallType ns t)
  = foralltype ns (trTypeExpr tvar tcons functype foralltype t)

-- Test Operations

--- is type expression a type variable?
isTVar :: TypeExpr -> Bool
isTVar = trTypeExpr (\_ -> True) (\_ _ -> False) (\_ _ -> False) (\_ _ -> False)

--- is type declaration a constructed type?
isTCons :: TypeExpr -> Bool
isTCons
  = trTypeExpr (\_ -> False) (\_ _ -> True) (\_ _ -> False) (\_ _ -> False)

--- is type declaration a functional type?
isFuncType :: TypeExpr -> Bool
isFuncType
  = trTypeExpr (\_ -> False) (\_ _ -> False) (\_ _ -> True) (\_ _ -> False)

--- is type declaration a forall type?
isForallType :: TypeExpr -> Bool
isForallType
  = trTypeExpr (\_ -> False) (\_ _ -> False) (\_ _ -> False) (\_ _ -> True)

-- Update Operations

--- update all type variables
updTVars :: (TVarIndex -> TypeExpr) -> TypeExpr -> TypeExpr
updTVars tvar = trTypeExpr tvar TCons FuncType ForallType

--- update all type constructors
updTCons :: (QName -> [TypeExpr] -> TypeExpr) -> TypeExpr -> TypeExpr
updTCons tcons = trTypeExpr TVar tcons FuncType ForallType

--- update all functional types
updFuncTypes :: (TypeExpr -> TypeExpr -> TypeExpr) -> TypeExpr -> TypeExpr
updFuncTypes functype = trTypeExpr TVar TCons functype ForallType

--- update all forall types
updForallTypes :: ([(TVarIndex, Kind)] -> TypeExpr -> TypeExpr)
               -> TypeExpr -> TypeExpr
updForallTypes = trTypeExpr TVar TCons FuncType

-- Auxiliary Functions

--- get argument types from functional type
argTypes :: TypeExpr -> [TypeExpr]
argTypes (TVar           _) = []
argTypes (TCons        _ _) = []
argTypes (FuncType dom ran) = dom : argTypes ran
argTypes (ForallType _ _) = []

--- get result type from (nested) functional type
resultType :: TypeExpr -> TypeExpr
resultType (TVar          n) = TVar n
resultType (TCons name args) = TCons name args
resultType (FuncType  _ ran) = resultType ran
resultType (ForallType ns t) = ForallType ns t

--- rename variables in type expression
rnmAllVarsInTypeExpr :: (TVarIndex -> TVarIndex) -> TypeExpr -> TypeExpr
rnmAllVarsInTypeExpr f = updTVars (TVar . f)

--- update all qualified names in type expression
updQNamesInTypeExpr :: (QName -> QName) -> TypeExpr -> TypeExpr
updQNamesInTypeExpr f = updTCons (\name args -> TCons (f name) args)

-- OpDecl --------------------------------------------------------------------

--- transform operator declaration
trOp :: (QName -> Fixity -> Int -> a) -> OpDecl -> a
trOp op (Op name fix prec) = op name fix prec

-- Selectors

--- get name from operator declaration
opName :: OpDecl -> QName
opName = trOp (\name _ _ -> name)

--- get fixity of operator declaration
opFixity :: OpDecl -> Fixity
opFixity = trOp (\_ fix _ -> fix)

--- get precedence of operator declaration
opPrecedence :: OpDecl -> Int
opPrecedence = trOp (\_ _ prec -> prec)

-- Update Operations

--- update operator declaration
updOp :: (QName -> QName) ->
         (Fixity -> Fixity) ->
         (Int -> Int)       -> OpDecl -> OpDecl
updOp fn ff fp = trOp op
 where
  op name fix prec = Op (fn name) (ff fix) (fp prec)

--- update name of operator declaration
updOpName :: Update OpDecl QName
updOpName f = updOp f id id

--- update fixity of operator declaration
updOpFixity :: Update OpDecl Fixity
updOpFixity f = updOp id f id

--- update precedence of operator declaration
updOpPrecedence :: Update OpDecl Int
updOpPrecedence = updOp id id

-- FuncDecl --------------------------------------------------------------------

--- transform function
trFunc :: (QName -> Int -> Visibility -> TypeExpr -> ARule a -> b)
       -> AFuncDecl a -> b
trFunc func (AFunc name arity vis t rule) = func name arity vis t rule

-- Selectors

--- get name of function
funcName :: AFuncDecl _ -> QName
funcName = trFunc (\name _ _ _ _ -> name)

--- get arity of function
funcArity :: AFuncDecl _ -> Int
funcArity = trFunc (\_ arity _ _ _ -> arity)

--- get visibility of function
funcVisibility :: AFuncDecl _ -> Visibility
funcVisibility = trFunc (\_ _ vis _ _ -> vis)

--- get type of function
funcType :: AFuncDecl _ -> TypeExpr
funcType = trFunc (\_ _ _ t _ -> t)

--- get rule of function
funcRule :: AFuncDecl a -> ARule a
funcRule = trFunc (\_ _ _ _ rule -> rule)

-- Update Operations

--- update function
updFunc :: (QName -> QName)
        -> (Int -> Int)
        -> (Visibility -> Visibility)
        -> (TypeExpr -> TypeExpr)
        -> (ARule a -> ARule a)
        -> AFuncDecl a -> AFuncDecl a
updFunc fn fa fv ft fr = trFunc func
 where
  func name arity vis t rule
    = AFunc (fn name) (fa arity) (fv vis) (ft t) (fr rule)

--- update name of function
updFuncName :: Update (AFuncDecl _) QName
updFuncName f = updFunc f id id id id

--- update arity of function
updFuncArity :: Update (AFuncDecl _) Int
updFuncArity f = updFunc id f id id id

--- update visibility of function
updFuncVisibility :: Update (AFuncDecl _) Visibility
updFuncVisibility f = updFunc id id f id id

--- update type of function
updFuncType :: Update (AFuncDecl _) TypeExpr
updFuncType f = updFunc id id id f id

--- update rule of function
updFuncRule :: Update (AFuncDecl a) (ARule a)
updFuncRule = updFunc id id id id

-- Auxiliary Functions

--- is function externally defined?
isExternal :: AFuncDecl _ -> Bool
isExternal = isRuleExternal . funcRule

--- get variable names in a function declaration
allVarsInFunc :: AFuncDecl _ -> [VarIndex]
allVarsInFunc = allVarsInRule . funcRule

--- get arguments of function, if not externally defined
funcArgs :: AFuncDecl a -> [(VarIndex, a)]
funcArgs = ruleArgs . funcRule

--- get body of function, if not externally defined
funcBody :: AFuncDecl a -> AExpr a
funcBody = ruleBody . funcRule

funcRHS :: AFuncDecl a -> [AExpr a]
funcRHS f | not (isExternal f) = orCase (funcBody f)
          | otherwise = []
 where
  orCase e
    | isOr e = concatMap orCase (orExps e)
    | isCase e = concatMap orCase (map branchExpr (caseBranches e))
    | otherwise = [e]

--- rename all variables in function
rnmAllVarsInFunc :: Update (AFuncDecl _) VarIndex
rnmAllVarsInFunc = updFunc id id id id . rnmAllVarsInRule

--- update all qualified names in function
updQNamesInFunc :: Update (AFuncDecl _) QName
updQNamesInFunc f = updFunc f id id (updQNamesInTypeExpr f) (updQNamesInRule f)

--- update arguments of function, if not externally defined
updFuncArgs :: Update (AFuncDecl a) [(VarIndex, a)]
updFuncArgs = updFuncRule . updRuleArgs

--- update body of function, if not externally defined
updFuncBody :: Update (AFuncDecl a) (AExpr a)
updFuncBody = updFuncRule . updRuleBody

-- Rule ------------------------------------------------------------------------

--- transform rule
trRule :: (a -> [(VarIndex, a)] -> AExpr a -> b) -> (a -> String -> b)
       -> ARule a -> b
trRule rule _   (ARule     a vs e) = rule a vs e
trRule _    ext (AExternal a    s) = ext a s

-- Selectors

--- get rules arguments if it's not external
ruleArgs :: ARule a -> [(VarIndex, a)]
ruleArgs = trRule (\_ vs _ -> vs) failed

--- get rules body if it's not external
ruleBody :: ARule a -> AExpr a
ruleBody = trRule (\_ _ e -> e) failed

--- get rules external declaration
ruleExtDecl :: ARule _ -> String
ruleExtDecl = trRule failed (\_ s -> s)

-- Test Operations

--- is rule external?
isRuleExternal :: ARule _ -> Bool
isRuleExternal = trRule (\_ _ _ -> False) (\_ _ -> True)

-- Update Operations

--- update rule
updRule :: (a -> a)
        -> ([(VarIndex, a)] -> [(VarIndex, a)])
        -> (AExpr a -> AExpr a)
        -> (String -> String)
        -> ARule a -> ARule a
updRule fa fvs fe fs = trRule rule ext
 where
  rule a vs e = ARule     (fa a) (fvs vs) (fe e)
  ext  a    s = AExternal (fa a) (fs s)

--- update rules arguments
updRuleArgs :: Update (ARule a) [(VarIndex, a)]
updRuleArgs f = updRule id f id id

--- update rules body
updRuleBody :: Update (ARule a) (AExpr a)
updRuleBody f = updRule id id f id

--- update rules external declaration
updRuleExtDecl :: Update (ARule _) String
updRuleExtDecl f = updRule id id id f

-- Auxiliary Functions

--- get variable names in a functions rule
allVarsInRule :: ARule _ -> [VarIndex]
allVarsInRule = trRule (\_ vs e -> map fst vs ++ allVars e) (\_ _ -> [])

--- rename all variables in rule
rnmAllVarsInRule :: Update (ARule _) VarIndex
rnmAllVarsInRule f = updRule id (map (\(v,a) -> (f v, a))) (rnmAllVars f) id

--- update all qualified names in rule
updQNamesInRule :: Update (ARule _) QName
updQNamesInRule = updRuleBody . updQNames

-- CombType --------------------------------------------------------------------

--- transform combination type
trCombType :: a -> (Int -> a) -> a -> (Int -> a) -> CombType -> a
trCombType fc _   _  _   FuncCall         = fc
trCombType _  fpc _  _   (FuncPartCall n) = fpc n
trCombType _  _   cc _   ConsCall         = cc
trCombType _  _   _  cpc (ConsPartCall n) = cpc n

-- Test Operations

--- is type of combination FuncCall?
isCombTypeFuncCall :: CombType -> Bool
isCombTypeFuncCall = trCombType True (\_ -> False) False (\_ -> False)

--- is type of combination FuncPartCall?
isCombTypeFuncPartCall :: CombType -> Bool
isCombTypeFuncPartCall = trCombType False (\_ -> True) False (\_ -> False)

--- is type of combination ConsCall?
isCombTypeConsCall :: CombType -> Bool
isCombTypeConsCall = trCombType False (\_ -> False) True (\_ -> False)

--- is type of combination ConsPartCall?
isCombTypeConsPartCall :: CombType -> Bool
isCombTypeConsPartCall = trCombType False (\_ -> False) False (\_ -> True)

-- Auxiliary Functions

missingArgs :: CombType -> Int
missingArgs = trCombType 0 id 0 id

-- Expr ------------------------------------------------------------------------

-- Selectors

--- get internal number of variable
varNr :: AExpr _ -> VarIndex
varNr aexpr = case aexpr of
  (AVar _ n) -> n
  _          -> error "AnnotatedFlatCurryGoodies.varNr: no variable"

--- get literal if expression is literal expression
literal :: AExpr _ -> Literal
literal aexpr = case aexpr of
  (ALit _ l) -> l
  _          -> error "AnnotatedFlatCurryGoodies.literal: no literal"

--- get combination type of a combined expression
combType :: AExpr _ -> CombType
combType aexpr = case aexpr of
  (AComb _ ct _ _) -> ct
  _                -> error "AnnotatedFlatCurryGoodies.combType: no combined expression"

--- get name of a combined expression
combName :: AExpr _ -> QName
combName aexpr = case aexpr of
  (AComb _ _ name _) -> fst name
  _                  -> error "AnnotatedFlatCurryGoodies.combName: no combined expression"

--- get arguments of a combined expression
combArgs :: AExpr a -> [AExpr a]
combArgs aexpr = case aexpr of
  (AComb _ _ _ args) -> args
  _                  -> error "AnnotatedFlatCurryGoodies.combArgs: no combined expression"

--- get number of missing arguments if expression is combined
missingCombArgs :: AExpr _ -> Int
missingCombArgs = missingArgs . combType

--- get indices of variables in let declaration
letBinds :: AExpr a -> [((VarIndex, a), AExpr a)]
letBinds aexpr = case aexpr of
  (ALet _ vs _) -> vs
  _             -> error "AnnotatedFlatCurryGoodies.letBinds: no let declaration"

--- get body of let declaration
letBody :: AExpr a -> AExpr a
letBody aexpr = case aexpr of
  (ALet _ _ e) -> e
  _            -> error "AnnotatedFlatCurryGoodies.letBody: no let declaration"

--- get variable indices from declaration of free variables
freeVars :: AExpr _ -> [VarIndex]
freeVars aexpr = case aexpr of
  (AFree _ vs _) -> map fst vs
  _              -> error "AnnotatedFlatCurryGoodies.freeVars: no free variable declaration"

--- get expression from declaration of free variables
freeExpr :: AExpr a -> AExpr a
freeExpr aexpr = case aexpr of
  (AFree _ _ e) -> e
  _             -> error "AnnotatedFlatCurryGoodies.freeExpr: no free variable declaration"

--- get expressions from or-expression
orExps :: AExpr a -> [AExpr a]
orExps aexpr = case aexpr of
  (AOr _ e1 e2) -> [e1,e2]
  _             -> error "AnnotatedFlatCurryGoodies.orExps: no or-expression"

--- get case-type of case expression
caseType :: AExpr _ -> CaseType
caseType aexpr = case aexpr of
  (ACase _ ct _ _) -> ct
  _                -> error "AnnotatedFlatCurryGoodies.caseType: no case expression"

--- get scrutinee of case expression
caseExpr :: AExpr a -> AExpr a
caseExpr aexpr = case aexpr of
  (ACase _ _ e _) -> e
  _               -> error "AnnotatedFlatCurryGoodies.caseExpr: no case expression"

 --- get branch expressions from case expression
caseBranches :: AExpr a -> [ABranchExpr a]
caseBranches aexpr = case aexpr of
  (ACase _ _ _ bs) -> bs
  _                -> error "AnnotatedFlatCurryGoodies.caseBranches: no case expression"

 -- Test Operations

--- is expression a variable?
isVar :: AExpr _ -> Bool
isVar e = case e of
  AVar _ _ -> True
  _ -> False

--- is expression a literal expression?
isLit :: AExpr _ -> Bool
isLit e = case e of
  ALit _ _ -> True
  _ -> False

--- is expression combined?
isComb :: AExpr _ -> Bool
isComb e = case e of
  AComb _ _ _ _ -> True
  _ -> False

--- is expression a let expression?
isLet :: AExpr _ -> Bool
isLet e = case e of
  ALet _ _ _ -> True
  _ -> False

--- is expression a declaration of free variables?
isFree :: AExpr _ -> Bool
isFree e = case e of
  AFree _ _ _ -> True
  _ -> False

--- is expression an or-expression?
isOr :: AExpr _ -> Bool
isOr e = case e of
  AOr _ _ _ -> True
  _ -> False

--- is expression a case expression?
isCase :: AExpr _ -> Bool
isCase e = case e of
  ACase _ _ _ _ -> True
  _ -> False

--- transform expression
trExpr :: (a -> VarIndex -> b)
       -> (a -> Literal -> b)
       -> (a -> CombType -> (QName, a) -> [b] -> b)
       -> (a -> [((VarIndex, a), b)] -> b -> b)
       -> (a -> [(VarIndex, a)] -> b -> b)
       -> (a -> b -> b -> b)
       -> (a -> CaseType -> b -> [c] -> b)
       -> (APattern a -> b -> c)
       -> (a -> b -> TypeExpr -> b)
       -> AExpr a -> b
trExpr var _ _ _ _ _ _ _ _ (AVar a n) = var a n

trExpr _ lit _ _ _ _ _ _ _ (ALit a l) = lit a l

trExpr var lit comb lt fr or cas branch typed (AComb a ct name args)
  = comb a ct name (map (trExpr var lit comb lt fr or cas branch typed) args)

trExpr var lit comb lt fr or cas branch typed (ALet a bs e)
  = lt a (map (\ (n,exp) -> (n,f exp)) bs) (f e)
 where
  f = trExpr var lit comb lt fr or cas branch typed

trExpr var lit comb lt fr or cas branch typed (AFree a vs e)
  = fr a vs (trExpr var lit comb lt fr or cas branch typed e)

trExpr var lit comb lt fr or cas branch typed (AOr a e1 e2) = or a (f e1) (f e2)
 where
  f = trExpr var lit comb lt fr or cas branch typed

trExpr var lit comb lt fr or cas branch typed (ACase a ct e bs)
  = cas a ct (f e) (map (\ (ABranch pat exp) -> branch pat (f exp)) bs)
 where
  f = trExpr var lit comb lt fr or cas branch typed
trExpr var lit comb lt fr or cas branch typed (ATyped a e ty)
  = typed a (trExpr var lit comb lt fr or cas branch typed e) ty

-- Update Operations

--- update all variables in given expression
updVars :: (a -> VarIndex -> AExpr a) -> AExpr a -> AExpr a
updVars var = trExpr var ALit AComb ALet AFree AOr ACase ABranch ATyped

--- update all literals in given expression
updLiterals :: (a -> Literal -> AExpr a) -> AExpr a -> AExpr a
updLiterals lit = trExpr AVar lit AComb ALet AFree AOr ACase ABranch ATyped

--- update all combined expressions in given expression
updCombs :: (a -> CombType -> (QName, a) -> [AExpr a] -> AExpr a)
         -> AExpr a -> AExpr a
updCombs comb = trExpr AVar ALit comb ALet AFree AOr ACase ABranch ATyped

--- update all let expressions in given expression
updLets :: (a -> [((VarIndex, a), AExpr a)] -> AExpr a -> AExpr a) -> AExpr a -> AExpr a
updLets lt = trExpr AVar ALit AComb lt AFree AOr ACase ABranch ATyped

--- update all free declarations in given expression
updFrees :: (a -> [(VarIndex, a)] -> AExpr a -> AExpr a) -> AExpr a -> AExpr a
updFrees fr = trExpr AVar ALit AComb ALet fr AOr ACase ABranch ATyped

--- update all or expressions in given expression
updOrs :: (a -> AExpr a -> AExpr a -> AExpr a) -> AExpr a -> AExpr a
updOrs or = trExpr AVar ALit AComb ALet AFree or ACase ABranch ATyped

--- update all case expressions in given expression
updCases :: (a -> CaseType -> AExpr a -> [ABranchExpr a] -> AExpr a)
         -> AExpr a -> AExpr a
updCases cas = trExpr AVar ALit AComb ALet AFree AOr cas ABranch ATyped

--- update all case branches in given expression
updBranches :: (APattern a -> AExpr a -> ABranchExpr a) -> AExpr a -> AExpr a
updBranches branch = trExpr AVar ALit AComb ALet AFree AOr ACase branch ATyped

--- update all typed expressions in given expression
updTypeds :: (a -> AExpr a -> TypeExpr -> AExpr a) -> AExpr a -> AExpr a
updTypeds typed = trExpr AVar ALit AComb ALet AFree AOr ACase ABranch typed

-- Auxiliary Functions

--- is expression a call of a function where all arguments are provided?
isFuncCall :: AExpr _ -> Bool
isFuncCall e = isComb e && isCombTypeFuncCall (combType e)

--- is expression a partial function call?
isFuncPartCall :: AExpr _ -> Bool
isFuncPartCall e = isComb e && isCombTypeFuncPartCall (combType e)

--- is expression a call of a constructor?
isConsCall :: AExpr _ -> Bool
isConsCall e = isComb e && isCombTypeConsCall (combType e)

--- is expression a partial constructor call?
isConsPartCall :: AExpr _ -> Bool
isConsPartCall e = isComb e && isCombTypeConsPartCall (combType e)

--- is expression fully evaluated?
isGround :: AExpr _ -> Bool
isGround exp = case exp of
  AComb _ ConsCall _ args -> all isGround args
  _ -> isLit exp

--- get all variables (also pattern variables) in expression
allVars :: AExpr _ -> [VarIndex]
allVars e = trExpr var lit comb lt fr or cas branch typed e []
 where
  var _ = (:)
  lit _ = const id
  comb _ _ _ = foldr (.) id
  lt _ bs exp = exp . foldr (.) id (map (\ ((n, _), ns) -> (n:) . ns) bs)
  fr _ vs exp = (map fst vs ++) . exp
  or _ = (.)
  cas _ _ exp bs = exp . foldr (.) id bs
  branch pat exp = (args pat ++) . exp
  args pat | isConsPattern pat = map fst (patArgs pat)
           | otherwise = []
  typed _ = const

--- rename all variables (also in patterns) in expression
rnmAllVars :: Update (AExpr _) VarIndex
rnmAllVars f = trExpr var ALit AComb lt fre AOr ACase branch ATyped
 where
   var a = AVar a . f
   fre a vs b = AFree a (map (\(v, x) -> (f v, x)) vs) b
   lt a = ALet a . map (\ ((n, x), exp) -> ((f n, x),exp))
   branch = ABranch . updPatArgs (map (\(v, a) -> (f v, a)))

--- update all qualified names in expression
updQNames :: Update (AExpr _) QName
updQNames f = trExpr AVar ALit comb ALet AFree AOr ACase branch typed
 where
  comb a ct (name, b) args = AComb a ct (f name, b) args
  branch = ABranch . updPatCons f
  typed a e ty = ATyped a e (updQNamesInTypeExpr f ty)


-- BranchExpr ----------------------------------------------------------------

--- transform branch expression
trBranch :: (APattern a -> AExpr a -> b) -> ABranchExpr a -> b
trBranch branch (ABranch pat exp) = branch pat exp

-- Selectors

--- get pattern from branch expression
branchPattern :: ABranchExpr a -> APattern a
branchPattern = trBranch (\pat _ -> pat)

--- get expression from branch expression
branchExpr :: ABranchExpr a -> AExpr a
branchExpr = trBranch (\_ e -> e)

-- Update Operations

--- update branch expression
updBranch :: (APattern a -> APattern a)
          -> (AExpr a -> AExpr a)
          -> ABranchExpr a -> ABranchExpr a
updBranch fp fe = trBranch branch
 where
  branch pat exp = ABranch (fp pat) (fe exp)

--- update pattern of branch expression
updBranchPattern :: Update (ABranchExpr a) (APattern a)
updBranchPattern f = updBranch f id

--- update expression of branch expression
updBranchExpr :: Update (ABranchExpr a) (AExpr a)
updBranchExpr = updBranch id

-- Pattern -------------------------------------------------------------------

--- transform pattern
trPattern :: (a -> (QName, a) -> [(VarIndex, a)] -> b) -> (a -> Literal -> b)
          -> APattern a -> b
trPattern pattern _        (APattern  a name args) = pattern a name args
trPattern _       lpattern (ALPattern a         l) = lpattern a l

-- Selectors

--- get name from constructor pattern
patCons :: APattern _ -> QName
patCons = trPattern (\_ name _ -> fst name) failed

--- get arguments from constructor pattern
patArgs :: APattern a -> [(VarIndex, a)]
patArgs = trPattern (\_ _ args -> args) failed

--- get literal from literal pattern
patLiteral :: APattern _ -> Literal
patLiteral = trPattern failed (\_ l -> l)

-- Test Operations

--- is pattern a constructor pattern?
isConsPattern :: APattern _ -> Bool
isConsPattern = trPattern (\ _ _ _ -> True) (\ _ _ -> False)

-- Update Operations

--- update pattern
updPattern :: ((QName, a) -> (QName, a))
           -> ([(VarIndex, a)] -> [(VarIndex, a)])
           -> (Literal -> Literal)
           -> APattern a -> APattern a
updPattern fn fa fl = trPattern apattern alpattern
 where
  apattern a name args = APattern a (fn name) (fa args)
  alpattern a l = ALPattern a (fl l)

--- update constructors name of pattern
updPatCons :: (QName -> QName) -> APattern a -> APattern a
updPatCons f = updPattern (\(n,a) -> (f n, a)) id id

--- update arguments of constructor pattern
updPatArgs :: ([(VarIndex, a)] -> [(VarIndex, a)]) -> APattern a -> APattern a
updPatArgs f = updPattern id f id

--- update literal of pattern
updPatLiteral :: (Literal -> Literal) -> APattern a -> APattern a
updPatLiteral f = updPattern id id f

-- Auxiliary Functions

--- build expression from pattern
patExpr :: APattern a -> AExpr a
patExpr = trPattern (\a name vs -> AComb a ConsCall name (map var vs)) ALit
  where var (v, a) = AVar a v

-- Retrieval of the annotation

annRule :: ARule a -> a
annRule (ARule a   _ _) = a
annRule (AExternal a _) = a

--- Extract the annotation of an annotated expression.
annExpr :: AExpr a -> a
annExpr (AComb  a _ _ _) = a
annExpr (ACase  a _ _ _) = a
annExpr (AVar   a _    ) = a
annExpr (ALit   a _    ) = a
annExpr (AOr    a _ _  ) = a
annExpr (ALet   a _ _  ) = a
annExpr (AFree  a _ _  ) = a
annExpr (ATyped a _ _  ) = a

--- Extract the annotation of an annotated pattern.
annPattern :: APattern a -> a
annPattern (APattern  a _ _) = a
annPattern (ALPattern a _  ) = a

-- Remove annotation

unAnnProg :: AProg _ -> FC.Prog
unAnnProg = trProg (\m is ts fs os -> FC.Prog m is ts (map unAnnFuncDecl fs) os)

unAnnFuncDecl :: AFuncDecl _ -> FC.FuncDecl
unAnnFuncDecl = trFunc (\f ar vs ty r -> FC.Func f ar vs ty (unAnnRule r))

unAnnRule :: ARule _ -> FC.Rule
unAnnRule = trRule (\_ vs e -> FC.Rule (map fst vs) (unAnnExpr e))
                   (\_ s -> FC.External s)

unAnnExpr :: AExpr _ -> FC.Expr
unAnnExpr = trExpr var lit comb lett fre or cse branch typed
  where
  var   _       v = FC.Var v
  lit   _       l = FC.Lit l
  comb  _ ct n es = FC.Comb ct (fst n) es
  lett  _    bs e = FC.Let (map (\((v, _), b) -> (v, b)) bs) e
  fre   _    vs e = FC.Free (map fst vs) e
  or    _     a b = FC.Or a b
  cse   _ ct e bs = FC.Case ct e bs
  branch      p e = FC.Branch (unAnnPattern p) e
  typed _    e ty = FC.Typed e ty

unAnnPattern :: APattern _ -> FC.Pattern
unAnnPattern = trPattern (\_ qn vs -> FC.Pattern (fst qn) (map fst vs))
                         (\_ l -> FC.LPattern l)
