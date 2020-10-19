----------------------------------------------------------------------------
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
--- @author Sebastian Fischer
--- @version May 2017
--- @category meta
----------------------------------------------------------------------------

module FlatCurry.Goodies where

import FlatCurry.Types

type Update a b = (b -> b) -> a -> a

-- Prog ----------------------------------------------------------------------

--- transform program
trProg :: (String -> [String] -> [TypeDecl] -> [FuncDecl] -> [OpDecl] -> a)
          -> Prog -> a
trProg prog (Prog name imps types funcs ops) = prog name imps types funcs ops

-- Selectors

--- get name from program
progName :: Prog -> String
progName = trProg (\name _ _ _ _ -> name)

--- get imports from program
progImports :: Prog -> [String]
progImports = trProg (\_ imps _ _ _ -> imps)

--- get type declarations from program
progTypes :: Prog -> [TypeDecl]
progTypes = trProg (\_ _ types _ _ -> types)

--- get functions from program
progFuncs :: Prog -> [FuncDecl]
progFuncs = trProg (\_ _ _ funcs _ -> funcs)

--- get infix operators from program
progOps :: Prog -> [OpDecl]
progOps = trProg (\_ _ _ _ ops -> ops)

-- Update Operations

--- update program
updProg :: (String -> String)         ->
           ([String] -> [String])     ->
           ([TypeDecl] -> [TypeDecl]) ->
           ([FuncDecl] -> [FuncDecl]) ->
           ([OpDecl] -> [OpDecl])     -> Prog -> Prog
updProg fn fi ft ff fo = trProg prog
 where
  prog name imps types funcs ops
    = Prog (fn name) (fi imps) (ft types) (ff funcs) (fo ops)

--- update name of program
updProgName :: Update Prog String
updProgName f = updProg f id id id id

--- update imports of program
updProgImports :: Update Prog [String]
updProgImports f = updProg id f id id id

--- update type declarations of program
updProgTypes :: Update Prog [TypeDecl]
updProgTypes f = updProg id id f id id

--- update functions of program
updProgFuncs :: Update Prog [FuncDecl]
updProgFuncs f = updProg id id id f id

--- update infix operators of program
updProgOps :: Update Prog [OpDecl]
updProgOps = updProg id id id id

-- Auxiliary Functions

--- get all program variables (also from patterns)
allVarsInProg :: Prog -> [VarIndex]
allVarsInProg = concatMap allVarsInFunc . progFuncs

--- lift transformation on expressions to program
updProgExps :: Update Prog Expr
updProgExps = updProgFuncs . map . updFuncBody

--- rename programs variables
rnmAllVarsInProg :: Update Prog VarIndex
rnmAllVarsInProg = updProgFuncs . map . rnmAllVarsInFunc

--- update all qualified names in program
updQNamesInProg :: Update Prog QName
updQNamesInProg f = updProg id id
  (map (updQNamesInType f)) (map (updQNamesInFunc f)) (map (updOpName f))

--- rename program (update name of and all qualified names in program)
rnmProg :: String -> Prog -> Prog
rnmProg name p = updProgName (const name) (updQNamesInProg rnm p)
 where
  rnm (mod,n) | mod==progName p = (name,n)
              | otherwise = (mod,n)

-- TypeDecl ------------------------------------------------------------------

-- Selectors

--- transform type declaration
trType :: (QName -> Visibility -> [(TVarIndex, Kind)] -> [ConsDecl] -> a) ->
          (QName -> Visibility -> [(TVarIndex, Kind)] -> TypeExpr   -> a) -> TypeDecl -> a
trType typ _ (Type name vis params cs) = typ name vis params cs
trType _ typesyn (TypeSyn name vis params syn) = typesyn name vis params syn

--- get name of type declaration
typeName :: TypeDecl -> QName
typeName = trType (\name _ _ _ -> name) (\name _ _ _ -> name)

--- get visibility of type declaration
typeVisibility :: TypeDecl -> Visibility
typeVisibility = trType (\_ vis _ _ -> vis) (\_ vis _ _ -> vis)

--- get type parameters of type declaration
typeParams :: TypeDecl -> [(TVarIndex, Kind)]
typeParams = trType (\_ _ params _ -> params) (\_ _ params _ -> params)

--- get constructor declarations from type declaration
typeConsDecls :: TypeDecl -> [ConsDecl]
typeConsDecls = trType (\_ _ _ cs -> cs) failed

--- get synonym of type declaration
typeSyn :: TypeDecl -> TypeExpr
typeSyn = trType failed (\_ _ _ syn -> syn)

--- is type declaration a type synonym?
isTypeSyn :: TypeDecl -> Bool
isTypeSyn = trType (\_ _ _ _ -> False) (\_ _ _ _ -> True)

-- Update Operations

--- update type declaration
updType :: (QName -> QName) ->
           (Visibility -> Visibility) ->
           ([(TVarIndex, Kind)] -> [(TVarIndex, Kind)]) ->
           ([ConsDecl] -> [ConsDecl]) ->
           (TypeExpr -> TypeExpr)     -> TypeDecl -> TypeDecl
updType fn fv fp fc fs = trType typ typesyn
 where
  typ name vis params cs = Type (fn name) (fv vis) (fp params) (fc cs)
  typesyn name vis params syn = TypeSyn (fn name) (fv vis) (fp params) (fs syn)

--- update name of type declaration
updTypeName :: Update TypeDecl QName
updTypeName f = updType f id id id id

--- update visibility of type declaration
updTypeVisibility :: Update TypeDecl Visibility
updTypeVisibility f = updType id f id id id

--- update type parameters of type declaration
updTypeParams :: Update TypeDecl [(TVarIndex, Kind)]
updTypeParams f = updType id id f id id

--- update constructor declarations of type declaration
updTypeConsDecls :: Update TypeDecl [ConsDecl]
updTypeConsDecls f = updType id id id f id

--- update synonym of type declaration
updTypeSynonym :: Update TypeDecl TypeExpr
updTypeSynonym = updType id id id id

-- Auxiliary Functions

--- update all qualified names in type declaration
updQNamesInType :: Update TypeDecl QName
updQNamesInType f
  = updType f id id (map (updQNamesInConsDecl f)) (updQNamesInTypeExpr f)

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

-- TypeExpr ------------------------------------------------------------------

-- Selectors

--- get index from type variable
tVarIndex :: TypeExpr -> TVarIndex
tVarIndex texpr = case texpr of
  (TVar (n, _)) -> n
  _             -> error "FlatCurryGoodies.tVarIndex: no type variable"

--- get domain from functional type
domain :: TypeExpr -> TypeExpr
domain texpr = case texpr of
  (FuncType dom _) -> dom
  _                -> error "FlatCurryGoodies.domain: no functional type"

--- get range from functional type
range :: TypeExpr -> TypeExpr
range texpr = case texpr of
  (FuncType _ ran) -> ran
  _                -> error "FlatCurryGoodies.range: no functional type"

--- get name from constructed type
tConsName :: TypeExpr -> QName
tConsName texpr = case texpr of
  (TCons name _) -> name
  _              -> error "FlatCurryGoodies.tConsName: no constructed type"

--- get arguments from constructed type
tConsArgs :: TypeExpr -> [TypeExpr]
tConsArgs texpr = case texpr of
  (TCons _ args) -> args
  _              -> error "FlatCurryGoodies.tConsArgs: no constructed type"

--- transform type expression
trTypeExpr :: (TVarWithKind -> a) ->
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
updTVars :: (TVarWithKind -> TypeExpr) -> TypeExpr -> TypeExpr
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
argTypes (TVar _) = []
argTypes (TCons _ _) = []
argTypes (FuncType dom ran) = dom : argTypes ran
argTypes (ForallType _ _) = []

--- get result type from (nested) functional type
resultType :: TypeExpr -> TypeExpr
resultType (TVar n) = TVar n
resultType (TCons name args) = TCons name args
resultType (FuncType _ ran) = resultType ran
resultType (ForallType ns t) = ForallType ns t

--- rename variables in type expression
rnmAllVarsInTypeExpr :: (Int -> Int) -> TypeExpr -> TypeExpr
rnmAllVarsInTypeExpr f = updTVars (\(i, k) -> TVar (f i, k))

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

-- FuncDecl ------------------------------------------------------------------

--- transform function
trFunc :: (QName -> Int -> Visibility -> TypeExpr -> Rule -> a) -> FuncDecl -> a
trFunc func (Func name arity vis t rule) = func name arity vis t rule

-- Selectors

--- get name of function
funcName :: FuncDecl -> QName
funcName = trFunc (\name _ _ _ _ -> name)

--- get arity of function
funcArity :: FuncDecl -> Int
funcArity = trFunc (\_ arity _ _ _ -> arity)

--- get visibility of function
funcVisibility :: FuncDecl -> Visibility
funcVisibility = trFunc (\_ _ vis _ _ -> vis)

--- get type of function
funcType :: FuncDecl -> TypeExpr
funcType = trFunc (\_ _ _ t _ -> t)

--- get rule of function
funcRule :: FuncDecl -> Rule
funcRule = trFunc (\_ _ _ _ rule -> rule)

-- Update Operations

--- update function
updFunc :: (QName -> QName) ->
           (Int -> Int) ->
           (Visibility -> Visibility) ->
           (TypeExpr -> TypeExpr) ->
           (Rule -> Rule)             -> FuncDecl -> FuncDecl
updFunc fn fa fv ft fr = trFunc func
 where
  func name arity vis t rule
    = Func (fn name) (fa arity) (fv vis) (ft t) (fr rule)

--- update name of function
updFuncName :: Update FuncDecl QName
updFuncName f = updFunc f id id id id

--- update arity of function
updFuncArity :: Update FuncDecl Int
updFuncArity f = updFunc id f id id id

--- update visibility of function
updFuncVisibility :: Update FuncDecl Visibility
updFuncVisibility f = updFunc id id f id id

--- update type of function
updFuncType :: Update FuncDecl TypeExpr
updFuncType f = updFunc id id id f id

--- update rule of function
updFuncRule :: Update FuncDecl Rule
updFuncRule = updFunc id id id id

-- Auxiliary Functions

--- is function externally defined?
isExternal :: FuncDecl -> Bool
isExternal = isRuleExternal . funcRule

--- get variable names in a function declaration
allVarsInFunc :: FuncDecl -> [Int]
allVarsInFunc = allVarsInRule . funcRule

--- get arguments of function, if not externally defined
funcArgs :: FuncDecl -> [Int]
funcArgs = ruleArgs . funcRule

--- get body of function, if not externally defined
funcBody :: FuncDecl -> Expr
funcBody = ruleBody . funcRule

funcRHS :: FuncDecl -> [Expr]
funcRHS f | not (isExternal f) = orCase (funcBody f)
          | otherwise = []
 where
  orCase e
    | isOr e = concatMap orCase (orExps e)
    | isCase e = concatMap orCase (map branchExpr (caseBranches e))
    | otherwise = [e]

--- rename all variables in function
rnmAllVarsInFunc :: Update FuncDecl VarIndex
rnmAllVarsInFunc = updFunc id id id id . rnmAllVarsInRule

--- update all qualified names in function
updQNamesInFunc :: Update FuncDecl QName
updQNamesInFunc f = updFunc f id id (updQNamesInTypeExpr f) (updQNamesInRule f)

--- update arguments of function, if not externally defined
updFuncArgs :: Update FuncDecl [VarIndex]
updFuncArgs = updFuncRule . updRuleArgs

--- update body of function, if not externally defined
updFuncBody :: Update FuncDecl Expr
updFuncBody = updFuncRule . updRuleBody

-- Rule ----------------------------------------------------------------------

--- transform rule
trRule :: ([Int] -> Expr -> a) -> (String -> a) -> Rule -> a
trRule rule _ (Rule args exp) = rule args exp
trRule _ ext (External s) = ext s

-- Selectors

--- get rules arguments if it's not external
ruleArgs :: Rule -> [Int]
ruleArgs = trRule (\args _ -> args) failed

--- get rules body if it's not external
ruleBody :: Rule -> Expr
ruleBody = trRule (\_ exp -> exp) failed

--- get rules external declaration
ruleExtDecl :: Rule -> String
ruleExtDecl = trRule failed id

-- Test Operations

--- is rule external?
isRuleExternal :: Rule -> Bool
isRuleExternal = trRule (\_ _ -> False) (\_ -> True)

-- Update Operations

--- update rule
updRule :: ([Int] -> [Int]) ->
           (Expr -> Expr) ->
           (String -> String) -> Rule -> Rule
updRule fa fe fs = trRule rule ext
 where
  rule args exp = Rule (fa args) (fe exp)
  ext s = External (fs s)

--- update rules arguments
updRuleArgs :: Update Rule [VarIndex]
updRuleArgs f = updRule f id id

--- update rules body
updRuleBody :: Update Rule Expr
updRuleBody f = updRule id f id

--- update rules external declaration
updRuleExtDecl :: Update Rule String
updRuleExtDecl f = updRule id id f

-- Auxiliary Functions

--- get variable names in a functions rule
allVarsInRule :: Rule -> [Int]
allVarsInRule = trRule (\args body -> args ++ allVars body) (\_ -> [])

--- rename all variables in rule
rnmAllVarsInRule :: Update Rule VarIndex
rnmAllVarsInRule f = updRule (map f) (rnmAllVars f) id

--- update all qualified names in rule
updQNamesInRule :: Update Rule QName
updQNamesInRule = updRuleBody . updQNames

-- CombType ------------------------------------------------------------------

--- transform combination type
trCombType :: a -> (Int -> a) -> a -> (Int -> a) -> CombType -> a
trCombType fc _ _ _ FuncCall = fc
trCombType _ fpc _ _ (FuncPartCall n) = fpc n
trCombType _ _ cc _ ConsCall = cc
trCombType _ _ _ cpc (ConsPartCall n) = cpc n

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

-- Expr ----------------------------------------------------------------------

-- Selectors

--- get internal number of variable
varNr :: Expr -> Int
varNr expr = case expr of
  (Var n) -> n
  _       -> error "FlatCurryGoodies.varNr: no variable"

--- get literal if expression is literal expression
literal :: Expr -> Literal
literal expr = case expr of
  (Lit l) -> l
  _       -> error "FlatCurryGoodies.literal: no literal"

--- get combination type of a combined expression
combType :: Expr -> CombType
combType expr = case expr of
  (Comb ct _ _) -> ct
  _             -> error "FlatCurryGoodies.combType: no combined expression"

--- get name of a combined expression
combName :: Expr -> QName
combName expr = case expr of
  (Comb _ name _) -> name
  _               -> error "FlatCurryGoodies.combName: no combined expression"

--- get arguments of a combined expression
combArgs :: Expr -> [Expr]
combArgs expr = case expr of
  (Comb _ _ args) -> args
  _               -> error "FlatCurryGoodies.combArgs: no combined expression"

--- get number of missing arguments if expression is combined
missingCombArgs :: Expr -> Int
missingCombArgs = missingArgs . combType

--- get indices of variables in let declaration
letBinds :: Expr -> [(Int,Expr)]
letBinds expr = case expr of
  (Let vs _) -> vs
  _          -> error "FlatCurryGoodies.letBinds: no let declaration"

--- get body of let declaration
letBody :: Expr -> Expr
letBody expr = case expr of
  (Let _ e) -> e
  _         -> error "FlatCurryGoodies.letBody: no let declaration"

--- get variable indices from declaration of free variables
freeVars :: Expr -> [Int]
freeVars expr = case expr of
  (Free vs _) -> vs
  _           ->  error "FlatCurryGoodies.freeVars: no free variable declaration"

--- get expression from declaration of free variables
freeExpr :: Expr -> Expr
freeExpr expr = case expr of
  (Free _ e) -> e
  _          ->  error "FlatCurryGoodies.freeExpr: no free variable declaration"

--- get expressions from or-expression
orExps :: Expr -> [Expr]
orExps expr = case expr of
  (Or e1 e2) -> [e1,e2]
  _          -> error "FlatCurryGoodies.orExps: no or-expression"

--- get case-type of case expression
caseType :: Expr -> CaseType
caseType expr = case expr of
  (Case ct _ _) -> ct
  _             -> error "FlatCurryGoodies.caseType: no case expression"

--- get scrutinee of case expression
caseExpr :: Expr -> Expr
caseExpr expr = case expr of
  (Case _ e _) -> e
  _            -> error "FlatCurryGoodies.caseExpr: no case expression"

--- get branch expressions from case expression
caseBranches :: Expr -> [BranchExpr]
caseBranches expr = case expr of
  (Case _ _ bs) -> bs
  _             -> error "FlatCurryGoodies.caseBranches: no case expression"

-- Test Operations

--- is expression a variable?
isVar :: Expr -> Bool
isVar e = case e of
  Var _ -> True
  _     -> False

--- is expression a literal expression?
isLit :: Expr -> Bool
isLit e = case e of
  Lit _ -> True
  _     -> False

--- is expression combined?
isComb :: Expr -> Bool
isComb e = case e of
  Comb _ _ _ -> True
  _          -> False

--- is expression a let expression?
isLet :: Expr -> Bool
isLet e = case e of
  Let _ _ -> True
  _       -> False

--- is expression a declaration of free variables?
isFree :: Expr -> Bool
isFree e = case e of
  Free _ _ -> True
  _        -> False

--- is expression an or-expression?
isOr :: Expr -> Bool
isOr e = case e of
  Or _ _ -> True
  _      -> False

--- is expression a case expression?
isCase :: Expr -> Bool
isCase e = case e of
  Case _ _ _ -> True
  _          -> False

--- transform expression
trExpr :: (Int -> a) ->
          (Literal -> a) ->
          (CombType -> QName -> [a] -> a) ->
          ([(Int,a)] -> a -> a) ->
          ([Int] -> a -> a) ->
          (a -> a -> a) ->
          (CaseType -> a -> [b] -> a) ->
          (Pattern -> a -> b) ->
          (a -> TypeExpr -> a)
          -> Expr -> a
trExpr var _ _ _ _ _ _ _ _ (Var n) = var n

trExpr _ lit _ _ _ _ _ _ _ (Lit l) = lit l

trExpr var lit comb lt fr or cas branch typed (Comb ct name args)
  = comb ct name (map (trExpr var lit comb lt fr or cas branch typed) args)

trExpr var lit comb lt fr or cas branch typed (Let bs e)
  = lt (map (\ (n,exp) -> (n,f exp)) bs) (f e)
 where
  f = trExpr var lit comb lt fr or cas branch typed

trExpr var lit comb lt fr or cas branch typed (Free vs e)
  = fr vs (trExpr var lit comb lt fr or cas branch typed e)

trExpr var lit comb lt fr or cas branch typed (Or e1 e2) = or (f e1) (f e2)
 where
  f = trExpr var lit comb lt fr or cas branch typed

trExpr var lit comb lt fr or cas branch typed (Case ct e bs)
  = cas ct (f e) (map (\ (Branch pat exp) -> branch pat (f exp)) bs)
 where
  f = trExpr var lit comb lt fr or cas branch typed
trExpr var lit comb lt fr or cas branch typed (Typed e ty)
  = typed (trExpr var lit comb lt fr or cas branch typed e) ty

-- Update Operations

--- update all variables in given expression
updVars :: (Int -> Expr) -> Expr -> Expr
updVars var = trExpr var Lit Comb Let Free Or Case Branch Typed

--- update all literals in given expression
updLiterals :: (Literal -> Expr) -> Expr -> Expr
updLiterals lit = trExpr Var lit Comb Let Free Or Case Branch Typed

--- update all combined expressions in given expression
updCombs :: (CombType -> QName -> [Expr] -> Expr) -> Expr -> Expr
updCombs comb = trExpr Var Lit comb Let Free Or Case Branch Typed

--- update all let expressions in given expression
updLets :: ([(Int,Expr)] -> Expr -> Expr) -> Expr -> Expr
updLets lt = trExpr Var Lit Comb lt Free Or Case Branch Typed

--- update all free declarations in given expression
updFrees :: ([Int] -> Expr -> Expr) -> Expr -> Expr
updFrees fr = trExpr Var Lit Comb Let fr Or Case Branch Typed

--- update all or expressions in given expression
updOrs :: (Expr -> Expr -> Expr) -> Expr -> Expr
updOrs or = trExpr Var Lit Comb Let Free or Case Branch Typed

--- update all case expressions in given expression
updCases :: (CaseType -> Expr -> [BranchExpr] -> Expr) -> Expr -> Expr
updCases cas = trExpr Var Lit Comb Let Free Or cas Branch Typed

--- update all case branches in given expression
updBranches :: (Pattern -> Expr -> BranchExpr) -> Expr -> Expr
updBranches branch = trExpr Var Lit Comb Let Free Or Case branch Typed

--- update all typed expressions in given expression
updTypeds :: (Expr -> TypeExpr -> Expr) -> Expr -> Expr
updTypeds typed = trExpr Var Lit Comb Let Free Or Case Branch typed

-- Auxiliary Functions

--- is expression a call of a function where all arguments are provided?
isFuncCall :: Expr -> Bool
isFuncCall e = isComb e && isCombTypeFuncCall (combType e)

--- is expression a partial function call?
isFuncPartCall :: Expr -> Bool
isFuncPartCall e = isComb e && isCombTypeFuncPartCall (combType e)

--- is expression a call of a constructor?
isConsCall :: Expr -> Bool
isConsCall e = isComb e && isCombTypeConsCall (combType e)

--- is expression a partial constructor call?
isConsPartCall :: Expr -> Bool
isConsPartCall e = isComb e && isCombTypeConsPartCall (combType e)

--- is expression fully evaluated?
isGround :: Expr -> Bool
isGround exp
  = case exp of
      Comb ConsCall _ args -> all isGround args
      _ -> isLit exp

--- get all variables (also pattern variables) in expression
allVars :: Expr -> [Int]
allVars e = trExpr (:) (const id) comb lt fr (.) cas branch const e []
 where
  comb _ _ = foldr (.) id
  lt bs exp = exp . foldr (.) id (map (\ (n,ns) -> (n:) . ns) bs)
  fr vs exp = (vs++) . exp
  cas _ exp bs = exp . foldr (.) id bs
  branch pat exp = ((args pat)++) . exp
  args pat | isConsPattern pat = patArgs pat
           | otherwise = []

--- rename all variables (also in patterns) in expression
rnmAllVars :: Update Expr Int
rnmAllVars f = trExpr (Var . f) Lit Comb lt (Free . map f) Or Case branch Typed
 where
   lt = Let . map (\ (n,exp) -> (f n,exp))
   branch = Branch . updPatArgs (map f)

--- update all qualified names in expression
updQNames :: Update Expr QName
updQNames f = trExpr Var Lit comb Let Free Or Case (Branch . updPatCons f) typed
 where
  comb ct name args = Comb ct (f name) args
  typed e ty = Typed e (updQNamesInTypeExpr f ty)

-- BranchExpr ----------------------------------------------------------------

--- transform branch expression
trBranch :: (Pattern -> Expr -> a) -> BranchExpr -> a
trBranch branch (Branch pat exp) = branch pat exp

-- Selectors

--- get pattern from branch expression
branchPattern :: BranchExpr -> Pattern
branchPattern = trBranch (\pat _ -> pat)

--- get expression from branch expression
branchExpr :: BranchExpr -> Expr
branchExpr = trBranch (\_ e -> e)

-- Update Operations

--- update branch expression
updBranch :: (Pattern -> Pattern) -> (Expr -> Expr) -> BranchExpr -> BranchExpr
updBranch fp fe = trBranch branch
 where
  branch pat exp = Branch (fp pat) (fe exp)

--- update pattern of branch expression
updBranchPattern :: Update BranchExpr Pattern
updBranchPattern f = updBranch f id

--- update expression of branch expression
updBranchExpr :: Update BranchExpr Expr
updBranchExpr = updBranch id

-- Pattern -------------------------------------------------------------------

--- transform pattern
trPattern :: (QName -> [Int] -> a) -> (Literal -> a) -> Pattern -> a
trPattern pattern _ (Pattern name args) = pattern name args
trPattern _ lpattern (LPattern l) = lpattern l

-- Selectors

--- get name from constructor pattern
patCons :: Pattern -> QName
patCons = trPattern (\name _ -> name) failed

--- get arguments from constructor pattern
patArgs :: Pattern -> [Int]
patArgs = trPattern (\_ args -> args) failed

--- get literal from literal pattern
patLiteral :: Pattern -> Literal
patLiteral = trPattern failed id

-- Test Operations

--- is pattern a constructor pattern?
isConsPattern :: Pattern -> Bool
isConsPattern = trPattern (\_ _ -> True) (\_ -> False)

-- Update Operations

--- update pattern
updPattern :: (QName -> QName) ->
              ([Int] -> [Int]) ->
              (Literal -> Literal) -> Pattern -> Pattern
updPattern fn fa fl = trPattern pattern lpattern
 where
  pattern name args = Pattern (fn name) (fa args)
  lpattern l = LPattern (fl l)

--- update constructors name of pattern
updPatCons :: (QName -> QName) -> Pattern -> Pattern
updPatCons f = updPattern f id id

--- update arguments of constructor pattern
updPatArgs :: ([Int] -> [Int]) -> Pattern -> Pattern
updPatArgs f = updPattern id f id

--- update literal of pattern
updPatLiteral :: (Literal -> Literal) -> Pattern -> Pattern
updPatLiteral f = updPattern id id f

-- Auxiliary Functions

--- build expression from pattern
patExpr :: Pattern -> Expr
patExpr = trPattern (\ name -> Comb ConsCall name . map Var) Lit
