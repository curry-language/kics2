----------------------------------------------------------------------------
--- This library provides transformation and update operations
--- on AbstractCurry programs.
--- Since the transformations are defined recursively on structured types,
--- they are useful to construct specific transformations on AbstractCurry
--- programs.
--- In particular, this library contains the transformation
--- `renameCurryModule` to rename an AbstractCurry module.
---
--- @author Michael Hanus
--- @version October 2016
--- @category meta
----------------------------------------------------------------------------

module AbstractCurry.Transform where

import AbstractCurry.Types
import AbstractCurry.Select

import Data.List (nub, union)

--- This type synonym is useful to denote the type of an update,
--- where the first argument is the type of values which are updated
--- by the local update (which acts on types described by the second argument).
type Update a b = (b -> b) -> a -> a

----------------------------------------------------------------------------
-- CurryProg

--- Transforms an AbstractCurry program.
trCProg :: (String -> [String] -> (Maybe CDefaultDecl) -> [CClassDecl]
        -> [CInstanceDecl] -> [CTypeDecl] -> [CFuncDecl] -> [COpDecl] -> a)
        -> CurryProg -> a
trCProg prog (CurryProg name imps dfltdecl clsdecls instdecls types funcs ops) =
  prog name imps dfltdecl clsdecls instdecls types funcs ops

--- Updates an AbstractCurry program.
updCProg :: (String      -> String)      ->
            ([String]    -> [String])    ->
            (Maybe CDefaultDecl -> Maybe CDefaultDecl) ->
            ([CClassDecl] -> [CClassDecl]) ->
            ([CInstanceDecl] -> [CInstanceDecl]) ->
            ([CTypeDecl] -> [CTypeDecl]) ->
            ([CFuncDecl] -> [CFuncDecl]) ->
            ([COpDecl]   -> [COpDecl])   -> CurryProg -> CurryProg
updCProg fn fi fdft fcl fci ft ff fo = trCProg prog
 where
  prog name imps dfltdecl clsdecls instdecls types funcs ops =
    CurryProg (fn name) (fi imps) (fdft dfltdecl) (fcl clsdecls) (fci instdecls)
              (ft types) (ff funcs) (fo ops)

--- Updates the name of a Curry program.
updCProgName :: Update CurryProg String
updCProgName f = updCProg f id id id id id id id

----------------------------------------------------------------------------
-- CDefaultDecl

--- Transforms a default declaration.
trCDefaultDecl :: ([CTypeExpr] -> a) -> CDefaultDecl -> a
trCDefaultDecl defdecl (CDefaultDecl texps) = defdecl texps

--- Updates a default declaration.
updCDefaultDecl :: ([CTypeExpr] -> [CTypeExpr])
                -> CDefaultDecl -> CDefaultDecl
updCDefaultDecl fts = trCDefaultDecl (\texps -> CDefaultDecl (fts texps))

----------------------------------------------------------------------------
-- CConstraint

--- Transforms a class context.
trCContext :: ([CConstraint] -> a) -> CContext -> a
trCContext ctxt (CContext constrs) = ctxt constrs

--- Updates a class context.
updCContext :: ([CConstraint] -> [CConstraint])
            -> CContext -> CContext
updCContext fc = trCContext (\constrs -> CContext (fc constrs))

----------------------------------------------------------------------------
-- CClassDecl

--- Transforms a class declaration.
trCClassDecl ::
  (QName -> CVisibility -> CContext -> CTVarIName -> [CFuncDecl] -> a)
  -> CClassDecl -> a
trCClassDecl cls (CClass name vis ctxt tvar funcs) =
  cls name vis ctxt tvar funcs

--- Updates an AbstractCurry program.
updCClassDecl :: (QName       -> QName)
              -> (CVisibility -> CVisibility)
              -> (CContext    -> CContext)
              -> (CTVarIName  -> CTVarIName)
              -> ([CFuncDecl] -> [CFuncDecl])
              -> CClassDecl -> CClassDecl
updCClassDecl fn fv fc ft ff = trCClassDecl cls
 where
  cls name vis ctxt tvar funcs =
    CClass (fn name) (fv vis) (fc ctxt) (ft tvar) (ff funcs)

----------------------------------------------------------------------------
-- CInstanceDecl

--- Transforms a class declaration.
trCInstanceDecl :: (QName -> CContext -> CTypeExpr -> [CFuncDecl] -> a)
                -> CInstanceDecl -> a
trCInstanceDecl inst (CInstance name ctxt texp funcs) =
  inst name ctxt texp funcs

--- Updates an AbstractCurry program.
updCInstanceDecl :: (QName       -> QName)
                 -> (CContext    -> CContext)
                 -> (CTypeExpr   -> CTypeExpr)
                 -> ([CFuncDecl] -> [CFuncDecl])
                 -> CInstanceDecl -> CInstanceDecl
updCInstanceDecl fn fc ft ff = trCInstanceDecl inst
 where
  inst name ctxt texp funcs =
    CInstance (fn name) (fc ctxt) (ft texp) (ff funcs)

----------------------------------------------------------------------------
-- CTypeDecl

--- Transforms a type declaration.
trCTypeDecl ::
    (QName -> CVisibility -> [CTVarIName] -> [CConsDecl] -> [QName] -> a)
 -> (QName -> CVisibility -> [CTVarIName] -> CTypeExpr   -> a)
 -> (QName -> CVisibility -> [CTVarIName] -> CConsDecl   -> [QName] -> a)
 -> CTypeDecl -> a
trCTypeDecl typ _ _   (CType name vis params cs dvs) =
 typ name vis params cs dvs
trCTypeDecl _ tsyn _  (CTypeSyn name vis params syn) = tsyn  name vis params syn
trCTypeDecl _ _ tntyp (CNewType name vis params nt dvs) =
 tntyp name vis params nt dvs

--- update type declaration
updCTypeDecl :: (QName -> QName)
             -> (CVisibility  -> CVisibility)
             -> ([CTVarIName] -> [CTVarIName])
             -> ([CConsDecl]  -> [CConsDecl])
             -> (CTypeExpr    -> CTypeExpr)
             -> (CConsDecl    -> CConsDecl)
             -> ([QName]      -> [QName])
             -> CTypeDecl -> CTypeDecl
updCTypeDecl fn fv fp fc fs ft fd = trCTypeDecl typ tsyn tntyp
 where
  typ   name vis params cs der =
    CType    (fn name) (fv vis) (fp params) (fc cs) (fd der)
  tsyn  name vis params syn  = CTypeSyn (fn name) (fv vis) (fp params) (fs syn)
  tntyp name vis params ntyp der =
    CNewType (fn name) (fv vis) (fp params) (ft ntyp) (fd der)

--- Updates the name of a type declaration.
updCTypeDeclName :: Update CTypeDecl QName
updCTypeDeclName f = updCTypeDecl f id id id id id id


----------------------------------------------------------------------------
-- CConsDecl

--- Transforms a constructor declaration.
trCConsDecl ::
     ([CTVarIName] -> CContext -> QName -> CVisibility -> [CTypeExpr]  -> a)
  -> ([CTVarIName] -> CContext -> QName -> CVisibility -> [CFieldDecl] -> a)
  -> CConsDecl -> a
trCConsDecl cons _ (CCons   qtvars ctxt name vis args) =
  cons qtvars ctxt name vis args
trCConsDecl _ rec  (CRecord qtvars ctxt name vis args) =
  rec  qtvars ctxt name vis args

--- Updates a constructor declaration.
updCConsDecl :: ([CTVarIName] -> [CTVarIName])
             -> (CContext -> CContext)
             -> (QName -> QName)
             -> (CVisibility  -> CVisibility)
             -> ([CTypeExpr]  -> [CTypeExpr])
             -> ([CFieldDecl] -> [CFieldDecl])
             -> CConsDecl -> CConsDecl
updCConsDecl fqv fc fn fv fts ffs = trCConsDecl cons rec
 where
  cons qtvars ctxt name vis args =
    CCons   (fqv qtvars) (fc ctxt) (fn name) (fv vis) (fts args)
  rec  qtvars ctxt name vis args =
    CRecord (fqv qtvars) (fc ctxt) (fn name) (fv vis) (ffs args)

--- Updates the name of a constructor declaration.
updCConsDeclName :: Update CConsDecl QName
updCConsDeclName f = updCConsDecl id id f id id id

----------------------------------------------------------------------------
-- CFieldDecl

--- Transforms a constructor declaration.
trCFieldDecl :: (QName -> CVisibility -> CTypeExpr  -> a)
             -> CFieldDecl -> a
trCFieldDecl field (CField name vis texp) = field name vis texp

--- update constructor declaration
updCFieldDecl :: (QName -> QName)
              -> (CVisibility -> CVisibility)
              -> (CTypeExpr   -> CTypeExpr)
              -> CFieldDecl -> CFieldDecl
updCFieldDecl fn fv ft = trCFieldDecl field
 where
  field name vis texp = CField (fn name) (fv vis) (ft texp)

--- Updates the name of a constructor declaration.
updCFieldDeclName :: Update CFieldDecl QName
updCFieldDeclName f = updCFieldDecl f id id

----------------------------------------------------------------------------
-- CQualTypeExpr

--- Transforms a default declaration.
trCQualTypeExpr :: (CContext -> CTypeExpr -> a) -> CQualTypeExpr -> a
trCQualTypeExpr qtexp (CQualType ctxt texp) = qtexp ctxt texp

--- Updates a default declaration.
updCQualTypeExpr :: (CContext  -> CContext)
                 -> (CTypeExpr -> CTypeExpr)
                 -> CQualTypeExpr -> CQualTypeExpr
updCQualTypeExpr fc ft =
  trCQualTypeExpr (\ctxt texp -> CQualType (fc ctxt) (ft texp))

----------------------------------------------------------------------------
-- CTypeExpr

--- Transforms a type expression.
trCTypeExpr :: (CTVarIName -> a)
            -> (QName -> a)
            -> (a -> a -> a)
            -> (a -> a -> a)
            -> CTypeExpr -> a
trCTypeExpr tvar tcons functype applytype texp = trTE texp
 where
  trTE (CTVar n)           = tvar n
  trTE (CTCons name)       = tcons name
  trTE (CFuncType from to) = functype  (trTE from) (trTE to)
  trTE (CTApply   from to) = applytype (trTE from) (trTE to)

--- Updates all type constructors in a type expression.
updTConsApp :: (QName -> CTypeExpr) -> CTypeExpr -> CTypeExpr
updTConsApp tcons = trCTypeExpr CTVar tcons CFuncType CTApply

----------------------------------------------------------------------------
-- COpDecl

--- Transforms an operator declaration.
trCOpDecl :: (QName -> CFixity -> Int -> a) -> COpDecl -> a
trCOpDecl op (COp name fix prec) = op name fix prec

--- Updates an operator declaration.
updCOpDecl :: (QName -> QName) -> (CFixity -> CFixity) -> (Int -> Int)
           -> COpDecl -> COpDecl
updCOpDecl fn ff fp = trCOpDecl op
 where
  op name fix prec = COp (fn name) (ff fix) (fp prec)

--- Updates the name of an operator declaration.
updCOpName :: Update COpDecl QName
updCOpName f = updCOpDecl f id id

----------------------------------------------------------------------------
-- CFuncDecl

--- Transforms a function declaration
trCFuncDecl ::
    (String -> QName -> Int -> CVisibility -> CQualTypeExpr -> [CRule] -> a)
    -> CFuncDecl -> a
trCFuncDecl func (CFunc      name arity vis t rs) = func "" name arity vis t rs
trCFuncDecl func (CmtFunc cm name arity vis t rs) = func cm name arity vis t rs

--- Updates a function declaration.
updCFuncDecl :: (String -> String)
             -> (QName -> QName)
             -> (Int -> Int)
             -> (CVisibility -> CVisibility)
             -> (CQualTypeExpr -> CQualTypeExpr)
             -> ([CRule] -> [CRule])
             -> CFuncDecl -> CFuncDecl
updCFuncDecl fc fn fa fv ft fr = trCFuncDecl func
 where 
  func cmt name arity vis t rules =
    if null cmt
    then CFunc (fn name) (fa arity) (fv vis) (ft t) (fr rules)
    else CmtFunc (fc cmt) (fn name) (fa arity) (fv vis) (ft t) (fr rules)

----------------------------------------------------------------------------
-- CRule

--- Transform a rule.
trCRule :: ([CPattern] -> CRhs -> a) -> CRule -> a
trCRule rule (CRule pats rhs) = rule pats rhs

--- Update a rule.
updCRule :: ([CPattern] -> [CPattern])
         -> (CRhs -> CRhs)
         -> CRule -> CRule
updCRule fp fr = trCRule rule
 where
  rule pats rhs = CRule (fp pats) (fr rhs)

----------------------------------------------------------------------------
-- CRhs

--- Transforms a right-hand side (of a rule or case expression).
trCRhs :: (CExpr -> [CLocalDecl] -> a)
       -> ([(CExpr, CExpr)] -> [CLocalDecl] -> a)
       -> CRhs -> a
trCRhs srhs _ (CSimpleRhs  exp   locals) = srhs exp locals
trCRhs _ grhs (CGuardedRhs gexps locals) = grhs gexps locals

--- Updates right-hand side.
updCRhs :: (CExpr -> CExpr)
        -> ([(CExpr, CExpr)] -> [(CExpr, CExpr)])
        -> ([CLocalDecl]     -> [CLocalDecl])
        -> CRhs -> CRhs
updCRhs fe fg fl = trCRhs srhs grhs
 where
  srhs exp   locals = CSimpleRhs (fe exp) (fl locals)
  grhs gexps locals = CGuardedRhs (fg gexps) (fl locals)

----------------------------------------------------------------------------
-- CLocalDecl

--- Transforms a local declaration.
trCLocalDecl :: (CFuncDecl -> a)
             -> (CPattern -> CRhs -> a)
             -> ([CVarIName] -> a)
             -> CLocalDecl -> a
trCLocalDecl lfun _ _ (CLocalFunc fdecl)  = lfun fdecl
trCLocalDecl _ lpat _ (CLocalPat pat rhs) = lpat pat rhs
trCLocalDecl _ _ vars (CLocalVars vs)     = vars vs

--- Updates a local declaration.
updCLocalDecl :: (CFuncDecl   -> CFuncDecl)
              -> (CPattern    -> CPattern)
              -> (CRhs        -> CRhs)
              -> ([CVarIName] -> [CVarIName])
              -> CLocalDecl   -> CLocalDecl
updCLocalDecl ff fp fr fv = trCLocalDecl lfun lpat lvars
 where
  lfun fdecl   = CLocalFunc (ff fdecl)
  lpat pat rhs = CLocalPat (fp pat) (fr rhs)
  lvars vars   = CLocalVars (fv vars)

----------------------------------------------------------------------------
-- CPattern

--- Transforms a pattern.
trCPattern :: (CVarIName -> a)
           -> (CLiteral -> a)
           -> (QName -> [a] -> a)
           -> (CVarIName -> a -> a)
           -> (QName -> [a] -> a)
           -> (QName -> [CField a] -> a)
           -> CPattern -> a
trCPattern fv fl fc fa ff fr pattern = trP pattern
 where
  trP (CPVar pvar)         = fv pvar
  trP (CPLit lit)          = fl lit
  trP (CPComb c pats)      = fc c (map trP pats)
  trP (CPAs v pat)         = fa v (trP pat)
  trP (CPFuncComb fn pats) = ff fn (map trP pats)
  trP (CPLazy pat)         = trP pat
  trP (CPRecord r fs)      = fr r (map (\(n,p) -> (n,trP p)) fs)

--- Updates a pattern.
updCPattern :: (CVarIName   -> CVarIName)
            -> (CLiteral    -> CLiteral)
            -> (QName       -> QName)
            -> CPattern   -> CPattern
updCPattern fv fl fn = trCPattern pvar plit pcomb pas pfcomb prec
 where
  pvar var = CPVar (fv var)
  plit lit = CPLit (fl lit)
  pcomb c pats = CPComb (fn c) (map (updCPattern fv fl fn) pats)
  pas v pat = CPAs (fv v) (updCPattern fv fl fn pat)
  pfcomb f pats = CPFuncComb (fn f) (map (updCPattern fv fl fn) pats)
  prec r fields = CPRecord (fn r)
                    (map (\ (n,p) -> (fn n, updCPattern fv fl fn p)) fields)

----------------------------------------------------------------------------
-- CExpr

--- Transforms an expression.
trExpr :: (CVarIName -> a)
       -> (CLiteral -> a)
       -> (QName -> a)
       -> (a -> a -> a)
       -> ([CPattern] -> a -> a)
       -> ([CLocalDecl] -> a -> a)
       -> ([CStatement] -> a)
       -> (a -> [CStatement] -> a)
       -> (CCaseType -> a -> [(CPattern, CRhs)] -> a)
       -> (a -> CQualTypeExpr -> a)
       -> (QName -> [CField a] -> a)
       -> (a -> [CField a] -> a)
       -> CExpr -> a
trExpr var lit sym app lam clet cdo lcomp cas typ rcon rupd exp = trE exp
 where
  trE (CVar n) = var n
  trE (CLit l) = lit l
  trE (CSymbol n) = sym n
  trE (CApply e1 e2) = app (trE e1) (trE e2)
  trE (CLambda pats e) = lam pats (trE e)
  trE (CLetDecl ls e) = clet ls (trE e)
  trE (CDoExpr stm) = cdo stm
  trE (CListComp e stm) = lcomp (trE e) stm
  trE (CCase ct e branches) = cas ct (trE e) branches
  trE (CTyped e te) = typ (trE e) te
  trE (CRecConstr rn fds) = rcon rn (map (\ (lb,e) -> (lb, trE e)) fds)
  trE (CRecUpdate e  fds) = rupd (trE e) (map (\ (lb,v) -> (lb, trE v)) fds)
 
----------------------------------------------------------------------------
-- CStatement

--- Transforms a statement (occuring in do expressions or list comprehensions).
trCStatement :: (CExpr -> a)
             -> (CPattern -> CExpr -> a)
             -> ([CLocalDecl] -> a)
             -> CStatement -> a
trCStatement sexp _ _ (CSExpr exp)    = sexp exp
trCStatement _ spat _ (CSPat pat exp) = spat pat exp
trCStatement _ _ slet (CSLet locals)  = slet locals

--- Updates a statement (occuring in do expressions or list comprehensions).
updCStatement :: (CExpr      -> CExpr)
              -> (CPattern   -> CPattern)
              -> (CLocalDecl -> CLocalDecl)
              -> CStatement  -> CStatement
updCStatement fe fp fd = trCStatement sexp spat slet
 where
  sexp exp     = CSExpr (fe exp)
  spat pat exp = CSPat (fp pat) (fe exp)
  slet locals  = CSLet (map fd locals)

----------------------------------------------------------------------------
--- Renames a Curry module, i.e., updates the module name and all qualified
--- names in a program.
renameCurryModule :: String -> CurryProg -> CurryProg
renameCurryModule newname prog =
  updCProgName (const newname) (updQNamesInCProg rnm prog)
 where
  rnm mn@(mod,n) | mod == progName prog = (newname,n)
                 | otherwise            = mn

--- Updates all qualified names in a Curry program.
updQNamesInCProg :: Update CurryProg QName
updQNamesInCProg f =
  updCProg id
           id 
           (updQNamesInCDefaultDecl f)
           (map (updQNamesInCClassDecl f))
           (map (updQNamesInCInstanceDecl f))
           (map (updQNamesInCTypeDecl f))
           (map (updQNamesInCFuncDecl f))
           (map (updCOpName f))

--- Updates all qualified names in a default declaration.
updQNamesInCDefaultDecl :: Update (Maybe CDefaultDecl) QName
updQNamesInCDefaultDecl f = updateDefltDecl
 where
   updateDefltDecl Nothing = Nothing
   updateDefltDecl (Just defdecl) =
     Just (updCDefaultDecl (map (updQNamesInCTypeExpr f)) defdecl)

--- Updates all qualified names in a class declaration.
updQNamesInCClassDecl :: Update CClassDecl QName
updQNamesInCClassDecl f =
  updCClassDecl f id (updQNamesInCContext f) id
                (map (updQNamesInCFuncDecl f))

--- Updates all qualified names in an instance declaration.
updQNamesInCInstanceDecl :: Update CInstanceDecl QName
updQNamesInCInstanceDecl f =
  updCInstanceDecl f
                   (updQNamesInCContext f)
                   (updQNamesInCTypeExpr f)
                   (map (updQNamesInCFuncDecl f))

--- Updates all qualified names in a type declaration.
updQNamesInCTypeDecl :: Update CTypeDecl QName
updQNamesInCTypeDecl f =
  updCTypeDecl f id id
               (map (updQNamesInCConsDecl f))
               (updQNamesInCTypeExpr f)
               (updQNamesInCConsDecl f)
               (map f)

--- Updates all qualified names in a constructor declaration.
updQNamesInCConsDecl :: Update CConsDecl QName
updQNamesInCConsDecl f =
  updCConsDecl id (updQNamesInCContext f) f id
               (map (updQNamesInCTypeExpr f))
               (map (updQNamesInCFieldDecl f))

--- Updates all qualified names in a constructor declaration.
updQNamesInCContext :: Update CContext QName
updQNamesInCContext f = updCContext (map updConstr)
 where
  updConstr (n,texp) = (f n, updQNamesInCTypeExpr f texp)

--- Updates all qualified names in a record field declaration.
updQNamesInCFieldDecl :: Update CFieldDecl QName
updQNamesInCFieldDecl f = updCFieldDecl f id (updQNamesInCTypeExpr f)

--- Updates all qualified names in a type expression.
updQNamesInCQualTypeExpr :: Update CQualTypeExpr QName
updQNamesInCQualTypeExpr f =
  updCQualTypeExpr (updQNamesInCContext f) (updQNamesInCTypeExpr f)

--- Updates all qualified names in a type expression.
updQNamesInCTypeExpr :: Update CTypeExpr QName
updQNamesInCTypeExpr f = updTConsApp (CTCons . f)

--- Updates all qualified names in a function declaration.
updQNamesInCFuncDecl :: Update CFuncDecl QName
updQNamesInCFuncDecl f =
  updCFuncDecl id f id id
               (updQNamesInCQualTypeExpr f)
               (map (updQNamesInCRule f))

--- Updates all qualified names in a function declaration.
updQNamesInCRule :: Update CRule QName
updQNamesInCRule f =
  updCRule (map (updQNamesInCPattern f))
           (updQNamesInCRhs f)

--- Updates all qualified names in a function declaration.
updQNamesInCRhs :: Update CRhs QName
updQNamesInCRhs f =
  updCRhs (updQNamesInCExpr f)
          (map (\ (g,e) -> (updQNamesInCExpr f g, updQNamesInCExpr f e)))
          (map (updQNamesInCLocalDecl f))

--- Updates all qualified names in a function declaration.
updQNamesInCLocalDecl :: Update CLocalDecl QName
updQNamesInCLocalDecl f =
  updCLocalDecl (updQNamesInCFuncDecl f)
                (updQNamesInCPattern f)
                (updQNamesInCRhs f)
                id

--- Updates all qualified names in a function declaration.
updQNamesInCPattern :: Update CPattern QName
updQNamesInCPattern f = updCPattern id id f

--- Updates all qualified names in a statement.
updQNamesInCStatement :: Update CStatement QName
updQNamesInCStatement f =
  updCStatement (updQNamesInCExpr f)
                (updQNamesInCPattern f)
                (updQNamesInCLocalDecl f)

updQNamesInCExpr :: Update CExpr QName
updQNamesInCExpr f =
  trExpr CVar CLit (CSymbol . f) CApply lam ldecl doexp lcomp ccase ctyped
         reccon recupd
 where
  lam pats exp = CLambda (map (updQNamesInCPattern f) pats) exp
  ldecl locals exp = CLetDecl (map (updQNamesInCLocalDecl f) locals) exp
  doexp stms = CDoExpr (map (updQNamesInCStatement f) stms)
  lcomp exp stms = CListComp exp (map (updQNamesInCStatement f) stms)
  ccase ct exp bs = CCase ct exp
    (map (\ (pat,rhs) -> (updQNamesInCPattern f pat, updQNamesInCRhs f rhs)) bs)
  ctyped exp texp = CTyped exp (updQNamesInCQualTypeExpr f texp)
  reccon rec fields = CRecConstr (f rec) (map (\ (l,e) -> (f l,e)) fields)
  recupd exp fields = CRecUpdate exp (map (\ (l,e) -> (f l,e)) fields)
  
-------------------------------------------------------------------------
--- Extracts all type names occurring in a program.
typesOfCurryProg :: CurryProg -> [QName]
typesOfCurryProg =
  trCProg (\_ _ dfts cls insts types funcs _ ->
              typesOfDefault dfts ++
              unionMap typesOfCClassDecl    cls   ++
              unionMap typesOfCInstanceDecl insts ++
              unionMap typesOfCTypeDecl     types ++
              unionMap typesOfCFuncDecl     funcs)
 where
  typesOfDefault Nothing = []
  typesOfDefault (Just (CDefaultDecl texps)) = concatMap typesOfTypeExpr texps

--- Extracts all type names occurring in a class declaration.
--- Class names are ignored.
typesOfCClassDecl :: CClassDecl -> [QName]
typesOfCClassDecl =
  trCClassDecl (\_ _ ctxt _ funcs -> typesOfContext ctxt ++
                     unionMap typesOfCFuncDecl funcs)

--- Extracts all type names occurring in a class declaration.
--- Class names are ignored.
typesOfCInstanceDecl :: CInstanceDecl -> [QName]
typesOfCInstanceDecl =
  trCInstanceDecl (\_ ctxt texp funcs -> typesOfContext ctxt ++
                     typesOfTypeExpr texp ++
                     unionMap typesOfCFuncDecl funcs)

--- Extracts all type names occurring in a type declaration.
--- Class names are ignored.
typesOfCTypeDecl :: CTypeDecl -> [QName]
typesOfCTypeDecl =
  trCTypeDecl (\qn _ _ cdecls _ -> qn : concatMap typesOfConsDecl cdecls)
              (\qn _ _ texp     -> qn : typesOfTypeExpr texp)
              (\qn _ _ cdecl  _ -> qn : typesOfConsDecl cdecl)

typesOfConsDecl :: CConsDecl -> [QName]
typesOfConsDecl =
  trCConsDecl (\_ ctxt _ _ texps   -> typesOfContext ctxt ++
                                      concatMap typesOfTypeExpr texps)
              (\_ ctxt _ _ fddecls -> typesOfContext ctxt ++
                                      concatMap typesOfFieldDecl fddecls)

typesOfFieldDecl :: CFieldDecl -> [QName]
typesOfFieldDecl = trCFieldDecl (\_ _ texp -> typesOfTypeExpr texp)

typesOfContext :: CContext -> [QName]
typesOfContext = trCContext (concatMap (typesOfTypeExpr . snd))

typesOfTypeExpr :: CTypeExpr -> [QName]
typesOfTypeExpr = trCTypeExpr (\_ -> [])
                              (\qn -> [qn])
                              (++)
                              (++)

typesOfQualTypeExpr :: CQualTypeExpr -> [QName]
typesOfQualTypeExpr =
  trCQualTypeExpr (\ctxt texp -> typesOfContext ctxt ++ typesOfTypeExpr texp)

typesOfCFuncDecl :: CFuncDecl -> [QName]
typesOfCFuncDecl =
  trCFuncDecl (\_ _ _ _ texp _ -> typesOfQualTypeExpr texp)
  -- type annotations in expressions are currently ignored

-- Map a list-valued function on a list and remove duplicates.
unionMap :: Eq b => (a -> [b]) -> [a] -> [b]
unionMap f = foldr union [] . (map (nub . f))

----------------------------------------------------------------------------
--- Extracts all function (and constructor) names occurring in a program.
funcsOfCurryProg :: CurryProg -> [QName]
funcsOfCurryProg =
  trCProg (\_ _ _ cls insts types funcs _ ->
              unionMap funcsOfCClassDecl    cls   ++
              unionMap funcsOfCInstanceDecl insts ++
              unionMap funcsOfCTypeDecl types ++
              unionMap funcsOfCFuncDecl funcs)

funcsOfCClassDecl :: CClassDecl -> [QName]
funcsOfCClassDecl =
  trCClassDecl (\_ _ _ _ funcs -> unionMap funcsOfCFuncDecl funcs)

funcsOfCInstanceDecl :: CInstanceDecl -> [QName]
funcsOfCInstanceDecl =
  trCInstanceDecl (\_ _ _ funcs -> unionMap funcsOfCFuncDecl funcs)

funcsOfCTypeDecl :: CTypeDecl -> [QName]
funcsOfCTypeDecl =
  trCTypeDecl (\_ _ _ cdecls _ -> concatMap funcsOfConsDecl cdecls)
              (\_ _ _ _        -> [])
              (\_ _ _ cdecl  _ -> funcsOfConsDecl cdecl)

funcsOfConsDecl :: CConsDecl -> [QName]
funcsOfConsDecl =
  trCConsDecl (\_ _ qn _ _       -> [qn])
              (\_ _ qn _ fddecls -> qn : concatMap funcsOfFieldDecl fddecls)

funcsOfFieldDecl :: CFieldDecl -> [QName]
funcsOfFieldDecl = trCFieldDecl (\qn _ _ -> [qn])

--- Extracts all function (and constructor) names occurring in a function
--- declaration.
funcsOfCFuncDecl :: CFuncDecl -> [QName]
funcsOfCFuncDecl =
  trCFuncDecl (\_ _ _ _ _ rules -> concatMap funcsOfCRule rules)

funcsOfCRule :: CRule -> [QName]
funcsOfCRule = trCRule (\_ rhs -> funcsOfCRhs rhs)

funcsOfCRhs :: CRhs -> [QName]
funcsOfCRhs =
  trCRhs (\e  ldecls -> funcsOfExpr e ++ concatMap funcsOfLDecl ldecls)
         (\gs ldecls -> concatMap (\ (g,e) -> funcsOfExpr g ++ funcsOfExpr e) gs
                        ++ concatMap funcsOfLDecl ldecls)

funcsOfLDecl :: CLocalDecl -> [QName]
funcsOfLDecl = trCLocalDecl funcsOfCFuncDecl (const funcsOfCRhs) (const [])

funcsOfExpr :: CExpr -> [QName]
funcsOfExpr =
  trExpr (const [])
         (const [])
         (\n -> [n])
         (++)
         (const id)
         (\ldecls e -> concatMap funcsOfLDecl ldecls ++ e)
         (concatMap funcsOfStat)
         (\e stats -> e ++ concatMap funcsOfStat stats)
         (\_ e brs -> e ++ concatMap (funcsOfCRhs . snd) brs)
         (\e _ -> e)
         (\_ fields -> concatMap snd fields)
         (\e fields -> e ++ concatMap snd fields)
         
funcsOfStat :: CStatement -> [QName]
funcsOfStat = trCStatement funcsOfExpr
                           (const funcsOfExpr)
                           (concatMap funcsOfLDecl)

-------------------------------------------------------------------------
