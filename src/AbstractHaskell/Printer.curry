------------------------------------------------------------------------------
--- A pretty printer for AbstractHaskell programs.
---
--- This library defines a function "ppProg" that shows
--- an AbstractHaskell program in standard Haskell syntax.
---
--- @author Björn Peemöller
--- @version May 2017
------------------------------------------------------------------------------
module AbstractHaskell.Printer
  ( Options (..), defaultOptions, pPrint, ppProg, ppHeader, ppImports
  , ppDecls
  ) where

import Text.Pretty

import AbstractHaskell.Types
import AbstractHaskell.Goodies (tyVarsOf)

data Options = Options
  { currentModule :: String
  , qualImpModule :: String -> Bool -- should a module be qualified imported?
  , traceFailure  :: Bool
  , kics2Mode     :: Bool
  }

defaultOptions :: Options
defaultOptions = Options
  { currentModule = ""
  , qualImpModule = const False
  , traceFailure = False
  , kics2Mode = False
  }

pPrint :: Doc -> String
pPrint = showWidth 80

-- ---------------------------------------------------------------------------
-- Functions to print an AbstractHaskell program in standard Haskell syntax
-- ---------------------------------------------------------------------------

--- Shows an AbstractHaskell program in standard Haskell syntax.
--- The export list contains the public functions and the
--- types with their data constructors (if all data constructors are public),
--- otherwise only the type constructors.
--- The potential comments in function declarations are formatted as
--- documentation comments.
ppProg :: Options ->  Prog -> Doc
ppProg opts (Prog m is ts fs os) = compose (<$+$>)
  [ ppHeader  opts' m ts fs
  , ppImports opts' is
  , ppDecls   opts' os ts fs
  ]
 where opts' = opts { currentModule = m }

ppHeader :: Options -> String -> [TypeDecl] -> [FuncDecl] -> Doc
ppHeader opts m ts fs = indent $ sep
  [ text "module" <+> text (if traceFailure opts then addTrace m else m)
  , ppExports opts ts fs
  , text "where"
  ]

ppDecls :: Options -> [OpDecl] -> [TypeDecl] -> [FuncDecl] -> Doc
ppDecls opts os ts fs = compose (<$+$>)
  [ ppOpDecls        os
  , ppTypeDecls opts ts
  , ppFuncDecls opts fs
  ]

-- ---------------------------------------------------------------------------
-- Module Header
-- ---------------------------------------------------------------------------

--- Create the export specification for a list of types and a list of functions.
--- Note that for types all constructors are exported regardless of the Haskell
--- export specification (= the visibility information of the constructors)
--- because record update expressions have been previously desugared into
--- case expressions mentioning all constructors belonging to the set of labels
--- in the update. While the record update expression is valid in Curry even if
--- the constructors are not imported (they are, at least implicitly), the case
--- expression is only valid if all mentioned constructors are exported.
--- Therefore, to avoid any GHC errors, we simply export all constructors.
--- This should be no problem since imported entities are always used fully
--- qualified after the translation process.
--- (bjp, jrt 2015-03-04)
ppExports :: Options -> [TypeDecl] -> [FuncDecl] -> Doc
ppExports opts ts fs = tupledSpaced $ filter (not . isEmpty)
                     $ map ppTypeExport ts ++ map ppFuncExport fs
 where
  ppTypeExport :: TypeDecl -> Doc
  ppTypeExport (Type     qn vis _ _)
    | vis == Public = ppQName opts qn <+> text "(..)"
    | otherwise     = Text.Pretty.empty
  ppTypeExport (TypeSyn  qn vis _ _)
    | vis == Public = ppQName opts qn
    | otherwise     = Text.Pretty.empty
  ppTypeExport (Instance _  _   _ _) = Text.Pretty.empty

  ppFuncExport :: FuncDecl -> Doc
  ppFuncExport (Func _ qn _ vis _ _)
    | vis == Public = ppPrefixQOp opts qn
    | otherwise     = Text.Pretty.empty

-- ---------------------------------------------------------------------------
-- Imports + infix operator declarations
-- ---------------------------------------------------------------------------

ppImports :: Options -> [String] -> Doc
ppImports opts = vsep . map (ppImport opts)

ppImport :: Options -> String -> Doc
ppImport opts imp
    -- Import module qualified if required:
  | qualImpModule opts imp = indent $ fillSep $ map text
                               ["import", "qualified", imp]
  | traceFailure opts      = indent $ fillSep $ map text $
                               ["import", "qualified", addTrace imp, "as", imp]
  | otherwise              = indent $ text "import" <+> text imp

ppOpDecls :: [OpDecl] -> Doc
ppOpDecls = vsep . map ppOpDecl

ppOpDecl :: OpDecl -> Doc
ppOpDecl (Op qn fix prec) = ppFixity fix <+> int prec <+> ppInfixOp (snd qn)

ppFixity :: Fixity -> Doc
ppFixity InfixOp  = text "infix"
ppFixity InfixlOp = text "infixl"
ppFixity InfixrOp = text "infixr"

-- ---------------------------------------------------------------------------
-- Type declarations
-- ---------------------------------------------------------------------------

--- pretty-print a list of AbstractHaskell type declarations
ppTypeDecls :: Options -> [TypeDecl] -> Doc
ppTypeDecls opts = compose (<$+$>) . map (ppTypeDecl opts)

--- pretty-print an AbstractHaskell type declaration
ppTypeDecl :: Options -> TypeDecl -> Doc
ppTypeDecl opts (TypeSyn qname _ vs ty) = indent $
   text "type" <+> ppName qname <+> fillSep (map ppTypeVar vs)
               </> equals <+> ppTypeExp opts ty
ppTypeDecl opts (Type    qname _ vs cs)
  | null cs   = Text.Pretty.empty
  | otherwise = indent $
    (text "data" <+> ppName qname <+> fillSep (map ppTypeVar vs))
    $$ ppConsDecls opts cs
ppTypeDecl opts (Instance qname ty ctxts rs) = indent $
  text "instance" <+> ppContexts opts ctxts
    <+> ppQName opts qname <+> ppTypeExpr opts 2 ty
    <+> (text "where" $$ vsep (map ppInstRule rs))
 where ppInstRule ((_, f), r) = ppRule opts f  r

--- pretty-print the constructor declarations
ppConsDecls :: Options -> [ConsDecl] -> Doc
ppConsDecls o cs = vsep $ zipWith (<+>) (equals : repeat bar)
                                        (map (ppConsDecl o) cs)

--- pretty print a single constructor declaration
ppConsDecl :: Options -> ConsDecl -> Doc
ppConsDecl o (Cons (_, qn) _ _ tys) = indent $ fillSep
                                    $ ppPrefixOp qn : map (ppTypeExpr o 2) tys

ppContexts :: Options -> [Context] -> Doc
ppContexts opts cs
  | null cs   = Text.Pretty.empty
  | otherwise = tupled (map (ppContext opts) cs) <+> doubleArrow

ppContext :: Options -> Context -> Doc
ppContext opts (Context tvs cxs qn ts) = quantifiedVars <+> ppContexts opts cxs
                                                        <+> ppTypeExp opts (TCons qn ts)
  where quantifiedVars | null tvs  = Text.Pretty.empty
                       | otherwise = text "forall" <+> fillSep (map ppTypeVar tvs) <+> dot

--- pretty a top-level type expression
ppTypeExp :: Options -> TypeExpr -> Doc
ppTypeExp o = ppTypeExpr o 0

--- Shows an AbstractHaskell type expression in standard Haskell syntax.
ppTypeExpr :: Options -> Int -> TypeExpr -> Doc
ppTypeExpr _ _ (TVar           v) = ppTypeVar v
ppTypeExpr o p (FuncType   t1 t2) = parensIf (p > 0) $
  ppTypeExpr o 1 t1 </> rarrow <+> ppTypeExp o t2
ppTypeExpr o p (TCons     qn tys)
  | isList qn && length tys == 1 = brackets (ppTypeExp o (head tys))
  | isTuple qn                   = tupled (map (ppTypeExp o) tys)
  | otherwise                    = parensIf (p > 1 && not (null tys))
                                 $ fillSep
                                 $ ppQName o qn : map (ppTypeExpr o 2) tys
ppTypeExpr o p (ForallType vs cx t) = parensIf (p > 0) $ text "forall"
  <+> fillSep (map (ppTypeVar . fst) vs) <+> dot
  <+> ppContexts o cx <+> ppTypeExp o t

ppTypeVar :: TVarIName -> Doc
ppTypeVar (_, name) = text name

-- ---------------------------------------------------------------------------
-- Function Declaration
-- ---------------------------------------------------------------------------

ppFunc :: FuncDecl -> Doc
ppFunc = ppFuncDecl defaultOptions

ppFuncDecls :: Options -> [FuncDecl] -> Doc
ppFuncDecls opts = compose (<$+$>) . map (ppFuncDecl opts)

ppFuncDecl :: Options -> FuncDecl -> Doc
ppFuncDecl opts (Func cmt (_,name) _ _ ty (Rules rs))
  =  ppComment cmt
  $$ indent (ppTypeSig opts name ty)
  $$ vsep (map (ppRule opts name) rs)
ppFuncDecl _    (Func _ _ _ _ _ External) = Text.Pretty.empty

ppComment :: String -> Doc
ppComment = vsep . map (\c -> text "---" <+> text c) . lines

--- Shows an AbstractHaskell type signature of a given function name.
ppTypeSig :: Options -> String -> TypeSig -> Doc
ppTypeSig _    _ Untyped         = Text.Pretty.empty
ppTypeSig opts f (CType ctxt ty) = hsep [ ppPrefixOp f, doubleColon
                                        , ppScopedTyVars ty
                                        , ppContexts opts ctxt
                                        , ppTypeExp opts ty
                                        ]

ppScopedTyVars :: TypeExpr -> Doc
ppScopedTyVars ty
  | null tvs  = Text.Pretty.empty
  | otherwise = text "forall" <+> fillSep (map ppTypeVar tvs) <+> dot
  where tvs = tyVarsOf ty

ppRule :: Options -> String -> Rule -> Doc
ppRule opts f (Rule ps rhs ds) = indent $
  hsep (ppPrefixOp f : map (ppPattern opts 1) ps)
  <+> equals </> ppRhs opts rhs $$ ppLocalDecls opts ds

ppRhs :: Options -> Rhs -> Doc
ppRhs opts (SimpleRhs   e) = ppExpr opts 0 e
ppRhs opts (GuardedRhs gs) = indent $ vsep (map (ppCond opts) gs)

ppCond :: Options -> (Expr, Expr) -> Doc
ppCond opts (c, e) = bar <+> ppExpr opts 0 c </> equals <+> ppExpr opts 0 e

ppLocalDecls :: Options -> [LocalDecl] -> Doc
ppLocalDecls opts ds
  | null ds   = Text.Pretty.empty
  | otherwise = text "where" $$ ppBlock opts ds

ppBlock :: Options -> [LocalDecl] -> Doc
ppBlock opts ds = align ((compose (<$!$>)) (map (ppLocalDecl opts) ds))

ppLocalDecl :: Options -> LocalDecl -> Doc
ppLocalDecl opts (LocalFunc     f) = ppFuncDecl opts f
ppLocalDecl opts (LocalPat p e ds) = indent $
  ppPattern opts 0 p <+> equals <+> ppExpr opts 0 e
  $$ ppLocalDecls opts ds

ppExp :: Expr -> Doc
ppExp = ppExpr defaultOptions 0

ppExpr :: Options -> Int -> Expr -> Doc
ppExpr _    _ (Var               v) = ppVar v
ppExpr _    _ (Lit               l) = ppLiteral l
ppExpr opts _ (Symbol           op) = ppPrefixQOp opts op
ppExpr opts p (Apply         e1 e2) = parensIf (p > 1) $
  fillSep [ppExpr opts 1 e1, ppExpr opts 2 e2]
ppExpr opts p (InfixApply e1 op e2) = parensIf (p > 0) $
  fillSep [ppExpr opts 1 e1 <+> ppInfixQOp opts op, ppExpr opts 1 e2]
ppExpr opts p (Lambda         ps e) = parensIf (p > 0) $
  fillSep [ char '\\' <> hsep (map (ppPattern opts 1) ps) <+> rarrow
          , ppExpr opts 0 e
          ]
ppExpr opts p (Let            ds e) = parensIf (p > 0) $
  sep [text "let" <+> ppBlock opts ds, text "in" <+> ppExpr opts 0 e]
ppExpr opts p (DoExpr          sts) = parensIf (p > 0) $
  text "do" <+> vsep (map (ppStmt opts) sts)
ppExpr opts _ (ListComp       e qs) = brackets $
  ppExpr opts 0 e <+> bar <+> sequence (map (ppStmt opts) qs)
ppExpr opts p (Case           e bs) = parensIf (p > 0)
  (text "case" <+> ppExpr opts 0 e <+> text "of"
  $$ align (vsep (map (ppBranchExpr opts) bs)))
ppExpr opts p (Typed          e ty) = parensIf (p > 0) $
  ppExpr opts 0 e <+> doubleColon <+> ppTypeExp opts ty
ppExpr opts p (IfThenElse    c t e) = parensIf (p > 0) $
  text "if" <+> fillSep [ ppExpr opts 0 c
                        , text "then" <+> ppExpr opts 0 t
                        , text "else" <+> ppExpr opts 0 e
                        ]
ppExpr opts _ (Tuple        es) = tupled (map (ppExpr opts 0) es)
ppExpr opts _ (List         es) = list   (map (ppExpr opts 0) es)

ppStmt :: Options -> Statement -> Doc
ppStmt opts (SExpr  e) = ppExpr opts 0 e
ppStmt opts (SPat p e) = fillSep [ppPattern opts 0 p, larrow, ppExpr opts 0 e]
ppStmt opts (SLet  ds) = text "let" <+> ppBlock opts ds

ppPattern :: Options -> Int -> Pattern -> Doc
ppPattern _    _ (PVar     v) = ppVar v
ppPattern opts _ (PLit     l) = ppLitPattern opts l
ppPattern opts p (PComb c ts) = parensIf (p > 0 && not (null ts))
                              $ hsep (ppQName opts c : map (ppPattern opts 1) ts)
ppPattern opts _ (PAs    v t) = ppVar v <> at <> ppPattern opts 1 t
ppPattern opts _ (PTuple  ts) = tupled (map (ppPattern opts 0) ts)
ppPattern opts _ (PList   ts) = list   (map (ppPattern opts 0) ts)

ppVar :: VarIName -> Doc
ppVar (_, name) = text name

ppLitPattern :: Options -> Literal -> Doc
ppLitPattern opts l
  | kics2Mode opts = case l of
    Charc   _ -> wrapUnboxed (curryPrelude, "C_Char")
    Floatc  _ -> wrapUnboxed (curryPrelude, "C_Float")
    Intc    _ -> parens (ppQName opts (curryPrelude, "C_Int") <+>
                         parens (ppLiteral l))
    Stringc _ -> ppLiteral l
  | otherwise =  ppLiteral l
  where wrapUnboxed c = parens (ppQName opts c <+> ppLiteral l <> char '#')

ppBranchExpr :: Options -> BranchExpr -> Doc
ppBranchExpr opts (Branch p e) = indent $
  ppPattern opts 0 p <+> rarrow <+> ppExpr opts 0 e

ppLiteral :: Literal -> Doc
ppLiteral (Intc    i) = int   i
ppLiteral (Floatc  f) = float f
ppLiteral (Charc   c) = text  (show c)
ppLiteral (Stringc s) = text  (show s)

ppPrefixOp :: String -> Doc
ppPrefixOp op = parensIf (isInfixOpName op) (text op)

ppInfixOp :: String -> Doc
ppInfixOp op = if isInfixOpName op then text op else bquotes (text op)

ppPrefixQOp :: Options -> QName -> Doc
ppPrefixQOp opts op = parensIf (isInfixOpName (snd op)) (ppQName opts op)

ppInfixQOp :: Options -> QName -> Doc
ppInfixQOp opts op = if isInfixOpName (snd op) then ppQName opts op
                                               else bquotes (ppQName opts op)

ppName :: QName -> Doc
ppName (_, n) = text n

ppQName :: Options -> QName -> Doc
ppQName opts (modName, symName)
    -- do not qualify names from current module or if it is not required:
  | modName == currentModule opts ||
    not (qualImpModule opts modName) = text symName
    -- otherwise, qualifiy the name:
  | otherwise                        = text $ modName ++ "." ++ symName

isList :: QName -> Bool
isList (_, s) = s == "[]"

isTuple :: QName -> Bool
isTuple (_, []    ) = False
isTuple (_, (x:xs)) = (x == '(') && (p1_isTuple xs)
  where
    p1_isTuple []         = False
    p1_isTuple (z:[])     = z == ')'
    p1_isTuple (z1:z2:zs) = (z1 == ',') && (p1_isTuple (z2:zs))

isInfixOpName :: String -> Bool
isInfixOpName = all (`elem` infixIDs)

infixIDs :: String
infixIDs =  "~!@#$%^&*+-=<>?./|\\:"

-- enclose string with brackets, if required by first argument
parensIf :: Bool -> Doc -> Doc
parensIf nested s
  | nested    = parens s
  | otherwise = s

indent :: Doc -> Doc
indent = nest 2

sequence :: [Doc] -> Doc
sequence = fillSep . punctuate comma

fillEncloseSep :: Doc -> Doc -> Doc -> [Doc] -> Doc
fillEncloseSep l r _ []     = l <> r
fillEncloseSep l r s (d:ds) = enclose l r (fillCat (d:map (s<>) ds))

list :: [Doc] -> Doc
list = fillEncloseSep lbracket rbracket (comma <> space)

tupled :: [Doc] -> Doc
tupled = fillEncloseSep lparen rparen (comma <> space)

curryPrelude :: String
curryPrelude = "Curry_Prelude"

renameModule :: String -> String
renameModule = onLastIdentifier ("Curry_" ++)

unRenameModule :: String -> String
unRenameModule = onLastIdentifier (dropPrefix "Curry_")

addTrace :: String -> String
addTrace = renameModule . onLastIdentifier ("Trace_" ++) . unRenameModule

removeTrace :: String -> String
removeTrace = renameModule . onLastIdentifier (dropPrefix "Trace_")
            . unRenameModule

onLastIdentifier :: (String -> String) -> String -> String
onLastIdentifier f = joinModuleIdentifiers . onLast f . splitModuleIdentifiers

onLast :: (a -> a) -> [a] -> [a]
onLast _ []           = error "Names.onLast: empty list"
onLast f [x]          = [f x]
onLast f (x:xs@(_:_)) = x : onLast f xs

--- Split up the components of a module identifier. For instance,
--- `splitModuleIdentifiers "Data.Set"` evaluates to `["Data", "Set"]`.
splitModuleIdentifiers :: String -> [String]
splitModuleIdentifiers s = let (pref, rest) = break (== '.') s in
  pref : case rest of
    []     -> []
    _ : s' -> splitModuleIdentifiers s'

--- Join the components of a module identifier. For instance,
--- `joinModuleIdentifiers ["Data", "Set"]` evaluates to `"Data.Set"`.
joinModuleIdentifiers :: [String] -> String
joinModuleIdentifiers = foldr1 combine
  where combine xs ys = xs ++ '.' : ys

dropPrefix :: String -> String -> String
dropPrefix pfx s
  | take n s == pfx = drop n s
  | otherwise       = s
  where n = length pfx
