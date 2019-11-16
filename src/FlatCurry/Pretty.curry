--- --------------------------------------------------------------------------
--- This library provides pretty-printers for FlatCurry modules
--- and all substructures (e.g., expressions).
---
--- @author  Bjoern Peemoeller
--- @version June 2015
--- --------------------------------------------------------------------------

module FlatCurry.Pretty where

import Prelude hiding (empty)
import Text.Pretty

import FlatCurry.Types

--- Options for pretty printing
--- @field indentWidth   - number of columns for indentation of substructures
--- @field qualMode      - Qualification mode of pretty printer
--- @field currentModule - Name of current module to be pretty-printed, used
---                        for proper qualification
data Options = Options
  { indentWidth   :: Int
  , qualMode      :: QualMode
  , currentModule :: String
  }

--- Qualification mode, determines whether identifiers are printed qualified
--- or unqualified. While `QualNone` and `QualImports` aim at readability,
--- there may be ambiguities due to shadowing. On the contrary, `QualImports`
--- and `QualAll` produce correct output at the cost of readability.
---
--- @cons QualNone              - no qualification, only unqualified names
--- @cons QualImportsButPrelude - qualify all imports except those from
---                               the module `Prelude`
--- @cons QualImports           - qualify all imports, including `Prelude`
--- @cons QualAll               - qualify all names
data QualMode = QualNone | QualImportsButPrelude | QualImports | QualAll
-- deriving Eq

instance Eq QualMode where
  QualNone == x = case x of { QualNone -> True ; _ -> False }
  QualImportsButPrelude == x = case x of { QualImportsButPrelude -> True ; _ -> False }
  QualImports == x = case x of { QualImports -> True ; _ -> False }
  QualAll == x = case x of { QualAll -> True ; _ -> False }


--- Default `Options` for pretty-printing.
defaultOptions :: Options
defaultOptions = Options
  { indentWidth   = 2
  , qualMode      = QualImportsButPrelude
  , currentModule = ""
  }

-- ---------------------------------------------------------------------------
-- Pretty printing of Flat modules
-- ---------------------------------------------------------------------------

--- pretty-print a FlatCurry module
ppProg :: Options -> Prog -> Doc
ppProg o (Prog m is ts fs os) = vsepBlank
  [ ppHeader    o' m ts fs
  , ppImports   o' is
  , ppOpDecls   o' os
  , ppTypeDecls o' ts
  , ppFuncDecls o' fs
  ]
  where o' = o { currentModule = m }

--- pretty-print the module header
ppHeader :: Options -> String -> [TypeDecl] -> [FuncDecl] -> Doc
ppHeader o m ts fs = indent o $
  sep [text "module" <+> text m, ppExports o ts fs, text "where"]

--- pretty-print the export list
ppExports :: Options -> [TypeDecl] -> [FuncDecl] -> Doc
ppExports o ts fs = tupledSpaced (map (ppTypeExport o) ts ++ ppFuncExports o fs)

--- pretty-print a type export
ppTypeExport :: Options -> TypeDecl -> Doc
ppTypeExport o (Type    qn vis _ cs)
  | vis == Private      = empty
  | null cs             = ppPrefixQOp o qn
  | all isPublicCons cs = ppPrefixQOp o qn <+> text "(..)"
  | otherwise           = ppPrefixQOp o qn <+> tupled (ppConsExports o cs)
    where isPublicCons (Cons _ _ v _) = v == Public
ppTypeExport o (TypeSyn qn vis _ _ )
  | vis == Private = empty
  | otherwise      = ppPrefixQOp o qn

--- pretty-print the export list of constructors
ppConsExports :: Options -> [ConsDecl] -> [Doc]
ppConsExports o cs = [ ppPrefixQOp o qn | Cons qn _ Public _ <- cs]

--- pretty-print the export list of functions
ppFuncExports :: Options -> [FuncDecl] -> [Doc]
ppFuncExports o fs = [ ppPrefixQOp o qn | Func qn _ Public _ _ <- fs]

--- pretty-print a list of import statements
ppImports :: Options -> [String] -> Doc
ppImports o = vsep . map (ppImport o)

--- pretty-print a single import statement
ppImport :: Options -> String -> Doc
ppImport o m = indent o $ text "import" <+> text m

--- pretty-print a list of operator fixity declarations
ppOpDecls :: Options -> [OpDecl] -> Doc
ppOpDecls o = vsep . map (ppOpDecl o)

--- pretty-print a single operator fixity declaration
ppOpDecl :: Options -> OpDecl -> Doc
ppOpDecl o (Op qn fix n) = indent o $ ppFixity fix <+> int n <+> ppInfixQOp o qn

--- pretty-print the associativity keyword
ppFixity :: Fixity -> Doc
ppFixity InfixOp  = text "infix"
ppFixity InfixlOp = text "infixl"
ppFixity InfixrOp = text "infixr"

--- pretty-print a list of type declarations
ppTypeDecls :: Options -> [TypeDecl] -> Doc
ppTypeDecls o = vsepBlank . map (ppTypeDecl o)

--- pretty-print a type declaration
ppTypeDecl :: Options -> TypeDecl -> Doc
ppTypeDecl o (Type    qn _ vs cs) = indent o $ (text "data" <+> ppName qn
  <+> hsep (empty : map ppTVarIndex vs)) $$ ppConsDecls o cs
ppTypeDecl o (TypeSyn qn _ vs ty) = indent o $ text "type" <+> ppName qn
  <+> hsep (empty : map ppTVarIndex vs) </> equals <+> ppTypeExp o ty

--- pretty-print the constructor declarations
ppConsDecls :: Options -> [ConsDecl] -> Doc
ppConsDecls o cs = vsep $ zipWith (<+>) (equals : repeat bar)
                                        (map (ppConsDecl o) cs)

--- pretty print a single constructor
ppConsDecl :: Options -> ConsDecl -> Doc
ppConsDecl o (Cons qn _ _ tys) = hsep $ ppPrefixOp qn : map (ppTypeExpr o 2) tys

--- pretty a top-level type expression
ppTypeExp :: Options -> TypeExpr -> Doc
ppTypeExp o = ppTypeExpr o 0

--- pretty-print a type expression
ppTypeExpr :: Options -> Int -> TypeExpr -> Doc
ppTypeExpr _ _ (TVar           v) = ppTVarIndex v
ppTypeExpr o p (FuncType ty1 ty2) = parensIf (p > 0) $
  ppTypeExpr o 1 ty1 </> rarrow <+> ppTypeExp o ty2
ppTypeExpr o p (TCons     qn tys)
  | isListId qn && length tys == 1 = brackets (ppTypeExp o (head tys))
  | isTupleId qn                   = tupled   (map (ppTypeExp o) tys)
  | otherwise                      = parensIf (p > 1 && not (null tys)) $ sep
                                   (ppPrefixQOp o qn : map (ppTypeExpr o 2) tys)
ppTypeExpr o p (ForallType vs ty)
  | null vs   = ppTypeExpr o p ty
  | otherwise = parensIf (p > 0) $ ppQuantifiedVars vs <+> ppTypeExpr o 0 ty

--- pretty-print explicitly quantified type variables
ppQuantifiedVars :: [TVarIndex] -> Doc
ppQuantifiedVars vs
  | null vs = empty
  | otherwise = text "forall" <+> hsep (map ppTVarIndex vs) <+> char '.'

--- pretty-print a type variable
ppTVarIndex :: TVarIndex -> Doc
ppTVarIndex i = text $ vars !! i
  where vars = [ chr c : if n == 0 then [] else show n
               | n <- [0 ..], c <- [ord 'a' .. ord 'z']
               ]

--- pretty-print a list of function declarations
ppFuncDecls :: Options -> [FuncDecl] -> Doc
ppFuncDecls o = vsepBlank . map (ppFuncDecl o)

--- pretty-print a function declaration
ppFuncDecl :: Options -> FuncDecl -> Doc
ppFuncDecl o (Func qn _ _ ty r)
  =  indent o (sep [ppPrefixOp qn, text "::", ppTypeExp o ty])
  $$ indent o (ppPrefixOp qn <+> ppRule o r)

--- pretty-print a function rule
ppRule :: Options -> Rule -> Doc
ppRule o (Rule  vs e)
  | null vs   = equals <+> ppExp o e
  | otherwise = hsep (map ppVarIndex vs) </> equals <+> ppExp o e
ppRule _ (External e) = text "external" <+> dquotes (text e)

--- Pretty-print a top-level expression.
ppExp :: Options -> Expr -> Doc
ppExp o = ppExpr o 0

--- pretty-print an expression
ppExpr :: Options -> Int -> Expr -> Doc
ppExpr _ _ (Var        v) = ppVarIndex v
ppExpr _ _ (Lit        l) = ppLiteral l
ppExpr o p (Comb _ qn es) = ppComb o p qn es
ppExpr o p (Free    vs e)
  | null vs               = ppExpr o p e
  | otherwise             = parensIf (p > 0) $ sep
                            [ text "let"
                              <+> sep (punctuate comma (map ppVarIndex vs))
                              <+> text "free"
                            , text "in" </> ppExp o e
                            ]
ppExpr o p (Let     ds e) = parensIf (p > 0) $ sep
                            [ text "let" <+> ppDecls o ds
                            , text "in"  <+> ppExp   o e
                            ]
ppExpr o p (Or     e1 e2) = parensIf (p > 0)
                          $ ppExpr o 1 e1 <+> text "?" <+> ppExpr o 1 e2
ppExpr o p (Case ct e bs) = parensIf (p > 0) $ indent o
                          $ ppCaseType ct <+> ppExpr o 1 e <+> text "of"
                            $$ vsep (map (ppBranch o) bs)
ppExpr o p (Typed   e ty) = parensIf (p > 0)
                          $ ppExp o e <+> text "::" <+> ppTypeExp o ty

--- pretty-print a variable
ppVarIndex :: VarIndex -> Doc
ppVarIndex i | i < 0     = text $ 'x' : show (negate i)
             | otherwise = text $ 'v' : show i

--- pretty-print a literal
ppLiteral :: Literal -> Doc
ppLiteral (Intc   i) = int i
ppLiteral (Floatc f) = float f
ppLiteral (Charc  c) = text (show c)

--- Pretty print a constructor or function call
ppComb :: Options -> Int -> QName -> [Expr] -> Doc
ppComb o p qn es | isListId  qn && null es = text "[]"
                 | isTupleId qn            = tupled (map (ppExp o) es)
                 | otherwise               = case es of
  []               -> ppPrefixQOp o qn
  [e1,e2]
    | isInfixOp qn -> parensIf (p > 0)
                    $ fillSep [ppExpr o 1 e1, ppInfixQOp o qn, ppExpr o 1 e2]
  _                -> parensIf (p > 0)
                    $ fillSep (ppPrefixQOp o qn : map (ppExpr o 1) es)

--- pretty-print a list of declarations
ppDecls :: Options -> [(VarIndex, Expr)] -> Doc
ppDecls o = align . vsep . map (ppDecl o)

--- pretty-print a single declaration
ppDecl :: Options -> (VarIndex, Expr) -> Doc
ppDecl o (v, e) = ppVarIndex v <+> equals <+> ppExp o e

--- Pretty print the type of a case expression
ppCaseType :: CaseType -> Doc
ppCaseType Rigid = text "case"
ppCaseType Flex  = text "fcase"

--- Pretty print a case branch
ppBranch :: Options -> BranchExpr -> Doc
ppBranch o (Branch p e) = ppPattern o p <+> rarrow <+> indent o (ppExp o e)

--- Pretty print a pattern
ppPattern :: Options -> Pattern -> Doc
ppPattern o (Pattern c vs)
  | isListId c && null vs = text "[]"
  | isTupleId c           = tupled (map ppVarIndex vs)
  | otherwise             = case vs of
  [v1,v2] | isInfixOp c -> ppVarIndex v1 <+> ppInfixQOp o c <+> ppVarIndex v2
  _                     -> hsep (ppPrefixQOp o c : map ppVarIndex vs)
ppPattern _ (LPattern   l) = ppLiteral l

-- ---------------------------------------------------------------------------
-- Names
-- ---------------------------------------------------------------------------

--- pretty-print a qualified prefix operator.
ppPrefixQOp :: Options -> QName -> Doc
ppPrefixQOp o qn = parensIf (isInfixOp qn) (ppQName o qn)

--- pretty-print a prefix operator unqualified.
ppPrefixOp :: QName -> Doc
ppPrefixOp qn = parensIf (isInfixOp qn) (ppName qn)

--- pretty-print an infix operator
ppInfixQOp :: Options -> QName -> Doc
ppInfixQOp o qn = if isInfixOp qn then ppQName o qn else bquotes (ppQName o qn)

--- Pretty-print a qualified name
ppQName :: Options -> QName -> Doc
ppQName o qn@(m, i)
  | null m                                     = text i
  | isConsId qn || isListId qn || isTupleId qn = text i
  | q == QualNone                              = text i
  | q == QualImportsButPrelude
    && (m == m' || m == "Prelude")             = text i
  | q == QualImports && m == m'                = text i
  | otherwise                                  = text $ m ++ '.' : i
  where
  q  = qualMode o
  m' = currentModule o

--- Pretty-print a qualified name unqualified (e.g., for type definitions).
ppName :: QName -> Doc
ppName (_, i) = text i

--- Check whether an operator is an infix operator
isInfixOp :: QName -> Bool
isInfixOp = all (`elem` "~!@#$%^&*+-=<>:?./|\\") . snd

--- Check whether an identifier represents the `:` list constructor.
isConsId :: QName -> Bool
isConsId (m, i) = m `elem` ["Prelude", ""] && i == ":"

--- Check whether an identifier represents a list
isListId :: QName -> Bool
isListId (m, i) = m `elem` ["Prelude", ""] && i == "[]"

--- Check whether an identifier represents a tuple
isTupleId :: QName -> Bool
isTupleId (m, i) = m `elem` ["Prelude", ""] && i == mkTuple (length i)
  where mkTuple n = '(' : replicate (n - 2) ',' ++ ")"

--- Indentation
indent :: Options -> Doc -> Doc
indent o d = nest (indentWidth o) d
