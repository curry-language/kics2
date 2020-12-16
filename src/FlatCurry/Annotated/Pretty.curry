--- --------------------------------------------------------------------------
--- This library provides pretty-printers for AnnotatedFlatCurry modules
--- and all substructures (e.g., expressions). Note that annotations are
--- ignored for pretty-printing.
---
--- @author  Bjoern Peemoeller
--- @version October 2015
--- @category meta
--- --------------------------------------------------------------------------
module FlatCurry.Annotated.Pretty where

import Prelude hiding ( empty )

import Text.Pretty

import FlatCurry.Annotated.Types

--- pretty-print a FlatCurry module
ppProg :: AProg _ -> Doc
ppProg (AProg m is ts fs os) = compose (<$+$>)
  [ ppHeader    m ts fs
  , ppImports   is
  , ppOpDecls   os
  , ppTypeDecls ts
  , ppFuncDecls fs
  ]

--- pretty-print the module header
ppHeader :: String -> [TypeDecl] -> [AFuncDecl _] -> Doc
ppHeader m ts fs = indent $
  sep [text "module" <+> text m, ppExports ts fs, text "where"]

--- pretty-print the export list
ppExports :: [TypeDecl] -> [AFuncDecl _] -> Doc
ppExports ts fs = tupledSpaced (map ppTypeExport ts ++ ppFuncExports fs)

--- pretty-print a type export
ppTypeExport :: TypeDecl -> Doc
ppTypeExport (Type    qn vis _ cs)
  | vis == Private      = empty
  | all isPublicCons cs = ppPrefixOp qn <+> text "(..)"
  | otherwise           = ppPrefixOp qn <+> tupled (ppConsExports cs)
    where isPublicCons (Cons _ _ v _) = v == Public
ppTypeExport (TypeSyn qn vis _ _ )
  | vis == Private = empty
  | otherwise      = ppPrefixOp qn
ppTypeExport (TypeNew qn vis _ (NewCons _ vis' _))
  | vis == Private || vis' == Private = empty
  | otherwise                         = ppPrefixOp qn <+> text "(..)"

--- pretty-print the export list of constructors
ppConsExports :: [ConsDecl] -> [Doc]
ppConsExports cs = [ ppPrefixOp qn | Cons qn _ Public _ <- cs]

--- pretty-print the export list of functions
ppFuncExports :: [AFuncDecl _] -> [Doc]
ppFuncExports fs = [ ppPrefixOp qn | AFunc qn _ Public _ _ <- fs]

--- pretty-print a list of import statements
ppImports :: [String] -> Doc
ppImports = vsep . map ppImport

--- pretty-print a single import statement
ppImport :: String -> Doc
ppImport m = indent $ text "import" <+> text m

--- pretty-print a list of operator fixity declarations
ppOpDecls :: [OpDecl] -> Doc
ppOpDecls = vsep . map ppOpDecl

--- pretty-print a single operator fixity declaration
ppOpDecl :: OpDecl -> Doc
ppOpDecl (Op qn fix n) = indent $ ppFixity fix <+> int n <+> ppInfixOp qn

--- pretty-print the associativity keyword
ppFixity :: Fixity -> Doc
ppFixity InfixOp  = text "infix"
ppFixity InfixlOp = text "infixl"
ppFixity InfixrOp = text "infixr"

--- pretty-print a list of type declarations
ppTypeDecls :: [TypeDecl] -> Doc
ppTypeDecls = compose (<$+$>) . map ppTypeDecl

--- pretty-print a type declaration
ppTypeDecl :: TypeDecl -> Doc
ppTypeDecl (Type    qn _ vs cs) = indent $ text "data" <+> ppQName qn
  <+> hsep (map (ppTVarIndex . fst) vs) <$$> ppConsDecls cs
ppTypeDecl (TypeSyn qn _ vs ty) = indent $ text "type" <+> ppQName qn
  <+> hsep (map (ppTVarIndex . fst) vs) </> equals <+> ppTypeExp ty
ppTypeDecl (TypeNew qn _ vs c)  = indent $ text "newtype" <+> ppQName qn
  <+> hsep (empty : map (ppTVarIndex . fst) vs) $$ ppNewConsDecl c

--- pretty-print the constructor declarations
ppConsDecls :: [ConsDecl] -> Doc
ppConsDecls cs = vsep $
  zipWith (<+>) (equals : repeat bar) (map ppConsDecl cs)

--- pretty print a single constructor
ppConsDecl :: ConsDecl -> Doc
ppConsDecl (Cons qn _ _ tys) = hsep $ ppPrefixOp qn : map (ppTypeExpr 2) tys

--- pretty print a single newtype constructor
ppNewConsDecl :: NewConsDecl -> Doc
ppNewConsDecl (NewCons qn _ ty) = hsep [ppPrefixOp qn, ppTypeExpr 2 ty]

--- pretty a top-level type expression
ppTypeExp :: TypeExpr -> Doc
ppTypeExp = ppTypeExpr 0

--- pretty-print a type expression
ppTypeExpr :: Int -> TypeExpr -> Doc
ppTypeExpr _ (TVar           v) = ppTVarIndex v
ppTypeExpr p (FuncType ty1 ty2) = parensIf (p > 0) $
  ppTypeExpr 1 ty1 </> rarrow <+> ppTypeExp ty2
ppTypeExpr p (TCons     qn tys)
  | isListId qn && length tys == 1 = brackets (ppTypeExp (head tys))
  | isTupleId qn                   = tupled   (map ppTypeExp tys)
  | otherwise                      = parensIf (p > 1 && not (null tys)) $ sep
                                      (ppPrefixOp qn : map (ppTypeExpr 2) tys)
ppTypeExpr p (ForallType vs ty)
  | null vs   = ppTypeExpr p ty
  | otherwise = parensIf (p > 0) $ ppQuantifiedVars vs <+> ppTypeExpr 0 ty

--- pretty-print explicitly quantified type variables
ppQuantifiedVars :: [(TVarIndex, Kind)] -> Doc
ppQuantifiedVars vs
  | null vs   = empty
  | otherwise = text "forall" <+> hsep (map (ppTVarIndex . fst) vs) <+> char '.'

--- pretty-print a type variable
ppTVarIndex :: TVarIndex -> Doc
ppTVarIndex i = text $ vars !! i
  where vars = [ chr c : if n == 0 then [] else show n
               | n <- [0 ..], c <- [ord 'a' .. ord 'z']
               ]

--- pretty-print a list of function declarations
ppFuncDecls :: [AFuncDecl _] -> Doc
ppFuncDecls = compose (<$+$>) . map ppFuncDecl

--- pretty-print a function declaration
ppFuncDecl :: AFuncDecl _ -> Doc
ppFuncDecl (AFunc qn _ _ ty r)
  =    indent (sep [ppPrefixOp qn, text "::", ppTypeExp ty])
  <$$> indent (ppPrefixOp qn <+> ppRule r)

--- pretty-print a function rule
ppRule :: ARule _ -> Doc
ppRule (ARule    _ vs e)
  | null vs   = equals <+> ppExp e
  | otherwise = hsep (map ppAVarIndex vs) </> equals <+> ppExp e
ppRule (AExternal _   e) = text "external" <+> dquotes (text e)

--- pretty-print a top-level expression
ppExp :: AExpr _ -> Doc
ppExp = ppExpr 0

--- pretty-print an expression
ppExpr :: Int -> AExpr _ -> Doc
ppExpr _ (AVar   _       v) = ppVarIndex v
ppExpr _ (ALit   _       l) = ppLiteral l
ppExpr p (AComb  _ _ qn es) = ppComb p qn es
ppExpr p (AFree  _    vs e)
  | null vs                 = ppExpr p e
  | otherwise               = parensIf (p > 0) $ sep
                              [ text "let"
                                <+> encloseSep empty empty comma
                                    (map ppAVarIndex vs)
                                <+> text "free"
                              , text "in" </> ppExp e
                              ]
ppExpr p (ALet   _    ds e) = parensIf (p > 0) $ sep
                              [text "let" <+> ppDecls ds, text "in" <+> ppExp e]
ppExpr p (AOr    _   e1 e2) = parensIf (p > 0)
                            $ ppExpr 1 e1 <+> text "?" <+> ppExpr 1 e2
ppExpr p (ACase  _ ct e bs) = parensIf (p > 0) $ indent
                            $ ppCaseType ct <+> ppExpr 1 e <+> text "of"
                              <$$> vsep (map ppBranch bs)
ppExpr p (ATyped _    e ty) = parensIf (p > 0)
                            $ ppExp e <+> text "::" <+> ppTypeExp ty

--- pretty-print an annotated variable
ppAVarIndex :: (VarIndex, _) -> Doc
ppAVarIndex (i, _) | i < 0     = text $ 'x' : show (negate i)
                   | otherwise = text $ 'v' : show i

--- pretty-print a variable
ppVarIndex :: VarIndex -> Doc
ppVarIndex i | i < 0     = text $ 'x' : show (negate i)
             | otherwise = text $ 'v' : show i

--- pretty-print a literal
ppLiteral :: Literal -> Doc
ppLiteral (Intc   i) = int i
ppLiteral (Floatc f) = float f
ppLiteral (Charc  c) = text (showEscape c)

--- Escape character literal
showEscape :: Char -> String
showEscape c
  | o <   10  = "'\\00" ++ show o ++ "'"
  | o <   32  = "'\\0"  ++ show o ++ "'"
  | o == 127  = "'\\127'"
  | otherwise = show c
  where o = ord c

--- Pretty print a constructor or function call
ppComb :: Int -> (QName, _) -> [AExpr _] -> Doc
ppComb p (qn, _) es | isListId  qn && null es = text "[]"
                    | isTupleId qn            = tupled (map ppExp es)
                    | otherwise               = case es of
  []               -> ppPrefixOp qn
  [e1,e2]
    | isInfixOp qn -> parensIf (p > 0)
                    $ sep [ppExpr 1 e1, ppInfixOp qn, ppExpr 1 e2]
  _                -> parensIf (p > 0)
                    $ sep (ppPrefixOp qn : map (ppExpr 1) es)

--- pretty-print a list of declarations
ppDecls :: [((VarIndex, _), AExpr _)] -> Doc
ppDecls = semiBracesSpaced . map ppDecl

--- pretty-print a single declaration
ppDecl :: ((VarIndex, _), AExpr _) -> Doc
ppDecl (v, e) = ppAVarIndex v <+> equals <+> ppExp e

--- Pretty print the type of a case expression
ppCaseType :: CaseType -> Doc
ppCaseType Rigid = text "case"
ppCaseType Flex  = text "fcase"

--- Pretty print a case branch
ppBranch :: ABranchExpr _ -> Doc
ppBranch (ABranch p e) = ppPattern p <+> rarrow <+> indent (ppExp e)

--- Pretty print a pattern
ppPattern :: APattern _ -> Doc
ppPattern (APattern _ (c, _) vs)
  | isListId c && null vs = text "[]"
  | isTupleId c           = tupled (map ppAVarIndex vs)
  | otherwise             = case vs of
  [v1,v2] | isInfixOp c -> ppAVarIndex v1 <+> ppInfixOp c <+> ppAVarIndex v2
  _                     -> hsep (ppPrefixOp c : map ppAVarIndex vs)
ppPattern (ALPattern _     l) = ppLiteral l

--- pretty-print a prefix operator
ppPrefixOp :: QName -> Doc
ppPrefixOp qn = parensIf (isInfixOp qn) (ppQName qn)

--- pretty-print an infix operator
ppInfixOp :: QName -> Doc
ppInfixOp qn = if isInfixOp qn then ppQName qn else bquotes (ppQName qn)

--- Pretty-print a qualified name
ppQName :: QName -> Doc
ppQName (m, i)
  | m == "Prelude" && i `elem` builtin = text i
  | otherwise                          = text $ m ++ '.' : i
  where builtin = [ "[]", "?", ":", "+", "-", "*", "<"
                  , ">", "<=", ">=", "==", "/=", "&>", "&" ]

--- Check whether an operator is an infix operator
isInfixOp :: QName -> Bool
isInfixOp = all (`elem` "~!@#$%^&*+-=<>:?./|\\") . snd

--- Check whether an identifier represents a list
isListId :: QName -> Bool
isListId (m, i) = m `elem` ["Prelude", ""] && i == "[]"

--- Check whether an identifier represents a tuple
isTupleId :: QName -> Bool
isTupleId (m, i) = m `elem` ["Prelude", ""] && i == mkTuple (length i)
  where mkTuple n = '(' : replicate (n - 2) ',' ++ ")"

--- Indentation
indent :: Doc -> Doc
indent = nest 2
