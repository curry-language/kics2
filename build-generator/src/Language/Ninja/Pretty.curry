module Language.Ninja.Pretty
  ( ppNinja, ppRule, ppBuild
  ) where

import Data.Maybe ( mapMaybe, catMaybes )
import Data.List ( intercalate )
import Language.Ninja.Types

-- | Pretty-prints a generic statement.
ppKeywordStmt :: String -> String -> [Var String] -> String
ppKeywordStmt keyword args vars = intercalate "\n" $ unwords [keyword, args] : (indent . ppVar <$> vars)
  where

-- | Pretty-prints a variable declaration.
ppVar :: Var String -> String
ppVar (Var name value) = name ++ " = " ++ value

-- | Pretty-prints a rule.
ppRule :: Rule -> String
ppRule r = ppKeywordStmt "rule" (ruleName r) $ catMaybeVars
  [ "command" =. ruleCommand r
  , "description" =. ruleDescription r
  ]

-- | Pretty-prints a build statement.
ppBuild :: Build -> String
ppBuild b = ppKeywordStmt "build" line vars
  where
    line = unwords (buildOutputs b)
      ++ ": "
      ++ unwords (buildRule b : buildExplicitDeps b)
      ++ (unwords . catMaybes)
        [ (" | "  ++) . unwords <$> nothingIfEmpty (buildImplicitDeps  b)
        , (" || " ++) . unwords <$> nothingIfEmpty (buildOrderOnlyDeps b)
        ]
    vars = buildVariables b ++ catMaybeVars
      [ "description" =. buildDescription b
      , "generator" =. if buildGenerator b then Just "1" else Nothing
      ]

-- | Pretty-prints a comment.
ppComment :: String -> String
ppComment c = "# " ++ c

-- | Pretty-prints an empty line.
ppWhitespace :: String
ppWhitespace = ""

-- | Pretty-prints a Ninja statement.
ppStmt :: Stmt -> String
ppStmt stmt = case stmt of
  BuildStmt b    -> ppBuild b
  RuleStmt r     -> ppRule r
  VarStmt v      -> ppVar v
  CommentStmt c  -> ppComment c
  WhitespaceStmt -> ppWhitespace

-- | Pretty-prints a Ninja file.
ppNinja :: Ninja -> String
ppNinja (Ninja stmts) = unlines $ ppStmt <$> stmts

-- | Nothing if the list is empty, otherwise the list wrapped in Just.
nothingIfEmpty :: [a] -> Maybe [a]
nothingIfEmpty xs = case xs of
  [] -> Nothing
  _  -> Just xs

-- | Indents a string.
indent :: String -> String
indent = ("  " ++)

-- | Compacts a variable list by filtering entries Just values.
catMaybeVars :: [Var (Maybe a)] -> [Var a]
catMaybeVars = mapMaybe $ \(Var n maybeV) -> Var n <$> maybeV
