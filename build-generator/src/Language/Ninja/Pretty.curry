module Language.Ninja.Pretty
  ( ppNinja, ppRule, ppBuild
  ) where

import Data.Maybe ( mapMaybe, catMaybes )
import Language.Ninja.Types

-- | Pretty-prints a generic statement.
ppStmt :: String -> String -> [Var String] -> String
ppStmt keyword args vars = unlines $ unwords [keyword, args] : (indent . ppVar <$> vars)
  where

-- | Pretty-prints a variable declaration.
ppVar :: Var String -> String
ppVar (Var name value) = name ++ " = " ++ value

-- | Pretty-prints a rule.
ppRule :: Rule -> String
ppRule r = ppStmt "rule" (ruleName r) $ catMaybeVars
  [ "command" =. ruleCommand r
  , "description" =. ruleDescription r
  ]

-- | Pretty-prints a build statement.
ppBuild :: Build -> String
ppBuild b = ppStmt "build" line vars
  where
    line = unwords (buildOutputs b)
      ++ ": "
      ++ unwords (buildRule b : buildExplicitDeps b)
      ++ (unwords . catMaybes)
        [ (" | "  ++) . unwords <$> nothingIfEmpty (buildImplicitDeps  b)
        , (" || " ++) . unwords <$> nothingIfEmpty (buildOrderOnlyDeps b)
        ]
    vars = buildVariables b

-- | Pretty-prints a Ninja file.
ppNinja :: Ninja -> String
ppNinja ninja = unlines $
     (ppVar   <$> ninjaVars   ninja)
  ++ (ppRule  <$> ninjaRules  ninja)
  ++ (ppBuild <$> ninjaBuilds ninja)

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
