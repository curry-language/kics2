module Ninja.Pretty
  ( ppNinja, ppRule, ppBuild
  ) where

import Data.Maybe ( catMaybes )
import Ninja.Types

-- | Pretty-prints a rule.
ppRule :: Rule -> String
ppRule r = unlines $
  [ "rule " ++ ruleName r
  ] ++ map indent
    [ "command = " ++ ruleCommand r
    ]

-- | Pretty-prints a build statement.
ppBuild :: Build -> String
ppBuild b = "build " ++ unwords (buildOutputs b)
  ++ ": " ++ unwords (buildRule b : buildExplicitDeps b)
  ++ (unwords . catMaybes)
    [ (" | "  ++) . unwords <$> nothingIfEmpty (buildImplicitDeps  b)
    , (" || " ++) . unwords <$> nothingIfEmpty (buildOrderOnlyDeps b)
    ] 

-- | Pretty-prints a Ninja file.
ppNinja :: Ninja -> String
ppNinja ninja = unlines $
     (ppBuild <$> ninjaBuilds ninja)
  ++ (ppRule  <$> ninjaRules  ninja)

-- | Nothing if the list is empty, otherwise the list wrapped in Just.
nothingIfEmpty :: [a] -> Maybe [a]
nothingIfEmpty xs = case xs of
  [] -> Nothing
  _  -> Just xs

-- | Indents a string.
indent :: String -> String
indent = ("  " ++)

