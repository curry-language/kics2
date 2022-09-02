module KiCS2.BuildGenerator.Ninja
  ( Ninja (..), Rule (..), Build (..)
  , (:.), (|.), (||.)
  , ppNinja, ppRule, ppBuild
  ) where

import Data.Maybe ( catMaybes )

data Ninja = Ninja
  { ninjaRules :: [Rule]
  , ninjaBuilds :: [Build]
  }

data Rule = Rule
  { ruleName :: String
  , ruleCommand :: String
  }

data Build = Build
  { buildOutputs :: [String]
  , buildRule :: String
  , buildExplicitDeps :: [String]
  , buildImplicitDeps :: [String]
  , buildOrderOnlyDeps :: [String]
  }

infixl 0 :., |., ||.

-- | Creates a build statement.
(:.) :: [String] -> (String, [String]) -> Build
outs :. (rule, deps) = Build
  { buildOutputs = outs
  , buildRule = rule
  , buildExplicitDeps = deps
  , buildImplicitDeps = []
  , buildOrderOnlyDeps = []
  }

-- | Attaches implicit dependencies to a build.
(|.) :: Build -> [String] -> Build
b |. deps = b { buildImplicitDeps = deps }

-- | Attaches order-only dependencies to a build.
(||.) :: Build -> [String] -> Build
b ||. deps = b { buildOrderOnlyDeps = deps }

-- | Pretty-prints a rule.
ppRule :: Rule -> String
ppRule rule = unlines $
  [ "rule " ++ ruleName rule
  ] ++ map indent
    [ "command = " ++ ruleCommand rule
    ]

-- | Pretty-prints a build statement.
ppBuild :: Build -> String
ppBuild build = "build " ++ unwords (buildOutputs build)
  ++ ": " ++ unwords (buildRule build : buildExplicitDeps build)
  ++ (unwords . catMaybes)
    [ (" | "  ++) . unwords <$> nothingIfEmpty (buildImplicitDeps  build)
    , (" || " ++) . unwords <$> nothingIfEmpty (buildOrderOnlyDeps build)
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
