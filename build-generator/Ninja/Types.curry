module Ninja.Types
  ( Ninja (..), Rule (..), Build (..)
  , (:.), (|.), (||.)
  ) where

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

instance Monoid Ninja where
  mempty = Ninja
    { ninjaRules  = []
    , ninjaBuilds = []
    }
  mappend n1 n2 = Ninja
    { ninjaRules  = ninjaRules  n1 ++ ninjaRules  n2
    , ninjaBuilds = ninjaBuilds n1 ++ ninjaBuilds n2
    }

infixl 1 :., |., ||.

-- | Creates a build statement.
(:.) :: [String] -> (String, [String]) -> Build
outs :. (r, deps) = Build
  { buildOutputs = outs
  , buildRule = r
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
