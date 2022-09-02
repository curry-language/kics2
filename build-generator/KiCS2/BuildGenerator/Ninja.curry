module KiCS2.BuildGenerator.Ninja
  ( Ninja (..), Rule (..), Build (..)
  , (:.), (|.), (||.)
  , ppNinja, ppRule, ppBuild
  , NinjaBuilder (..)
  , build, rule
  , execNinjaBuilder
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

-- Effectively a (Writer Ninja), however declared separately to avoid a dependency on transformers
data NinjaBuilder a = NinjaBuilder (a, Ninja)

instance Functor NinjaBuilder where
  fmap f (NinjaBuilder (x, n)) = NinjaBuilder (f x, n)

instance Applicative NinjaBuilder where
  pure x = NinjaBuilder (x, mempty)
  NinjaBuilder (f, n1) <*> NinjaBuilder (x, n2) = NinjaBuilder (f x, n1 `mappend` n2)

instance Monad NinjaBuilder where
  return = pure
  NinjaBuilder (x, n1) >>= f = let NinjaBuilder (y, n2) = f x
                               in NinjaBuilder (y, n1 `mappend` n2)

build :: Build -> NinjaBuilder ()
build b = NinjaBuilder ((), mempty { ninjaBuilds = [b] })

rule :: Rule -> NinjaBuilder ()
rule r = NinjaBuilder ((), mempty { ninjaRules = [r] })

execNinjaBuilder :: NinjaBuilder a -> Ninja
execNinjaBuilder (NinjaBuilder (_, n)) = n
