module Language.Ninja.Types
  ( Ninja (..), Stmt (..), Var (..), Rule (..), Build (..)
  , (:.), (|.), (||.), (=.)
  , emptyRule
  ) where

newtype Ninja = Ninja [Stmt]

data Stmt = VarStmt (Var String)
          | RuleStmt Rule
          | BuildStmt Build
          | CommentStmt String

data Var a = Var
  { varName  :: String
  , varValue :: a
  }

data Rule = Rule
  { ruleName        :: String
  , ruleCommand     :: Maybe String
  , ruleDescription :: Maybe String
  }

data Build = Build
  { buildOutputs       :: [String]
  , buildRule          :: String
  , buildExplicitDeps  :: [String]
  , buildImplicitDeps  :: [String]
  , buildOrderOnlyDeps :: [String]
  , buildVariables     :: [Var String]
  }

instance Monoid Ninja where
  mempty = Ninja []
  mappend (Ninja s1) (Ninja s2) = Ninja $ s1 ++ s2

infixl 1 :., |., ||., =.

-- | Creates a build statement.
(:.) :: [String] -> (String, [String]) -> Build
outs :. (r, deps) = Build
  { buildOutputs = outs
  , buildRule = r
  , buildExplicitDeps = deps
  , buildImplicitDeps = []
  , buildOrderOnlyDeps = []
  , buildVariables = []
  }

-- | Attaches implicit dependencies to a build.
(|.) :: Build -> [String] -> Build
b |. deps = b { buildImplicitDeps = deps }

-- | Attaches order-only dependencies to a build.
(||.) :: Build -> [String] -> Build
b ||. deps = b { buildOrderOnlyDeps = deps }

-- | Creates a variable.
(=.) :: String -> a -> Var a
(=.) = Var

-- | An empty rule with the given name.
emptyRule :: String -> Rule
emptyRule name = Rule
  { ruleName = name
  , ruleCommand = Nothing
  , ruleDescription = Nothing
  }
