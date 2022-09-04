module KiCS2.BuildGenerator.Rule.Curry
  ( curryNinja
  ) where

import Data.List ( intersperse )
import KiCS2.BuildGenerator.Options ( Options (..) )
import Language.Ninja.Types
import Language.Ninja.Builder ( NinjaBuilder, build, rule )
import System.FilePath ( (</>) )

-- | The Ninja source containing rules for building Curry code.
curryNinja :: Options -> NinjaBuilder ()
curryNinja _ = do
  rule (emptyRule "curry")
    { ruleCommand = Just $ unwords $ intersperse "&&"
        [ "tmpdir=\"$$(mktemp -d)\""
        , "trap 'rm -rf \"$$tmpdir\"' EXIT"
        , "cd \"$$(dirname $in)\""
        , "$curry :set v2 :load $main :cd \"$$tmpdir\" :save :quit"
        , "mv \"$$tmpdir/$main\" $out"
        ]
    , ruleDescription = Just "Building $main executable with $curry..."
    }
  
  rule (emptyRule "kics2c")
    { ruleCommand = Just $ "KICS2HOME=$kics2home $kics2c $kics2c_opts $mod"
    , ruleDescription = Just "Compiling $mod with kics2c..."
    }

