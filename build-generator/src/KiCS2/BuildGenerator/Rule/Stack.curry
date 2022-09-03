module KiCS2.BuildGenerator.Rule.Stack
  ( stackNinja
  ) where

import Data.List ( intersperse )
import KiCS2.BuildGenerator.Options ( Options (..) )
import Language.Ninja.Types
import Language.Ninja.Builder ( NinjaBuilder, build, rule )
import System.FilePath ( (</>) )

-- | The Ninja source containing rules for building Stack projects.
stackNinja :: Options -> NinjaBuilder ()
stackNinja _ = do
  rule (emptyRule "stack")
    { ruleCommand = Just $ unwords $ intersperse "&&"
        [ "tmpdir=\"$$(mktemp -d)\""
        , "trap 'rm -rf \"$$tmpdir\"' EXIT"
        , "$stack build $pkg --stack-yaml $in --copy-bins --local-bin-path \"$$tmpdir\""
        , "mv \"$$tmpdir/$pkg\" $out"
        ]
    , ruleDescription = Just "Building $pkg executable with Stack..."
    }
