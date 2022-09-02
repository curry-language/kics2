module KiCS2.BuildGenerator.Stack
  ( stackNinja
  ) where

import KiCS2.BuildGenerator.Options ( Options (..) )
import Ninja.Types
import Ninja.Builder ( NinjaBuilder, build, rule )
import System.FilePath ( (</>) )

-- | The Ninja source containing rules for building Stack projects.
stackNinja :: Options -> NinjaBuilder ()
stackNinja o = do
  rule Rule
    { ruleName = "stack"
    , ruleCommand = unwords
        [ "stack build"
        , "$stackflags"
        , "--stack-yaml", "$in"
        , "--copy-bins"
        , "--local-bin-path", "$$(dirname $$(realpath $out))"
        ]
    }
