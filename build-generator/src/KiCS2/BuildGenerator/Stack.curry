module KiCS2.BuildGenerator.Stack
  ( stackNinja
  ) where

import KiCS2.BuildGenerator.Options ( Options (..) )
import Language.Ninja.Types
import Language.Ninja.Builder ( NinjaBuilder, build, rule )
import System.FilePath ( (</>) )

-- | The Ninja source containing rules for building Stack projects.
stackNinja :: Options -> NinjaBuilder ()
stackNinja o = do
  rule (emptyRule "stack")
    { ruleCommand = Just $ unwords
        [ "stack build"
        , "$stackpkgs"
        , "--stack-yaml", "$in"
        , "--copy-bins"
        , "--local-bin-path", "$$(dirname $$(realpath $out))"
        ]
    , ruleDescription = Just "Building $out with Stack..."
    }
