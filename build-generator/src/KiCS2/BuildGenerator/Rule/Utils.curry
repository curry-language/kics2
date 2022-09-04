module KiCS2.BuildGenerator.Rule.Utils
  ( utilsNinja
  ) where

import Data.List ( intersperse )
import KiCS2.BuildGenerator.Options ( Options (..) )
import Language.Ninja.Types
import Language.Ninja.Builder ( NinjaBuilder, build, rule )
import System.FilePath ( (</>) )

-- | The Ninja source containing basic utility rules.
utilsNinja :: Options -> NinjaBuilder ()
utilsNinja _ = do
  rule (emptyRule "cp")
    { ruleCommand = Just "cp $in $out"
    }

  rule (emptyRule "run")
    { ruleCommand = Just "$in"
    }
