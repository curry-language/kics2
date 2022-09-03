module KiCS2.BuildGenerator.Rule.Curry
  ( curryNinja
  ) where

import KiCS2.BuildGenerator.Options ( Options (..) )
import Language.Ninja.Types
import Language.Ninja.Builder ( NinjaBuilder, build, rule )
import System.FilePath ( (</>) )

-- | The Ninja source containing rules for building Curry projects.
curryNinja :: Options -> NinjaBuilder ()
curryNinja _ = do
  -- TODO: Create tmpdirs for this
  rule (emptyRule "curry")
    { ruleCommand = Just $ unwords
        [ "$curry :set v2 :load $main :save :quit"
        , "&&"
        , "mv $main $out"
        ]
    , ruleDescription = Just "Building $main with $curry..."
    }

