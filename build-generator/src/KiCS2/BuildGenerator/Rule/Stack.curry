module KiCS2.BuildGenerator.Rule.Stack
  ( stackNinja
  ) where

import KiCS2.BuildGenerator.Options ( Options (..) )
import Language.Ninja.Types
import Language.Ninja.Builder ( NinjaBuilder, build, rule )
import System.FilePath ( (</>) )

-- | The Ninja source containing rules for building Stack projects.
stackNinja :: Options -> NinjaBuilder ()
stackNinja _ = do
  -- TODO: Create tmpdirs or copy from stack's install location
  rule (emptyRule "stack")
    { ruleCommand = Just $ unwords
        [ "$stack build $pkgs --stack-yaml $in --copy-bins --local-bin-path $$(dirname $$(realpath $out))"
        , "&&"
        , "for pkg in $pkgs; do mv $$(dirname $$(realpath $out))/$pkgs $out; done"
        ]
    , ruleDescription = Just "Building $pkgs with Stack..."
    }
