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
  
  let envVars = unwords $ (\(k, v) -> k ++ "=\"" ++ v ++ "\"") <$>
        [ ("CURRY", "$curry")
        , ("VERSION", "$version")
        , ("STACK", "$stack")
        , ("RESOLVER", "$resolver")
        , ("IDSUPPLY", "$idsupply")
        , ("GHCOPTS", "$ghcopts")
        , ("GHC", "$ghc")
        , ("CYPM", "$cypm")
        ]

  rule (emptyRule "configure")
    { ruleCommand = Just $ unwords [envVars, "envsubst < $in > $out"]
    }
