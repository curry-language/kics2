module KiCS2.BuildGenerator.Build.Frontend
  ( frontendNinja
  ) where

import KiCS2.BuildGenerator.Options ( Options (..), optFrontendDir, optFrontendBin )
import KiCS2.BuildGenerator.Utils ( findWithSuffix, concatMapM )
import Language.Ninja.Types
import Language.Ninja.Builder ( NinjaBuilder, build, rule )
import System.FilePath ( (</>) )

-- | The Ninja source for building the frontend.
frontendNinja :: Options -> NinjaBuilder ()
frontendNinja o = do
  let srcDirs     = (optFrontendDir o </>) <$> ["src", "app"]
      buildFiles  = (optFrontendDir o </>) <$> ["curry-frontend.cabal"]
      stackYaml   = optFrontendDir o </> "stack.yaml"
      frontendBin = optFrontendBin o

  srcs <- concatMapM (findWithSuffix ".hs") srcDirs

  build $ ([frontendBin] :. ("stack", [stackYaml]) |. buildFiles ++ srcs)
    { buildVariables =
        [ "pkg" =. "curry-frontend"
        ]
    }
