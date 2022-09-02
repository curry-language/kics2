module KiCS2.BuildGenerator.Frontend
  ( frontendNinja
  ) where

import Control.Monad ( join )
import Control.Monad.IO.Class ( liftIO )
import KiCS2.BuildGenerator.Options ( Options (..), optFrontendDir, frontendBin )
import KiCS2.BuildGenerator.Utils ( findWithSuffix, walk )
import Language.Ninja.Types
import Language.Ninja.Builder ( NinjaBuilder, build, rule )
import System.FilePath ( (</>) )

-- | The Ninja source for building the frontend.
frontendNinja :: Options -> NinjaBuilder ()
frontendNinja o = do
  let srcDirs    = (optFrontendDir o </>) <$> ["src", "app"]
      buildFiles = (optFrontendDir o </>) <$> ["curry-frontend.cabal"]
      stackYaml  = optFrontendDir o </> "stack.yaml"

  srcs <- liftIO $ join <$> mapM (findWithSuffix ".hs") srcDirs

  build $ [frontendBin o] :. ("stack", [stackYaml]) |. buildFiles ++ srcs
