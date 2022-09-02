module KiCS2.BuildGenerator.Build.Frontend
  ( frontendNinja
  ) where

import Control.Monad ( join )
import Control.Monad.IO.Class ( liftIO )
import KiCS2.BuildGenerator.Options ( Options (..), frontendDir, frontendBin )
import KiCS2.BuildGenerator.Utils ( findWithSuffix, walk )
import Language.Ninja.Types
import Language.Ninja.Builder ( NinjaBuilder, build, rule )
import System.FilePath ( (</>) )

-- | The Ninja source for building the frontend.
frontendNinja :: Options -> NinjaBuilder ()
frontendNinja o = do
  let srcDirs    = (frontendDir o </>) <$> ["src", "app"]
      buildFiles = (frontendDir o </>) <$> ["curry-frontend.cabal"]
      stackYaml  = frontendDir o </> "stack.yaml"

  srcs <- liftIO $ join <$> mapM (findWithSuffix ".hs") srcDirs

  build $ [frontendBin o] :. ("stack", [stackYaml]) |. buildFiles ++ srcs
