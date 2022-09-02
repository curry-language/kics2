module KiCS2.BuildGenerator.Frontend
  ( frontendNinja
  ) where

import Control.Monad ( join )
import Control.Monad.IO.Class ( liftIO )
import KiCS2.BuildGenerator.Options ( Options (..), optFrontendDir )
import KiCS2.BuildGenerator.Utils ( findWithSuffix, walk )
import Language.Ninja.Types
import Language.Ninja.Builder ( NinjaBuilder, build, rule )
import System.FilePath ( (</>) )

-- | The Ninja source for building the frontend.
frontendNinja :: Options -> NinjaBuilder ()
frontendNinja o = do
  let srcDirs    = (optFrontendDir o </>) <$> ["src", "app"]
      buildFiles = (optFrontendDir o </>) <$> ["package.yaml", "curry-frontend.cabal"]

  srcs <- liftIO $ join <$> mapM (findWithSuffix ".hs") srcDirs

  build $ [optBinDir o </> "kics2-frontend"]
    :. ("stack", [optFrontendDir o </> "stack.yaml"])
    |. buildFiles ++ srcs
