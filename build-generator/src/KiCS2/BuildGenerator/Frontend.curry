module KiCS2.BuildGenerator.Frontend
  ( frontendNinja
  ) where

import Control.Monad.IO.Class ( liftIO )
import KiCS2.BuildGenerator.Options ( Options (..), optFrontendDir )
import KiCS2.BuildGenerator.Utils ( findWithSuffix )
import Language.Ninja.Types
import Language.Ninja.Builder ( NinjaBuilder, build, rule )
import System.FilePath ( (</>) )

-- | The Ninja source for building the frontend.
frontendNinja :: Options -> NinjaBuilder ()
frontendNinja o = do
  srcs <- liftIO $ findWithSuffix ".hs" $ optFrontendDir o </> "src"

  build $ [optBinDir o </> "kics2-frontend"]
    :. ("stack", [optFrontendDir o </> "stack.yaml"])
    |. srcs
