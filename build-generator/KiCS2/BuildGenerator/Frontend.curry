module KiCS2.BuildGenerator.Frontend
  ( frontendNinja
  ) where

import KiCS2.BuildGenerator.Options ( Options (..) )
import Ninja.Types
import Ninja.Builder ( NinjaBuilder, build, rule )
import System.FilePath ( (</>) )

-- | The Ninja source for building the frontend.
frontendNinja :: Options -> NinjaBuilder ()
frontendNinja o = do
  build $ [optBinDir o </> "kics2-frontend"] :. ("stack", [])
