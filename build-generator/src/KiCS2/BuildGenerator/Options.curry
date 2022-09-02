module KiCS2.BuildGenerator.Options
  ( Options (..)
  , defaultOptions, frontendBin
  ) where

import System.FilePath ( FilePath, (</>) )

data Options = Options
  { optBinDir      :: FilePath
  , optFrontendDir :: FilePath
  }

-- | The default options for building KiCS2.
defaultOptions :: Options
defaultOptions = Options
  { optBinDir = "bin"
  , optFrontendDir = "frontend"
  }

-- | The path to the (to-be) built frontend binary.
frontendBin :: Options -> FilePath
frontendBin o = optBinDir o </> "kics2-frontend"
