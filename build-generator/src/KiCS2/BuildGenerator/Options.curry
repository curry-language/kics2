module KiCS2.BuildGenerator.Options
  ( Options (..)
  , defaultOptions, binDir, frontendDir, frontendBin
  ) where

import System.FilePath ( FilePath, (</>) )

data Options = Options
  { optRootDir :: FilePath
  }

-- | The default options for building KiCS2.
defaultOptions :: Options
defaultOptions = Options
  { optRootDir = "."
  }

-- ] The path to the directory for built binaries.
binDir :: Options -> FilePath
binDir o = optRootDir o </> "bin"

-- ] The path to the frontend submodule.
frontendDir :: Options -> FilePath
frontendDir o = optRootDir o </> "frontend"

-- | The path to the (to-be) built frontend binary.
frontendBin :: Options -> FilePath
frontendBin o = binDir o </> "kics2-frontend"
