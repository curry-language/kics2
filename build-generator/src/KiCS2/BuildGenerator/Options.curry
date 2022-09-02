module KiCS2.BuildGenerator.Options
  ( Options (..)
  , defaultOptions
  ) where

import System.FilePath ( FilePath )

data Options = Options
  { optBinDir :: FilePath
  , optFrontendDir :: FilePath
  }

defaultOptions :: Options
defaultOptions = Options
  { optBinDir = "bin"
  , optFrontendDir = "frontend"
  }
