module KiCS2.BuildGenerator.Options
  ( Options (..)
  , defaultOptions, binDir, frontendDir, frontendBin
  , parseOptions
  ) where

import OptParse ( ParseSpec, optParser
                , option, long, short, metavar, help, optional
                , (<.>), (<>), parse
                )
import System.FilePath ( FilePath, (</>) )

data Options = Options
  { optRootDir       :: FilePath
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

-- | An OptParse options parser.
optionsParser :: ParseSpec (Options -> Options)
optionsParser = optParser $
      option (\rootDir o -> o { optRootDir = rootDir })
        (  long "root-dir"
        <> short "r"
        <> metavar "ROOT-DIR"
        <> help "The 'kics2' root directory (i.e. this cloned repository)."
        <> optional
        )

-- | Parses options from arguments.
parseOptions :: String -> [String] -> Either String Options
parseOptions prog args = case args of
  -- Work around the fact that empty args are always parsed as a left/an error.
  [] -> Right defaultOptions
  _  -> do
    fs <- parse (unwords args) optionsParser prog
    Right $ (foldr (.) id fs) defaultOptions
