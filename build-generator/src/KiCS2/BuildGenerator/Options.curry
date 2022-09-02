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
  { optCurry           :: String
  , optRootDir         :: FilePath
  , optStackResolver   :: String
  , optRuntimeIdSupply :: String
  }

-- | The default options for building KiCS2.
defaultOptions :: Options
defaultOptions = Options
  { optCurry = "curry"
  , optRootDir = "."
  , optStackResolver = "ghc-9.2.4"
  , optRuntimeIdSupply = "idsupplyinteger"
  }

-- | An OptParse options parser.
optionsParser :: ParseSpec (Options -> Options)
optionsParser = optParser $
      option (\curry o -> o { optCurry = curry })
        (  long "curry"
        <> short "c"
        <> metavar "CURRY"
        <> help "The Curry compiler to bootstrap with."
        <> optional
        )
  <.> option (\rootDir o -> o { optRootDir = rootDir })
        (  long "root-dir"
        <> short "r"
        <> metavar "ROOT-DIR"
        <> help "The 'kics2' root directory (i.e. this cloned repository)."
        <> optional
        )
  <.> option (\resolver o -> o { optStackResolver = resolver })
        (  long "stack-resolver"
        <> short "s"
        <> metavar "RESOLVER"
        <> help "The Stack resolver to use."
        <> optional
        )
  <.> option (\idsupply o -> o { optRuntimeIdSupply = idsupply })
        (  long "idsupply"
        <> short "i"
        <> metavar "IDSUPPLY"
        <> help "The id supply to use in the KiCS2 runtime. Some implementations have a dependency on GHC's UniqSupply and thus require a dependency on 'ghc' too (which in turn adds shared library dependencies on libtinfo etc.)"
        <> optional
        )

-- ] The path to the directory for built binaries.
binDir :: Options -> FilePath
binDir o = optRootDir o </> "bin"

-- ] The path to the frontend submodule.
frontendDir :: Options -> FilePath
frontendDir o = optRootDir o </> "frontend"

-- | The path to the (to-be) built frontend binary.
frontendBin :: Options -> FilePath
frontendBin o = binDir o </> "kics2-frontend"

-- | Parses options from arguments.
parseOptions :: String -> [String] -> Either String Options
parseOptions prog args = do
  fs <- parse (unwords args) optionsParser prog
  Right $ (foldr (.) id fs) defaultOptions
