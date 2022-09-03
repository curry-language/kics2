module KiCS2.BuildGenerator.Options
  ( Options (..)
  , defaultOptions, optFrontendDir, optFrontendBin, optBinDir, optLibDir, optSrcDir
  , optBootDir, optLocalBinDir, optKics2cBin, optKics2iBin, optDotCpmDir, optPackageJson
  , optionsNinja
  , parseOptions
  ) where

import OptParse ( ParseSpec, optParser
                , option, long, short, metavar, help, optional
                , (<.>), (<>), parse
                )
import System.FilePath ( FilePath, (</>) )
import Language.Ninja.Builder ( NinjaBuilder, var )
import Language.Ninja.Types ( (=.) )

data Options = Options
  { optCurry           :: String
  , optVersion         :: String
  , optStack           :: String
  , optRootDir         :: FilePath
  , optStackResolver   :: String
  , optRuntimeIdSupply :: String
  , optGhcOpts         :: String
  }

-- | The default options for building KiCS2.
defaultOptions :: Options
defaultOptions = Options
  { optCurry = "curry"
  , optVersion = "3.0.0"
  , optStack = "stack"
  , optRootDir = "."
  , optStackResolver = "ghc-9.2.4"
  , optRuntimeIdSupply = "idsupplyinteger"
  , optGhcOpts = "-O2 -fno-strictness -fno-liberate-case"
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
  <.> option (\version o -> o { optVersion = version })
        (  long "version"
        <> short "v"
        <> metavar "VERSION"
        <> help "The KiCS2 version."
        <> optional)
  <.> option (\stack o -> o { optStack = stack })
        (  long "stack"
        <> metavar "STACK"
        <> help "The Haskell Stack binary to use."
        <> optional)
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
  <.> option (\ghcopts o -> o { optGhcOpts = ghcopts })
        (  long "ghcopts"
        <> short "g"
        <> metavar "OPTS"
        <> help "GHC options to use. By default this includes optimizations."
        <> optional
        )

-- | The Ninja source declaring non-path-related options as variables.
optionsNinja :: Options -> NinjaBuilder ()
optionsNinja o = do
  var $ "curry" =. optCurry o
  var $ "version" =. optVersion o
  var $ "stack" =. optStack o
  var $ "resolver" =. optStackResolver o
  var $ "idsupply" =. optRuntimeIdSupply o
  var $ "ghcopts" =. optGhcOpts o

  var $ "ghc" =. optStack o ++ " exec -- ghc"
  var $ "cypm" =. optCurry o ++ " cypm"

-- | The path to the directory for built binaries.
optBinDir :: Options -> FilePath
optBinDir o = optRootDir o </> "bin"

-- | The path to the directory for the KiCS2 standard libraries.
optLibDir :: Options -> FilePath
optLibDir o = optRootDir o </> "lib"

-- | The path to the directory for the KiCS2 sources.
optSrcDir :: Options -> FilePath
optSrcDir o = optRootDir o </> "src"

-- | The path to the directory for the KiCS2 bootstrapping sources.
optBootDir :: Options -> FilePath
optBootDir o = optRootDir o </> "boot"

-- | The path to the directory for built local binaries.
optLocalBinDir :: Options -> FilePath
optLocalBinDir o = optBinDir o </> ".local"

-- | The path to the built 'kics2c' compiler binary.
optKics2cBin :: Options -> FilePath
optKics2cBin o = optLocalBinDir o </> "kics2c"

-- | The path to the built 'kics2i' REPL binary.
optKics2iBin :: Options -> FilePath
optKics2iBin o = optLocalBinDir o </> "kics2i"

-- | The path to the KiCS2 .cpm directory.
optDotCpmDir :: Options -> FilePath
optDotCpmDir o = optRootDir o </> ".cpm"

-- | The path to the frontend submodule.
optFrontendDir :: Options -> FilePath
optFrontendDir o = optRootDir o </> "frontend"

-- | The path to the (to-be) built frontend binary.
optFrontendBin :: Options -> FilePath
optFrontendBin o = optBinDir o </> "kics2-frontend"

-- | The path to the KiCS2 CPM package manifest.
optPackageJson :: Options -> FilePath
optPackageJson o = optRootDir o </> "package.json"

-- | Parses options from arguments.
parseOptions :: String -> [String] -> Either String Options
parseOptions prog args = do
  fs <- parse (unwords args) optionsParser prog
  Right $ (foldr (.) id fs) defaultOptions
