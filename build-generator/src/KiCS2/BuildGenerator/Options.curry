module KiCS2.BuildGenerator.Options
  ( Options (..)
  , defaultOptions, optFrontendDir, optFrontendBin, optBinDir, optLibDir, optLibSrcDir, optSrcDir
  , optBootDir, optLocalBinDir, optKics2cBin, optKics2iBin, optDotCpmDir, optPackageJson
  , optionVars, optionsNinja
  , parseOptions
  ) where

import Data.Char ( toLower )
import Data.List ( splitOn )
import OptParse ( ParseSpec, optParser
                , option, long, short, metavar, help, optional
                , (<.>), (<>), parse
                )
import KiCS2.BuildGenerator.Utils ( forM_ )
import Language.Ninja.Builder ( NinjaBuilder, var )
import Language.Ninja.Types ( (=.) )
import System.FilePath ( FilePath, (</>) )

data Options = Options
  { optCurry            :: String
  , optVersion          :: String
  , optBuildVersion     :: String
  , optCompilerDate     :: String
  , optInstallDate      :: String
  , optBaseVersion      :: String
  , optStack            :: String
  , optRootDir          :: FilePath
  , optStackResolver    :: String
  , optGhcVersion       :: String
  , optRuntimeIdSupply  :: String
  , optGhcOpts          :: String
  , optGhcOptimizations :: String
  , optKics2cOpts       :: String
  }

-- | The default options for building KiCS2.
defaultOptions :: Options
defaultOptions = Options
  { optCurry = "curry"
  , optVersion = ".."
  , optBuildVersion = ""
  , optCompilerDate = ""
  , optInstallDate = ""
  , optBaseVersion = ""
  , optStack = "stack"
  , optRootDir = "."
  , optStackResolver = "ghc-" ++ ghcVersion
  , optGhcVersion = ghcVersion
  , optRuntimeIdSupply = "idsupplyinteger"
  , optGhcOpts = "-v1 -cpp -fno-strictness -fno-liberate-case"
  , optGhcOptimizations = "-O2"
  , optKics2cOpts = "-v2 --parse-options=-Wall"
  }
  where
    ghcVersion = "9.2.4"

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
        <> optional
        )
  <.> option (\build o -> o { optBuildVersion = build })
        (  long "build-version"
        <> short "b"
        <> metavar "BUILDVERSION"
        <> help "The KiCS2 build version. >0 if this is a pre-release."
        <> optional
        )
  <.> option (\date o -> o { optCompilerDate = date })
        (  long "compiler-date"
        <> short "d"
        <> metavar "COMPILERDATE"
        <> help "The date of the compiler version."
        <> optional
        )
  <.> option (\date o -> o { optInstallDate = date })
        (  long "install-date"
        <> metavar "INSTALLDATE"
        <> help "The date of the installation."
        <> optional
        )
  <.> option (\version o -> o { optBaseVersion = version })
        (  long "base-version"
        <> metavar "VERSION"
        <> help "The base libraries version."
        <> optional
        )
  <.> option (\stack o -> o { optStack = stack })
        (  long "stack"
        <> metavar "STACK"
        <> help "The Haskell Stack binary to use."
        <> optional
        )
  <.> option (\rootDir o -> o { optRootDir = rootDir })
        (  long "root"
        <> short "r"
        <> metavar "ROOT"
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
  <.> option (\opts o -> o { optGhcOpts = opts })
        (  long "ghc-opts"
        <> short "g"
        <> metavar "OPTS"
        <> help "Additional GHC options to use."
        <> optional
        )
  <.> option (\opts o -> o { optGhcOptimizations = opts })
        (  long "ghc-optimizations"
        <> metavar "OPTS"
        <> help "GHC optimization options to use. By default this includes -O2."
        <> optional
        )
  <.> option (\opts o -> o { optKics2cOpts = opts })
        (  long "kics2c-opts"
        <> metavar "OPTS"
        <> help "Additional kics2c options to use."
        <> optional
        )

-- | Fetches the variables to be substituted into 'configured' (.in) files.
optionVars :: Options -> [(String, String)]
optionVars o =
  [ ("CURRY", optCurry o)
  , ("VERSION", version)
  , ("MAJORVERSION", major)
  , ("MINORVERSION", minor)
  , ("REVISIONVERSION", rev)
  , ("BUILDVERSION", optBuildVersion o)
  , ("COMPILERDATE", optCompilerDate o)
  , ("INSTALLDATE", optInstallDate o)
  , ("BASE_VERSION", optBaseVersion o)
  , ("GHC_VERSION", ghcVersion)
  , ("GHC_MAJOR", ghcMajor)
  , ("GHC_MINOR", ghcMinor)
  , ("STACK", optStack o)
  , ("STACKRESOLVER", optStackResolver o)
  , ("RUNTIME_IDSUPPLY", optRuntimeIdSupply o)
  , ("GHC_OPTS", optGhcOpts o)
  , ("GHC_OPTIMIZATIONS", optGhcOptimizations o)
  , ("GHC", optStack o ++ " exec -- ghc")
  , ("CYPM", optCurry o ++ " cypm")
  , ("KICS2HOME", optRootDir o)
  , ("KICS2C_OPTS", optKics2cOpts o)
  ]
  where
    version    = optVersion o
    ghcVersion = optGhcVersion o
    (major, minor, rev)     = parseVersion version
    (ghcMajor, ghcMinor, _) = parseVersion ghcVersion

parseVersion :: String -> (String, String, String)
parseVersion version =  case splitOn "." version of
  [m, n]    -> (m, n, "")
  [m, n, r] -> (m, n, r)
  _         -> error $ "Invalid version: " ++ version

-- | The Ninja source declaring non-path-related options as variables.
optionsNinja :: Options -> NinjaBuilder ()
optionsNinja o = forM_ (optionVars o) $ \(name, value) ->
  var $ (toLower <$> name) =. value

-- | The path to the directory for built binaries.
optBinDir :: Options -> FilePath
optBinDir o = optRootDir o </> "bin"

-- | The path to the directory for the KiCS2 standard libraries.
optLibDir :: Options -> FilePath
optLibDir o = optRootDir o </> "lib"

-- | The path to the source directory for the KiCS2 standard libraries.
optLibSrcDir :: Options -> FilePath
optLibSrcDir o = optRootDir o </> "lib-trunk"

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
