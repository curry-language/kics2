------------------------------------------------------------------------------
--- Compiler options for the ID-based curry compiler
---
--- @author Fabian Reck, Bjoern Peemoeller
--- @version May 2011
------------------------------------------------------------------------------
module CompilerOpts
  ( Options (..), Verbosity (..), DumpLevel (..), defaultOptions, compilerOpts, debugOptions
  ) where

import FileGoodies (splitPath)
import IO (hPutStrLn, stderr)
import List (nub)
import Maybe (fromMaybe)
import System (exitWith, getArgs, getProgName)

import GetOpt
import Installation (compilerName, majorVersion, minorVersion, compilerDate)

type Options =
  { optHelp               :: Bool        -- show usage and exit
  , optVersion            :: Bool        -- show version and exit
  , optVerbosity          :: Verbosity   -- verbosity level
  , optForce              :: Bool        -- force recompilation
  , optImportPaths        :: [String]    -- directories searched for imports
  , optOutputSubdir       :: String      -- subdirectory for compiled modules
  , optDetOptimization    :: Bool        -- optimization for deterministic functions
  , optDump               :: [DumpLevel] -- dump intermediate results
  , optXNoImplicitPrelude :: Bool        -- don't implicitly import Prelude
  }

data Verbosity
  = VerbQuiet    -- be quiet
  | VerbStatus   -- show compilation status
  | VerbFrontend -- additionally show frontend infos
  | VerbAnalysis -- additionally show analysis infos
  | VerbDetails  -- additionally show details

data DumpLevel
  = DumpFlat        -- dump flat curry
  | DumpLifted      -- dump flat curry after case lifting
  | DumpRenamed     -- dump renamed flat curry
  | DumpFunDecls    -- dump transformed function declarations
  | DumpTypeDecls   -- dump transformed type declarations
  | DumpAbstractHs  -- dump abstract Haskell

allDumps :: [DumpLevel]
allDumps = [ DumpFlat    , DumpLifted   , DumpRenamed
           , DumpFunDecls, DumpTypeDecls, DumpAbstractHs]

defaultOptions :: Options
defaultOptions =
  { optHelp               = False
  , optVersion            = False
  , optVerbosity          = VerbStatus
  , optForce              = False
  , optImportPaths        = []
  , optOutputSubdir       = "/.curry/kics2/"
  , optDetOptimization    = True
  , optDump               = []
  , optXNoImplicitPrelude = False
  }

debugOptions = { optVerbosity := VerbDetails , optForce := True | defaultOptions }

parseVerbosity :: String -> Verbosity -> Verbosity
parseVerbosity s v = case s of
  "0" -> VerbQuiet
  "1" -> VerbStatus
  "2" -> VerbFrontend
  "3" -> VerbAnalysis
  "4" -> VerbDetails
  _   -> v

options :: [OptDescr (Options -> Options)]
options =
  [ Option ['h', '?'] ["help"]
      (NoArg (\opts -> { optHelp      := True | opts }))
      "show usage information"
  , Option ['V'] ["version"]
      (NoArg (\opts -> { optVersion   := True | opts }))
      "show version number"
  , Option ['v'] ["verbosity"]
      (ReqArg (\arg opts -> { optVerbosity :=
        parseVerbosity arg (opts -> optVerbosity) | opts }) "<n>")
      "set verbosity (0 = quiet, 1 = + status, 2 = + frontend, 3 = + nd-analysis, 4 = + dump-all)"
  , Option ['q'] ["quiet"]
      (NoArg (\opts -> { optVerbosity := VerbQuiet | opts }))
      "run in quiet mode"
  , Option ['f'] ["force"]
      (NoArg (\opts -> { optForce     := True | opts }))
      "force recompilation"
  , Option ['i'] ["import-dir"]
      (ReqArg (\arg opts -> { optImportPaths :=
        nub (opts -> optImportPaths ++ splitPath arg) | opts }) "DIR")
      "search for imports in DIR"
  , Option ['o'] ["output-subdir"]
      (ReqArg (\arg opts -> { optOutputSubdir := arg | opts }) "SUBDIR")
      "output compiled modules to SUBDIR"
  , Option [] ["no-opt"]
      (NoArg (\opts -> { optDetOptimization := False | opts } ))
      "disable optimization for deterministic functions"
  , Option [] ["dump-flat"]
      (NoArg (\opts -> { optDump :=
        nub (DumpFlat : opts -> optDump) | opts }))
      "dump flat curry representation"
  , Option [] ["dump-lifted"]
      (NoArg (\opts -> { optDump :=
        nub (DumpLifted : opts -> optDump) | opts }))
      "dump flat curry after case lifting"
  , Option [] ["dump-abstract-hs"]
      (NoArg (\opts -> { optDump :=
        nub (DumpAbstractHs : opts -> optDump) | opts }))
      "dump abstract Haskell representation"
  , Option [] ["dump-fun-decls"]
      (NoArg (\opts -> { optDump :=
        nub (DumpFunDecls : opts -> optDump) | opts }))
      "dump transformed function declarations"
  , Option [] ["dump-type-decls"]
      (NoArg (\opts -> { optDump :=
        nub (DumpTypeDecls : opts -> optDump) | opts }))
      "dump transformed type declarations"
  , Option [] ["dump-renamed"]
      (NoArg (\opts -> { optDump :=
        nub (DumpRenamed : opts -> optDump) | opts }))
      "dump renamed abstract Haskell representation"
  , Option [] ["dump-all"]
      (NoArg (\opts -> { optDump := allDumps | opts }))
      "dump all intermediate results"
  , Option ['x'] ["x-no-implicit-prelude"]
      (NoArg (\opts -> { optXNoImplicitPrelude := True | opts }))
      "do not implicitly import Prelude"
  ]

versionString :: String
versionString = concat
  [ compilerName
  , " (Version " ++ show majorVersion ++ '.' : show minorVersion
  , " of " ++ compilerDate ++ ")"
  ]

parseOpts :: [String] -> (Options, [String], [String])
parseOpts args = (foldl (flip ($)) defaultOptions opts, files, errs)
  where (opts, files, errs) = getOpt Permute options args

checkOpts :: Options -> [String] -> [String]
checkOpts _ []    = ["no files"]
checkOpts _ (_:_) = []

printVersion :: IO a
printVersion = do
  putStrLn versionString
  exitWith 0

printUsage :: String -> IO a
printUsage prog = do
  putStrLn $ usageInfo header options
  exitWith 0
    where header = "usage: " ++ prog ++ " [OPTION] ... MODULE ..."

badUsage :: String -> [String] -> IO a
badUsage prog [] = do
  hPutStrLn stderr $ "Try '" ++ prog ++ " --help' for more information"
  exitWith 1
badUsage prog (err:errs) = hPutStrLn stderr err >> badUsage prog errs

compilerOpts :: IO (Options, [String])
compilerOpts = do
  args <- getArgs
  prog <- getProgName
  processOpts prog $ parseOpts args

processOpts :: String -> (Options, [String], [String]) -> IO (Options, [String])
processOpts prog (opts, files, errs)
  | opts -> optHelp    = printUsage prog
  | opts -> optVersion = printVersion
  | not (null errs')   = badUsage prog errs'
  | otherwise          = return (opts, files)
    where errs' = errs ++ checkOpts opts files
