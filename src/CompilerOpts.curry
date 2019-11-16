------------------------------------------------------------------------------
--- Compiler options for the KiCS2 Curry compiler
---
--- @author Fabian Reck, Bjoern Peemoeller
--- @version April 2015
------------------------------------------------------------------------------
module CompilerOpts
  ( Options (..), Verbosity (..), OptimLevel (..), DumpFormat (..)
  , Extension (..), defaultOptions, getCompilerOpts
  ) where

import Data.List             (intercalate, maximum, nub)
import System.FilePath       ((</>), splitSearchPath)
import System.Console.GetOpt
import System.IO             (hPutStrLn, stderr)
import System.Process        (exitWith)
import System.Environment    (getArgs, getProgName)

import Installation (compilerName, majorVersion, minorVersion, compilerDate)

--- Version string
version :: String
version = concat
  [ compilerName
  , " (Version " ++ show majorVersion ++ '.' : show minorVersion
  , " of " ++ compilerDate ++ ")"
  ]

--- Compiler options
data Options = Opts
  { optHelp               :: Bool         -- show usage and exit
  , optVersion            :: Bool         -- show version and exit
  , optVerbosity          :: Verbosity    -- current verbosity level
  , optMainVerbosity      :: Verbosity    -- verbosity level of main module
  , optForce              :: Bool         -- force recompilation
  , optImportPaths        :: [String]     -- directories searched for imports
  , optOutputSubdir       :: String       -- subdirectory for compiled modules
  , optOptimization       :: OptimLevel   -- level of optimization
  , optExtensions         :: [Extension]  -- language extensions
  , optDump               :: [DumpFormat] -- dump intermediate results
  , optParser             :: String       -- additional front-end options
  , optTraceFailure       :: Bool         -- trace failures
  , rcVars                :: [(String, String)] -- content of rc file
  }

--- Default compiler options
defaultOptions :: Options
defaultOptions = Opts
  { optHelp          = False
  , optVersion       = False
  , optVerbosity     = VerbStatus
  , optMainVerbosity = VerbStatus
  , optForce         = False
  , optImportPaths   = []
  , optOutputSubdir  = ".curry" </> "kics2"
  , optOptimization  = OptimStrictSupply
  , optExtensions    = []
  , optDump          = []
  , optParser        = ""
  , optTraceFailure  = False
  , rcVars           = []
  }

--- Verbosity levels of the compiler
data Verbosity
  = VerbQuiet    -- be quiet
  | VerbFrontend -- show frontend status
  | VerbStatus   -- show own compilation status
  | VerbAnalysis -- additionally show analysis infos
  | VerbDetails  -- additionally show current transformation for each module
  deriving (Eq, Ord)

--- Description and flag of verbosities
verbosities :: [(Verbosity, String, String)]
verbosities = [ (VerbQuiet   , "0", "quiet"           )
              , (VerbFrontend, "1", "frontend status" )
              , (VerbStatus  , "2", "own status"      )
              , (VerbAnalysis, "3", "analysis"        )
              , (VerbDetails , "4", "details"         )
              ]

--- Levels of optimization
data OptimLevel
  = OptimNone         -- no optimization
  | OptimHigherOrder  -- higher-order optimization
  | OptimStrictSupply -- strict evaluation of supplies
  deriving (Eq, Ord)

--- Description and flag of optimization levels
optimizations :: [(OptimLevel, String, String)]
optimizations = [ (OptimNone        , "0", "no optimization"                   )
                , (OptimHigherOrder , "1", "higher order"                      )
                , (OptimStrictSupply, "2", "strict supply evaluation (default)")
                ]

--- Known language extensions
data Extension
  = AnonFreeVars       -- anonymous free variables
  | FunctionalPatterns -- functional patterns
  | NoImplicitPrelude  -- no implicit import of the prelude
  deriving Eq

--- Description and flag of language extensions
extensions :: [(Extension, String, String)]
extensions =
  [ ( AnonFreeVars      , "AnonFreeVars"
    , "enable anonymous free variables"     )
  , ( FunctionalPatterns, "FunctionalPatterns"
    , "enable functional patterns"          )
  , ( NoImplicitPrelude , "NoImplicitPrelude"
    , "do not implicitly import the Prelude")
  ]

--- Dump formats of the compiler
data DumpFormat
  = DumpTypedFlat   -- dump typed flat curry
  | DumpExtImports  -- dump typed flat curry with extended import list
  | DumpLifted      -- dump flat curry after case lifting
  | DumpEliminated  -- dump flat curry after cond elimination
  | DumpDefaulted   -- dump flat curry after defaulting
  | DumpRenamed     -- dump renamed flat curry
  | DumpFunDecls    -- dump transformed function declarations
  | DumpTypeDecls   -- dump transformed type declarations
  | DumpTranslated  -- dump abstract Haskell
  deriving Eq

--- Description and flag of dump levels
dumpLevel :: [(DumpFormat, String, String)]
dumpLevel =
  [ (DumpTypedFlat , "dump-typed"     , "typed FlatCurry"               )
  , (DumpExtImports, "dump-imports"   , "result of completing imports"  )
  , (DumpLifted    , "dump-lifted"    , "result of case lifting"        )
  , (DumpEliminated, "dump-condelim"  , "result of cond elimination"    )
  , (DumpDefaulted , "dump-defaulted" , "result of type defaulting"     )
  , (DumpRenamed   , "dump-renamed"   , "result of renaming"            )
  , (DumpFunDecls  , "dump-fun-decls" , "result of function translation")
  , (DumpTypeDecls , "dump-type-decls", "result of type translation"    )
  , (DumpTranslated, "dump-trans"     , "result of entire translation"  )
  ]

-- -----------------------------------------------------------------------------
-- Helper functions.
--
-- Because some flags require additional arguments, the structure is slightly
-- more complicated to enable malformed arguments to be reported.
-- -----------------------------------------------------------------------------

type OptErr = (Options, [String])

type OptErrTable = [(String, String, Options -> Options)]

onOpts :: (Options -> Options) -> OptErr -> OptErr
onOpts f (opts, errs) = (f opts, errs)

onOptsArg :: (String -> Options -> Options) -> String -> OptErr -> OptErr
onOptsArg f arg (opts, errs) = (f arg opts, errs)

addErr :: String -> OptErr -> OptErr
addErr err (opts, errs) = (opts, errs ++ [err])

mkOptErrOption :: String -> [String] -> String -> String -> OptErrTable
               -> OptDescr (OptErr -> OptErr)
mkOptErrOption flags longFlags arg what tbl = Option flags longFlags
  (ReqArg (parseOptErr what tbl) arg)
  ("set " ++ what ++ " `" ++ arg ++ "', where `" ++ arg ++ "' is one of\n"
    ++ renderOptErrTable tbl)

parseOptErr :: String -> OptErrTable -> String -> OptErr -> OptErr
parseOptErr what table opt = case lookup3 opt table of
  Just f  -> onOpts f
  Nothing -> addErr $ "unrecognized " ++ what ++ '`' : opt ++ "'\n"
 where
  lookup3 _ []                  = Nothing
  lookup3 k ((k', _, v2) : kvs)
    | k == k'                   = Just v2
    | otherwise                 = lookup3 k kvs

renderOptErrTable :: OptErrTable -> String
renderOptErrTable ds
  = intercalate "\n" $ map (\(k, d, _) -> rpad maxLen k ++ ": " ++ d) ds
  where
  maxLen = maximum $ map (\(k, _, _) -> length k) ds
  rpad n x = x ++ replicate (n - length x) ' '

-- -----------------------------------------------------------------------------
-- Specification of the command line options
-- -----------------------------------------------------------------------------

--- Command line options
options :: [OptDescr (OptErr -> OptErr)]
options =
  [ Option ['h', '?'] ["help"]
      (NoArg (onOpts $ \opts -> opts { optHelp    = True }))
      "display this help and exit"
  , Option ['V'] ["version"]
      (NoArg (onOpts $ \opts -> opts { optVersion = True }))
      "show the version number and exit"
  , mkOptErrOption "v" ["verbosity"] "n" "verbosity level" verbDescriptions
  , Option ['f'] ["force"]
      (NoArg (onOpts $ \opts -> opts { optForce   = True }))
      "force recompilation of target files"
  , Option ['i'] ["import-dir"]
      (ReqArg (onOptsArg $ \arg opts -> opts { optImportPaths = nub
        ((optImportPaths opts) ++ splitSearchPath arg) }) "dir[:dir]")
      "search for imports in `dir[:dir]'"
  , Option ['o'] ["output-subdir"]
      (ReqArg (onOptsArg $ \arg opts -> opts { optOutputSubdir = arg }) "dir")
      "output compiled modules to `dir'"
  , mkOptErrOption ['X'] [] "ext" "language extension" extDescriptions
  , mkOptErrOption ['O'] [] "n"   "optimization level" optimDescriptions
  , mkOptErrOption ['d'] [] "opt" "debug option"       dumpDescriptions
  , Option [] ["parse-options"]
      (ReqArg (onOptsArg $ \arg opts -> opts { optParser = arg }) "options")
      "additional options for the parser"
  , Option [] ["trace-failure"]
      (NoArg (onOpts $ \opts -> opts { optTraceFailure = True }))
      "Trace failures in deterministic program"
  ]

--- Verbosity descriptions
verbDescriptions :: OptErrTable
verbDescriptions = map toDescr verbosities
  where
  toDescr (flag, name, desc) = (name, desc, set flag)
  set f opts = opts { optVerbosity = f }

--- Optimization descriptions
optimDescriptions :: OptErrTable
optimDescriptions = map toDescr optimizations
  where
  toDescr (flag, name, desc) = (name, desc, set flag)
  set f opts = opts { optOptimization = f }

--- Extension descriptions
extDescriptions :: OptErrTable
extDescriptions = map toDescr extensions
  where
  toDescr (flag, name, desc) = (name, desc, set flag)
  set f opts = opts { optExtensions = addFlag f (optExtensions opts) }

--- Dump descriptions
dumpDescriptions :: OptErrTable
dumpDescriptions =
  [ ( "dump-all", "dump everything"
    , \ opts -> opts { optDump = map (\ (l, _, _) -> l) dumpLevel })
  , ( "dump-none", "dump nothing"
    , \ opts -> opts { optDump = []                               })
  ] ++ map toDescr dumpLevel
  where
  toDescr (flag, name, desc) = (name , "dump " ++ desc, set flag)
  set f opts = opts { optDump = addFlag f (optDump opts) }

addFlag :: Eq a => a -> [a] -> [a]
addFlag o opts = nub $ o : opts

removeFlag :: Eq a => a -> [a] -> [a]
removeFlag o opts = filter (/= o) opts

-- -----------------------------------------------------------------------------
-- Parsing of the command line options
-- -----------------------------------------------------------------------------

--- Retrieve the options specified at the command line and the input file names.
--- This operation directly handles the following cases and exits the program
--- (exit code in parentheses) if one of them occurs
---  * The usage should be printed (0)
---  * The version should be printed (0)
---  * There were errors in the specified options (1)
---  * no files for compilation are given (1)
getCompilerOpts :: IO (Options, [String])
getCompilerOpts = do
  -- Ignore "-Dprop=val" parameters since they are processed in main
  args <- getArgs >>= return . filter (\a -> take 2 a /= "-D")
  prog <- getProgName
  processOpts prog $ parseOpts args

--- Parse the command line arguments.
parseOpts :: [String] -> (Options, [String], [String])
parseOpts args = (opts, files, errs ++ errs2)
  where
  (opts, errs2)          = foldl (flip ($)) (defaultOptions, []) optErrs
  (optErrs, files, errs) = getOpt Permute options args

--- Process the parsed command line arguments.
processOpts :: String -> (Options, [String], [String])
            -> IO (Options, [String])
processOpts prog (opts, files, errs)
  | optHelp opts     = printUsage prog
  | optVersion opts  = printVersion
  | not (null errs') = badUsage prog errs'
  | otherwise        = return (opts, files)
    where errs' = errs ++ checkOpts opts files

--- Check the parsed command line arguments for errors.
checkOpts :: Options -> [String] -> [String]
checkOpts _ []    = ["no files"]
checkOpts _ (_:_) = []

--- Print the usage information.
printUsage :: String -> IO a
printUsage prog = do
  putStrLn $ usageInfo header options
  exitWith 0
 where header = "usage: " ++ prog ++ " [OPTION] ... MODULE ..."

--- Complain about a bad usage.
badUsage :: String -> [String] -> IO a
badUsage prog errs = do
  mapM_ (hPutStrLn stderr) errs
  hPutStrLn stderr $ "Try '" ++ prog ++ " --help' for more information"
  exitWith 1

--- Print the usage information.
printVersion :: IO a
printVersion = do
  putStrLn version
  exitWith 0
