------------------------------------------------------------------------------
--- This module contains operations to execute the front end of the
--- Curry system.
---
--- @author Bernd Brassel, Michael Hanus, Bjoern Peemoeller, Finn Teegen
--- @version December 2018
------------------------------------------------------------------------------

module System.FrontendExec
  (FrontendTarget(..)

  , FrontendParams, defaultParams, rcParams
  , quiet, extended, cpp, definitions, overlapWarn, fullPath, htmldir, logfile
  , specials, setQuiet, setExtended, setCpp, addDefinition, setDefinitions
  , setOverlapWarn, setFullPath, setHtmlDir, setLogfile, addTarget, setSpecials

  , callFrontend, callFrontendWithParams
  ) where

import Data.Char           ( toUpper )
import Data.List           ( intercalate, nub )
import Data.PropertyFile   ( getPropertiesFromFile )
import System.FilePath     ( FilePath, (</>), takeDirectory, takeFileName )
import System.Process      ( system )
import System.CurryPath    ( getLoadPathForModule )
import Language.Curry.Distribution 
                           ( curryCompiler, curryCompilerMajorVersion
                           , curryCompilerMinorVersion, installDir, rcFileName 
                           )

-------------------------------------------------------------------
-- calling the front end
-------------------------------------------------------------------

--- Data type for representing the different target files that can be produced
--- by the front end of the Curry compiler.
--- @cons FCY   - FlatCurry file ending with .fcy
--- @cons TFCY  - Typed FlatCurry file ending with .tfcy
--- @cons FINT  - FlatCurry interface file ending with .fint
--- @cons ACY   - AbstractCurry file ending with .acy
--- @cons UACY  - Untyped (without type checking) AbstractCurry file ending with .uacy
--- @cons HTML  - colored HTML representation of source program
--- @cons CY    - source representation employed by the frontend
--- @cons TOKS  - token stream of source program
--- @cons AST   - abstract syntax tree ending with .sast
--- @cons SAST  - shortened abstract syntax tree ending with .sast
--- @cons COMMS - comments stream ending with .cycom
data FrontendTarget = FCY | TFCY | FINT | ACY | UACY | HTML | CY | TOKS | TAFCY
                    | AST | SAST | COMMS
  deriving Eq

--- Abstract data type for representing parameters supported by the front end
--- of the Curry compiler.
-- The parameters are of the form
-- FrontendParams Quiet Extended Cpp NoOverlapWarn FullPath HtmlDir LogFile Specials
-- where
--   Quiet         - work silently
--   Extended      - support extended Curry syntax
--   Cpp           - enable conditional compiling
--   Definitions   - definitions for conditional compiling
--   OverlapWarn   - warn for overlapping rules
--   FullPath dirs - the complete list of directory names for loading modules
--   HtmlDir file  - output directory (only relevant for HTML target)
--   LogFile file  - store all output (including errors) of the front end in file
--   Targets       - additional targets for the front end
--   Specials      - additional special parameters (use with care!)
data FrontendParams =
  FrontendParams Bool
                 Bool
                 Bool
                 [(String, Int)]
                 Bool
                 (Maybe [String])
                 (Maybe String)
                 (Maybe String)
                 [FrontendTarget]
                 String

--- The default parameters of the front end.
defaultParams :: FrontendParams
defaultParams =
  FrontendParams False True False defaultDefs True Nothing Nothing Nothing [] ""
 where
  defaultDefs = [("__" ++ map toUpper curryCompiler ++ "__",
                  curryCompilerMajorVersion * 100 + curryCompilerMinorVersion)]

--- The default parameters of the front end as configured by the compiler
--- specific resource configuration file.
rcParams :: IO FrontendParams
rcParams = do
  rcfile <- rcFileName
  [mbExtended,mbOverlapWarn] <- getPropertiesFromFile rcfile
                                  ["curryextensions","warnoverlapping"]
  return $ setExtended    (mbExtended    /= Just "no")
         $ setOverlapWarn (mbOverlapWarn /= Just "no")
         $ defaultParams

--- Set quiet mode of the front end.
setQuiet :: Bool -> FrontendParams -> FrontendParams
setQuiet s (FrontendParams _ t u v w x y z ts sp) =
  FrontendParams s t u v w x y z ts sp

--- Set extended mode of the front end.
setExtended :: Bool -> FrontendParams -> FrontendParams
setExtended s (FrontendParams a _ u v w x y z ts sp) =
  FrontendParams a s u v w x y z ts sp

--- Set cpp mode of the front end.
setCpp :: Bool -> FrontendParams -> FrontendParams
setCpp s (FrontendParams a b _ v w x y z ts sp) =
  FrontendParams a b s v w x y z ts sp

--- Add cpp definition of the front end.
addDefinition :: (String, Int) -> FrontendParams -> FrontendParams
addDefinition d (FrontendParams a b c ds w x y z ts sp) =
  FrontendParams a b c (ds ++ [d]) w x y z ts sp

--- Set cpp definitions of the front end.
setDefinitions :: [(String, Int)] -> FrontendParams -> FrontendParams
setDefinitions s (FrontendParams a b c _ w x y z ts sp) =
  FrontendParams a b c s w x y z ts sp

--- Set overlap warn mode of the front end.
setOverlapWarn :: Bool -> FrontendParams -> FrontendParams
setOverlapWarn s (FrontendParams a b c d _ x y z ts sp) =
  FrontendParams a b c d s x y z ts sp

--- Set the full path of the front end.
--- If this parameter is set, the front end searches all modules
--- in this path (instead of using the default path).
setFullPath :: [String] -> FrontendParams -> FrontendParams
setFullPath s (FrontendParams a b c d e _ y z ts sp) =
  FrontendParams a b c d e (Just s) y z ts sp

--- Set the htmldir parameter of the front end.
--- Relevant for HTML generation.
setHtmlDir :: String -> FrontendParams -> FrontendParams
setHtmlDir s (FrontendParams a b c d e f _ z ts sp) =
  FrontendParams a b c d e f (Just s) z ts sp

--- Set the logfile parameter of the front end.
--- If this parameter is set, all messages produced by the front end
--- are stored in this file.
setLogfile :: String -> FrontendParams -> FrontendParams
setLogfile s (FrontendParams a b c d e f g _ ts sp) =
  FrontendParams a b c d e f g (Just s) ts sp

--- Set additional specials parameters of the front end.
--- These parameters are specific for the current front end and
--- should be used with care, since their form might change in the future.
setSpecials :: String -> FrontendParams -> FrontendParams
setSpecials s (FrontendParams a b c d e f g h ts _) =
  FrontendParams a b c d e f g h ts s

--- Add an additional front end target.
addTarget :: FrontendTarget -> FrontendParams -> FrontendParams
addTarget t (FrontendParams a b c d e f g h ts sp) =
  FrontendParams a b c d e f g h (t:ts) sp

--- Returns the value of the "quiet" parameter.
quiet :: FrontendParams -> Bool
quiet (FrontendParams x _ _ _ _ _ _ _ _ _) = x

--- Returns the value of the "extended" parameter.
extended :: FrontendParams -> Bool
extended (FrontendParams _ x _ _ _ _ _ _ _ _) = x

--- Returns the value of the "cpp" parameter.
cpp :: FrontendParams -> Bool
cpp (FrontendParams _ _ x _ _ _ _ _ _ _) = x

--- Returns the value of the "cpp" parameter.
definitions :: FrontendParams -> [(String, Int)]
definitions (FrontendParams _ _ _ x _ _ _ _ _ _) = x

--- Returns the value of the "overlapWarn" parameter.
overlapWarn :: FrontendParams -> Bool
overlapWarn (FrontendParams _ _ _ _ x _ _ _ _ _) = x

--- Returns the full path parameter of the front end.
fullPath :: FrontendParams -> Maybe [String]
fullPath (FrontendParams _ _ _ _ _ x _ _ _ _) = x

--- Returns the htmldir parameter of the front end.
htmldir :: FrontendParams -> Maybe String
htmldir  (FrontendParams _ _ _ _ _ _ x _ _ _) = x

--- Returns the logfile parameter of the front end.
logfile :: FrontendParams -> Maybe String
logfile  (FrontendParams _ _ _ _ _ _ _ x _ _) = x

--- Returns the special parameters of the front end.
targets :: FrontendParams -> [FrontendTarget]
targets (FrontendParams _ _ _ _ _ _ _ _ x _) = x

--- Returns the special parameters of the front end.
specials :: FrontendParams -> String
specials (FrontendParams _ _ _ _ _ _ _ _ _ x) = x

--- In order to make sure that compiler generated files (like .fcy, .fint, .acy)
--- are up to date, one can call the front end of the Curry compiler
--- with this action.
--- If the front end returns with an error, an exception is raised.
--- @param target - the kind of target file to be generated
--- @param progname - the name of the main module of the application to be compiled
callFrontend :: FrontendTarget -> String -> IO ()
callFrontend target p = do
  params <- rcParams
  callFrontendWithParams target params p

--- In order to make sure that compiler generated files (like .fcy, .fint, .acy)
--- are up to date, one can call the front end of the Curry compiler
--- with this action where various parameters can be set.
--- If the front end returns with an error, an exception is raised.
--- @param target - the kind of target file to be generated
--- @param params - parameters for the front end
--- @param modpath - the name of the main module possibly prefixed with a
---                  directory where this module resides
callFrontendWithParams :: FrontendTarget -> FrontendParams -> String -> IO ()
callFrontendWithParams target params modpath = do
  parsecurry <- callParseCurry
  let lf      = maybe "" id (logfile params)
      tgts    = nub (target : targets params)
      syscall = unwords $ [parsecurry] ++ map showFrontendTarget tgts ++
                          [showFrontendParams, cppParams, takeFileName modpath]
  retcode <- if null lf
             then system syscall
             else system (syscall ++ " > " ++ lf ++ " 2>&1")
  if retcode == 0
   then return ()
   else ioError (userError "Illegal source program")
 where
   callParseCurry = do
     path <- maybe (getLoadPathForModule modpath)
                   (\p -> return (nub (takeDirectory modpath : p)))
                   (fullPath params)
     return (quote (installDir </> "bin" </> curryCompiler ++ "-frontend")
             ++ concatMap ((" -i" ++) . quote) path)

   quote s = '"' : s ++ "\""

   showFrontendTarget FCY   = "--flat"
   showFrontendTarget TFCY  = "--typed-flat"
   showFrontendTarget TAFCY = "--type-annotated-flat"
   showFrontendTarget FINT  = "--flat"
   showFrontendTarget ACY   = "--acy"
   showFrontendTarget UACY  = "--uacy"
   showFrontendTarget HTML  = "--html"
   showFrontendTarget CY    = "--parse-only"
   showFrontendTarget TOKS  = "--tokens"
   showFrontendTarget AST   = "--ast"
   showFrontendTarget SAST  = "--short-ast"
   showFrontendTarget COMMS = "--comments"

   showFrontendParams = unwords
    [ if quiet       params then runQuiet     else ""
    , if extended    params then "--extended" else ""
    , if cpp         params then "--cpp"      else ""
    , if overlapWarn params then ""           else "--no-overlap-warn"
    , maybe "" ("--htmldir="++) (htmldir params)
    , specials params
    ]

   runQuiet = "--no-verb --no-warn --no-overlap-warn"

   cppParams = intercalate " " $ map showDefinition (definitions params)

   showDefinition (s, v) = "-D" ++ s ++ "=" ++ show v

------------------------------------------------------------------------------
