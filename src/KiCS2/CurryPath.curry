------------------------------------------------------------------------------
--- This module contains operations related to module names and paths
--- used in Curry system.
---
--- NOTE that KiCS2 uses a custom version of the module instead of the
--- 'currypath' package to make sure that e.g. a PAKCS-bootstrapped KiCS2
--- still yields the correct KiCS2 paths.
---
--- @author Bernd Brassel, Michael Hanus, Bjoern Peemoeller, Finn Teegen
--- @version December 2020
------------------------------------------------------------------------------

module KiCS2.CurryPath
  ( ModuleIdent
  , splitProgramName, splitValidProgramName, isValidModuleName
  , runModuleAction
  , splitModuleFileName, splitModuleIdentifiers  , joinModuleIdentifiers
  , stripCurrySuffix
  , ModulePath, modNameToPath
  , currySubdir, inCurrySubdir, inCurrySubdirModule, addCurrySubdir
  , sysLibPath, getLoadPathForModule
  , lookupModuleSourceInLoadPath, lookupModuleSource
  , curryrcFileName
  ) where

import Control.Monad       ( unless )
import Data.Char           ( toLower )
import Data.List           ( init, intercalate, last, split )
import System.Directory    ( doesFileExist, getCurrentDirectory
                           , getHomeDirectory, setCurrentDirectory )
import System.Environment  ( getEnv )
import System.Process      ( system )
import System.FilePath     ( FilePath, (</>), (<.>), addTrailingPathSeparator
                           , dropFileName, joinPath, splitDirectories
                           , splitExtension, splitFileName, splitPath
                           , splitSearchPath
                           , takeFileName, takeExtension, dropExtension
                           )

import Data.PropertyFile   ( getPropertyFromFile )

import Installation        ( compilerName, fullVersion, installDir )

------------------------------------------------------------------------------
--- Functions for handling file names of Curry modules
------------------------------------------------------------------------------

type ModuleIdent = String

--- Splits a program name, i.e., a module name possibly prefixed by
--- a directory, into the directory and the module name.
--- A possible suffix like `.curry` or `.lcurry` is dropped from the
--- module name.
--- For instance `splitProgramName "lib/Data.Set.curry"` evaluates
--- to `("lib","Data.Set")`.
splitProgramName :: String -> (FilePath, ModuleIdent)
splitProgramName s
  | null ps
  = (".", "")
  | null (tail ps)
  = (".", head ps)
  | otherwise
  = (concat (init ps), last ps)
 where
  ps = splitPath (stripCurrySuffix s)

--- Splits a program name, i.e., a module name possibly prefixed by
--- a directory, into the directory and a *valid* module name.
--- A possible suffix like `.curry` or `.lcurry` is dropped from the
--- module name.
--- For instance `splitValidProgramName "lib/Data.Set.curry"` evaluates
--- to `("lib","Data.Set")`.
--- An error is raised if the program name is empty or the module name
--- is not valid.
splitValidProgramName :: String -> (FilePath, ModuleIdent)
splitValidProgramName s
  | null mname
  = error $ "The module name is empty."
  | not (isValidModuleName mname)
  = error $ "The program name '" ++ s ++ "' contains an invalid module name."
  | otherwise
  = (dir,mname)
 where
  (dir,mname) = splitProgramName s

--- Is the given string a valid module name?
isValidModuleName :: String -> Bool
isValidModuleName = all isModId . split (=='.')
 where
  isModId []     = False
  isModId (c:cs) = isAlpha c && all (\x -> isAlphaNum x || x `elem` "_'") cs

--- Executes an I/O action, which is parameterized over a module name,
--- for a given program name. If the program name is prefixed by a directory,
--- switch to this directory before executing the action.
--- A possible suffix like `.curry` or `.lcurry` is dropped from the
--- module name passed to the action.
--- An error is raised if the module name is not valid.
runModuleAction :: (String -> IO a) -> String -> IO a
runModuleAction modaction progname = do
  let (progdir,mname) = splitValidProgramName progname
  curdir <- getCurrentDirectory
  unless (progdir == ".") $ do
    putStrLn $ "Switching to directory '" ++ progdir ++ "'..."
    setCurrentDirectory progdir
  result <- modaction mname
  unless (progdir == ".") $ setCurrentDirectory curdir
  return result

--- Split the `FilePath` of a module into the directory prefix and the
--- `FilePath` corresponding to the module name.
--- For instance, the call `splitModuleFileName "Data.Set" "lib/Data/Set.curry"`
--- evaluates to `("lib", "Data/Set.curry")`.
--- This can be useful to compute output directories while retaining the
--- hierarchical module structure.
splitModuleFileName :: ModuleIdent -> FilePath -> (FilePath, FilePath)
splitModuleFileName mid fn = case splitModuleIdentifiers mid of
  [_] -> splitFileName fn
  ms  -> let (base, ext) = splitExtension fn
             dirs        = splitDirectories base
             (pre , suf) = splitAt (length dirs - length ms) dirs
             path        = if null pre
                             then ""
                             else addTrailingPathSeparator (joinPath pre)
         in  (path, joinPath suf <.> ext)

--- Split up the components of a module identifier. For instance,
--- `splitModuleIdentifiers "Data.Set"` evaluates to `["Data", "Set"]`.
splitModuleIdentifiers :: ModuleIdent -> [String]
splitModuleIdentifiers = split (=='.')

--- Join the components of a module identifier. For instance,
--- `joinModuleIdentifiers ["Data", "Set"]` evaluates to `"Data.Set"`.
joinModuleIdentifiers :: [String] -> ModuleIdent
joinModuleIdentifiers = foldr1 combine
  where combine xs ys = xs ++ '.' : ys

--- Strips the suffix `.curry` or `.lcurry` from a file name.
stripCurrySuffix :: String -> String
stripCurrySuffix s =
  if takeExtension s `elem` [".curry",".lcurry"]
    then dropExtension s
    else s

--- A module path consists of a directory prefix (which can be omitted)
--- and a module name (which can be hierarchical). For instance, the
--- following strings are module paths in Unix-based systems:
---
---     HTML
---     Data.Number.Int
---     curry/Data.Number.Int
type ModulePath = String

--- Transforms a hierarchical module name into a path name, i.e.,
--- replace the dots in the name by directory separator chars.
modNameToPath :: ModuleIdent -> String
modNameToPath = foldr1 (</>) . split (=='.')

--- Name of the sub directory where auxiliary files (.fint, .fcy, etc)
--- are stored. Note that the name of this directory depends
--- on the compiler to avoid confusion when using different compilers.
--- For instance, when using PAKCS 3.2.0, `currySubdir` evaluates
--- to `".curry/pakcs-3.2.0"`.
currySubdir :: FilePath
currySubdir =
  ".curry" </> compilerName ++ "-" ++ fullVersion

--- Transforms a path to a module name into a file name
--- by adding the result of 'currySubDir' to the path and transforming
--- a hierarchical module name into a path.
--- For instance, when using PAKCS 3.2.0, `inCurrySubdir "mylib/Data.Char"`
--- evaluates to `"mylib/.curry/pakcs-3.2.0/Data/Char"`.
inCurrySubdir :: FilePath -> FilePath
inCurrySubdir filename =
  let (base,file) = splitFileName filename
   in base </> currySubdir </> modNameToPath file

--- Transforms a file name by adding the currySubDir to the file name.
--- This version respects hierarchical module names.
inCurrySubdirModule :: ModuleIdent -> FilePath -> FilePath
inCurrySubdirModule m fn = let (dirP, modP) = splitModuleFileName m fn
                           in  dirP </> currySubdir </> modP

--- Transforms a directory name into the name of the corresponding
--- sub directory containing auxiliary files.
addCurrySubdir :: FilePath -> FilePath
addCurrySubdir dir = dir </> currySubdir

------------------------------------------------------------------------------
--- Finding files in correspondence to compiler load path
------------------------------------------------------------------------------

--- Returns the current path (list of directory names) of the
--- system libraries.
sysLibPath :: [String]
sysLibPath = case compilerName of
  "pakcs" -> [installDir </> "lib"]
  "kics"  -> [installDir </> "src" </> "lib"]
  "kics2" -> [installDir </> "lib"]
  _       -> error "Distribution.sysLibPath: unknown compilerName"

--- Returns the current path (list of directory names) that is
--- used for loading modules w.r.t. a given module path.
--- The directory prefix of the module path (or "." if there is
--- no such prefix) is the first element of the load path and the
--- remaining elements are determined by the environment variable
--- CURRYRPATH and the entry "libraries" of the system's rc file.
getLoadPathForModule :: ModulePath -> IO [String]
getLoadPathForModule modpath = do
  rcfile <- curryrcFileName
  mblib  <- getPropertyFromFile rcfile "libraries"
  let fileDir = dropFileName modpath
  if compilerName `elem` ["pakcs","kics","kics2"] then
    do currypath <- getEnv "CURRYPATH"
       let llib = maybe [] (\l -> if null l then [] else splitSearchPath l)
                        mblib
       return $ (fileDir : (if null currypath
                            then []
                            else splitSearchPath currypath) ++
                           llib ++ sysLibPath)
    else error "Distribution.getLoadPathForModule: unknown compilerName"

--- Returns a directory name and the actual source file name for a module
--- by looking up the module source in the current load path.
--- If the module is hierarchical, the directory is the top directory
--- of the hierarchy.
--- Returns Nothing if there is no corresponding source file.
lookupModuleSourceInLoadPath :: ModulePath -> IO (Maybe (String,String))
lookupModuleSourceInLoadPath modpath = do
  loadpath <- getLoadPathForModule modpath
  lookupModuleSource loadpath modpath

--- Returns a directory name and the actual source file name for a module
--- by looking up the module source in the load path provided as the
--- first argument.
--- If the module is hierarchical, the directory is the top directory
--- of the hierarchy.
--- Returns Nothing if there is no corresponding source file.
lookupModuleSource :: [String] -> String -> IO (Maybe (String,String))
lookupModuleSource loadpath mod = lookupSourceInPath loadpath
 where
  fn       = takeFileName mod
  fnlcurry = modNameToPath fn ++ ".lcurry"
  fncurry  = modNameToPath fn ++ ".curry"

  lookupSourceInPath [] = return Nothing
  lookupSourceInPath (dir:dirs) = do
    lcurryExists <- doesFileExist (dir </> fnlcurry)
    if lcurryExists then return (Just (dir, dir </> fnlcurry)) else do
     curryExists <- doesFileExist (dir </> fncurry)
     if curryExists then return (Just (dir, dir </> fncurry))
                    else lookupSourceInPath dirs

------------------------------------------------------------------------------
--- The name of the file specifying resource configuration parameters of the
--- current distribution.
--- This file must have the usual format of property files.
curryrcFileName :: IO FilePath
curryrcFileName = getHomeDirectory >>= return . (</> rcFile)
  where rcFile = '.' : compilerName ++ "rc"

------------------------------------------------------------------------------
