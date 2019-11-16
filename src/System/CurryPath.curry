------------------------------------------------------------------------------
--- This module contains operations related to module names and paths
--- used in Curry system.
---
--- @author Bernd Brassel, Michael Hanus, Bjoern Peemoeller, Finn Teegen
--- @version December 2018
------------------------------------------------------------------------------

module System.CurryPath
  ( ModuleIdent
  , splitModuleFileName, splitModuleIdentifiers  , joinModuleIdentifiers
  , stripCurrySuffix
  , ModulePath, modNameToPath
  , currySubdir, inCurrySubdir, inCurrySubdirModule, addCurrySubdir
  , sysLibPath, getLoadPathForModule
  , lookupModuleSourceInLoadPath, lookupModuleSource
  ) where

import Data.Char           ( toLower )
import Data.List           ( split )
import System.Directory    ( doesFileExist )
import System.Environment  ( getEnv )
import System.Process      ( system )
import System.FilePath     ( FilePath, (</>), (<.>), addTrailingPathSeparator
                           , dropFileName, joinPath, splitDirectories
                           , splitExtension, splitFileName, splitSearchPath
                           , takeFileName, takeExtension, dropExtension
                           )
import Language.Curry.Distribution ( curryCompiler, installDir, rcFileName )

import Data.PropertyFile   ( getPropertyFromFile )

-----------------------------------------------------------
--- Functions for handling file names of Curry modules
-----------------------------------------------------------

type ModuleIdent = String

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
             path        = if null pre then ""
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

--- Strips the suffix ".curry" or ".lcurry" from a file name.
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
--- are stored.
currySubdir :: FilePath
currySubdir = ".curry"

--- Transforms a path to a module name into a file name
--- by adding the `currySubDir` to the path and transforming
--- a hierarchical module name into a path.
--- For instance, `inCurrySubdir "mylib/Data.Char"` evaluates to
--- `"mylib/.curry/Data/Char"`.
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

-----------------------------------------------------------
--- finding files in correspondence to compiler load path
-----------------------------------------------------------

--- Returns the current path (list of directory names) of the
--- system libraries.
sysLibPath :: [String]
sysLibPath = case curryCompiler of
  "pakcs" -> [installDir </> "lib"]
  "kics"  -> [installDir </> "src" </> "lib"]
  "kics2" -> [installDir </> "lib"]
  _       -> error "Distribution.sysLibPath: unknown curryCompiler"

--- Returns the current path (list of directory names) that is
--- used for loading modules w.r.t. a given module path.
--- The directory prefix of the module path (or "." if there is
--- no such prefix) is the first element of the load path and the
--- remaining elements are determined by the environment variable
--- CURRYRPATH and the entry "libraries" of the system's rc file.
getLoadPathForModule :: ModulePath -> IO [String]
getLoadPathForModule modpath = do
  rcfile <- rcFileName
  mblib  <- getPropertyFromFile rcfile "libraries"
  let fileDir = dropFileName modpath
  if curryCompiler `elem` ["pakcs","kics","kics2"] then
    do currypath <- getEnv "CURRYPATH"
       let llib = maybe [] (\l -> if null l then [] else splitSearchPath l)
                        mblib
       return $ (fileDir : (if null currypath
                            then []
                            else splitSearchPath currypath) ++
                           llib ++ sysLibPath)
    else error "Distribution.getLoadPathForModule: unknown curryCompiler"

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
