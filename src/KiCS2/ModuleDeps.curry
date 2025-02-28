--- --------------------------------------------------------------------------
--- Computation of dependendies between Curry modules.
---
--- This module implements the functions to compute the dependency
--- information between Curry modules.
---
--- @author  Björn Peemöller, Fabian Skrlac, Finn Teegen, Jan Tikovsky
--- @version December 2018
--- --------------------------------------------------------------------------
module KiCS2.ModuleDeps (ModuleIdent, Source, Errors, deps, updatePreludeImport) where

import Data.Char                   ( isSpace, toUpper )
import Data.Map                    ( Map, empty, insert, toList, lookup )
import Data.SCC                    ( scc )
import Data.List                   ( intercalate, partition )
import Data.Maybe                  ( fromJust, isJust, isNothing )
import Control.Monad               ( foldM, unless )
import System.IO                   ( Handle, IOMode(ReadMode), hClose
                                   , hGetChar, hIsEOF, openFile
                                   )
import System.Process              ( system )
import System.FilePath             ( FilePath, dropExtension, takeExtension
                                   , takeBaseName, dropTrailingPathSeparator
                                   , (</>), (<.>), normalise
                                   )
import System.Directory            ( doesFileExist, getModificationTime
                                   , findFileWithSuffix, getFileWithSuffix
                                   )
import KiCS2.System.CurryPath      ( currySubdir )
import KiCS2.System.FrontendExec   ( defaultParams, setDefinitions, setOutDir
                                   , setFullPath, setQuiet, setFrontendPath
                                   , setSpecials, callFrontendWithParams
                                   , FrontendTarget(..), FrontendParams )

import FlatCurry.Annotated.Types
import KiCS2.FlatCurry.Annotated.Files ( annotatedFlatCurryFileName )

import KiCS2.CompilerOpts
import KiCS2.System.CurryPath      ( inCurrySubdirModule, stripCurrySuffix )
import KiCS2.InstallationPaths     ( kics2HomeDir )
import KiCS2.Message               ( showStatus,showAnalysis )
import KiCS2.Names                 ( moduleNameToPath, prelude )
import KiCS2.RCFile                ( rcValue )
import Installation                ( compilerName
                                   , majorVersion, minorVersion, fullVersion
                                   )

type ModuleIdent = String
type Errors      = [String]

 -- file name, imports, TAFCY file name
type Source      = (FilePath, [ModuleIdent], FilePath)
type SourceEnv   = Map ModuleIdent (Maybe Source)


--- Compute all dependendies for a given module
--- @param opts - compiler options
--- @param mn   - module name
--- @param fn   - file name of the module (or FlatCurry file name)
--- @return     - topologically sorted list of all dependendies
---               and a list of errors (empty in case of success)
deps :: Options -> ModuleIdent -> FilePath
     -> IO ([(ModuleIdent, Source)], Errors)
deps opts mn fn = do
  mEnv <- sourceDeps opts mn fn Data.Map.empty
  let (mods1, errs1) = filterMissing mEnv -- handle missing modules
      (mods2, errs2) = flattenDeps mods1  -- check for cyclic imports
                                          -- and sort topologically
  errs3 <- checkAnnotatedFlatCurry mn fn
  return (mods2, concat [errs1, errs2, errs3])

-- Has the given program name a valid type-annotated FlatCurry file?
-- Used to check the result of the front end compilation process.
checkAnnotatedFlatCurry :: ModuleIdent -> String -> IO Errors
checkAnnotatedFlatCurry mname fname
  | isAnnotatedFlatCurryFile fname = return []
  | otherwise             = do
    let afcyname = stripCurrySuffix (inCurrySubdirModule mname fname) <.> "afcy"
    existcy  <- doesFileExist fname
    existafcy <- doesFileExist afcyname
    if existcy && existafcy
      then do cymtime  <- getModificationTime fname
              afcymtime <- getModificationTime afcyname
              return [ "type-annotated FlatCurry file " ++ afcyname ++
                       " is older than Curry file " ++ fname
                     | afcymtime < cymtime ]
      else return $    [ "Missing Curry file " ++ fname | not existcy ]
                    ++ [ "Missing type-annotated FlatCurry file " ++ afcyname
                       | not existafcy ]

moduleDeps :: Options -> SourceEnv -> ModuleIdent -> IO SourceEnv
moduleDeps opts mEnv m = case Data.Map.lookup m mEnv of
  Just _  -> return mEnv
  Nothing -> do
    mbFile <- lookupModule opts m
    case mbFile of
      Nothing -> return $ insert m Nothing mEnv
      Just fn -> sourceDeps opts { optVerbosity = VerbQuiet } m fn mEnv

lookupModule :: Options -> String -> IO (Maybe FilePath)
lookupModule opts m = findFileWithSuffix (moduleNameToPath m)
                      [".curry", ".lcurry", ".afcy"]
                      (map dropTrailingPathSeparator importPaths)
  where importPaths = "." : optImportPaths opts

sourceDeps :: Options -> ModuleIdent -> String -> SourceEnv -> IO SourceEnv
sourceDeps opts mn fn mEnv = do
  afcyName <- getAfcyFileName opts mn fn
  rawAfcyHeader <- readAfcyModuleHeader afcyName
  let (m, is) = readModuleNameAndImports rawAfcyHeader
      is'     = updatePreludeImport opts is
  foldM (moduleDeps opts) (insert m (Just (fn, is', afcyName)) mEnv) is'

-- Remove Prelude import if NoImplicitPrelude is set, otherwise
-- ensure that it is present.
updatePreludeImport :: Options -> [String] -> [String]
updatePreludeImport opts imps
  | noPrelude = filter (/= prelude) imps
  | otherwise = if hasPrelude then imps else prelude : imps
  where noPrelude = NoImplicitPrelude `elem` optExtensions opts
        hasPrelude = prelude `elem` imps

-- Reads only module name and imports from a type-annotated FlatCurry representation.
readModuleNameAndImports :: String -> (ModuleIdent, [ModuleIdent])
readModuleNameAndImports s = (m, is)
  where ((m , s'):_) = reads s
        ((is, _ ):_) = reads s'

-- Reads chars using the given handler until the given predicate satisfies on a
-- read char. The handler is immediately closed if eof is reached.
-- Attention: This function consumes (and returns) the last character, that
-- satisfies the predicate, i.e. it consumes at least one character!
hReadUntil  :: Handle -> (Char -> Bool) -> IO String
hReadUntil h isBreakChar = do
  eof <- hIsEOF h
  if eof then hClose h >> return ""
         else do c <- hGetChar h
                 if isBreakChar c then return [c]
                                  else do cs <- hReadUntil h isBreakChar
                                          return (c:cs)

-- Get module name and imports from the given type-annotated FlatCurry file.
readAfcyModuleHeader :: FilePath -> IO String
readAfcyModuleHeader afcyFile = do
  h <- openFile afcyFile ReadMode
  hReadUntil h $ not . isSpace -- leading spaces
  hReadUntil h isSpace          -- AProg
  hReadUntil h $ not . isSpace -- spaces
  modId  <- hReadUntil h $ isSpace
  hReadUntil h (== '[')          -- spaces
  modIds <- hReadUntil h (== ']')
  hClose h
  return $ '"' : modId ++ "[" ++ modIds

-- Get a type-annotated FlatCurry file name. Parses a Curry module and creates the
-- type-annotated FlatCurry file if it does not exist.
-- TODO: This should better return `Either Errors Prog` so that compilation
-- errors can be recognized.
getAfcyFileName :: Options -> ModuleIdent -> FilePath -> IO FilePath
getAfcyFileName opts mn fn
  | isAnnotatedFlatCurryFile fn
  = do showStatus opts $ "Reading directly from type-annotated FlatCurry file '"++fn++"'"
       return fn
  | otherwise
  = do defps <- defaultParams
       k2home <- kics2HomeDir
       afcyname <- parseCurryWithOptions opts (stripCurrySuffix mn)
                   $ setDefinitions  [(compiler, version)]
                   $ setFullPath     importPaths
                   $ setQuiet        (optVerbosity opts == VerbQuiet)
                   $ setSpecials     (optParser opts)
                   $ setOutDir       currySubdir
                   $ setFrontendPath (k2home </> "bin" </> "kics2-frontend")
                   defps
       return afcyname
  where importPaths = "." : optImportPaths opts
        compiler    = "__" ++ map toUpper compilerName ++ "__"
        version     = majorVersion * 100 + minorVersion

-- Parse a Curry program with the front end and return the type-annotated FlatCurry
-- file name.
parseCurryWithOptions :: Options -> ModuleIdent -> FrontendParams -> IO String
parseCurryWithOptions opts modname options = do
  mbCurryFile  <- lookupModule opts modname
  unless (isNothing mbCurryFile) $
    callFrontendWithParams TAFCY options modname
  normalise <$> getFileWithSuffix (annotatedFlatCurryFileName modname)
                  [""]
                  (map dropTrailingPathSeparator importPaths)
    where importPaths = "." : optImportPaths opts

isAnnotatedFlatCurryFile :: FilePath -> Bool
isAnnotatedFlatCurryFile fn = takeExtension fn == ".afcy"

filterMissing :: SourceEnv -> ([(ModuleIdent, Source)], Errors)
filterMissing env = (map (\(a, b) -> (a, fromJust b)) present, errs) where
  errs = map (\(m, _) -> "Module " ++ m ++ " could not be found") missing
  (present, missing) = partition (isJust . snd) $ toList env

--- Convert the dependency map into a topologically sorted dependency list
--- and a list of errors for cyclic imports.
flattenDeps :: [(ModuleIdent, Source)] -> ([(ModuleIdent, Source)], Errors)
flattenDeps = fdeps . sortDeps where

  sortDeps :: [(ModuleIdent, Source)] -> [[(ModuleIdent, Source)]]
  sortDeps = scc modules imports where
    -- extract the module ident
    modules (m, _) = [m]
    -- extract the imports
    imports (_, (_, imps, _)) = imps

  fdeps :: [[(ModuleIdent, Source)]] -> ([(ModuleIdent, Source)], Errors)
  fdeps = foldr checkdep ([], [])

  checkdep []          (ms', errs) = (ms'  , errs)
  checkdep [m]         (ms', errs) = (m:ms', errs)
  checkdep dep@(_:_:_) (ms', errs) = (ms'  , cyclicError (map fst dep) : errs)

  cyclicError :: [ModuleIdent] -> String
  cyclicError ms = "Cylic import dependency between modules " ++
                   intercalate ", " inits ++ " and " ++ last where
    (inits, last)      = splitLast ms
    splitLast []       = error "ModuleDeps.splitLast: empty list"
    splitLast (x:[])   = ([]  , x)
    splitLast (x:y:ys) = (x:xs, z) where (xs, z) = splitLast (y:ys)
