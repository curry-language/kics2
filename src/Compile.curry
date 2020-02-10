--- --------------------------------------------------------------------------
--- The main module for KiCS2c Curry to Haskell compiler
---
--- @author  Bernd Brassel, Michael Hanus, Bjoern Peemoeller, Fabian Reck
--- @version December 2018
--- --------------------------------------------------------------------------
module Compile where

import ReadShowTerm                (readQTermFile)
import Data.Char                   (isSpace)
import Data.Maybe                  (fromJust)
import Data.List                   (intercalate, isPrefixOf)
import Data.Map                    (union)
import Control.Monad               (when, foldM)
import System.Directory            (doesFileExist)
import System.FilePath             (FilePath, (</>), dropExtension, normalise)
import System.IOExts               (readCompleteFile)
import System.Environment          (getArgs)

import System.CurryPath            ( getLoadPathForModule, stripCurrySuffix
                                   , lookupModuleSourceInLoadPath
                                   )
import FlatCurry.Types
import FlatCurry.Goodies           (updQNamesInProg)
import FlatCurry.Annotated.Types
import FlatCurry.Annotated.Files   (typedFlatCurryFileName)
import FlatCurry.Annotated.Goodies (unAnnProg)

import qualified AbstractHaskell.Types   as AH
import qualified AbstractHaskell.Goodies as AHG (funcName, renameSymbolInProg, typeOf)
import qualified AbstractHaskell.Printer as AHP
import Analysis               (AnalysisResult, showAnalysisResult, readAnalysisResult)
import CompilerOpts
import RCFile
import Files                     ( withBaseName, withDirectory, withExtension
                                 , writeFileInDir, writeQTermFileInDir
                                 , lookupFileInPath
                                 )
import LiftCase                  (liftCases)
import EliminateCond             (eliminateCond)
import DefaultPolymorphic        (defaultPolymorphic)
import MissingImports            (fixMissingImports)
import Message                   (putErrLn, showStatus, showDetail)
import ModuleDeps                (ModuleIdent, Source, deps)
import Names
import SimpleMake
import TransFunctions
import TransTypes
import Utils                     (notNull, lpad, rpad)

--- Parse the command-line arguments and build the specified modules.
main :: IO ()
main = do
  rcFileDefs      <- readRC
  args            <- getArgs
  (opts, modules) <- getCompilerOpts
  mapM_ (build opts { rcVars = updateRCDefs rcFileDefs
                                             (snd (extractRCArgs args))
                    , optMainVerbosity = optVerbosity opts
                    })
        modules

--- Load the module, resolve the dependencies and compile the source files
--- if necessary.
build :: Options -> String -> IO ()
build opts mn = do
  mbMn <- locateCurryFile mn
  case mbMn of
    Nothing -> putErrLn $ "Could not find module " ++ mn
    Just f -> do
      (mods, errs) <- deps opts mn f
      if null errs
        then foldM (makeModule mods) initState (zip mods [1 .. ]) >> return ()
        else mapM_ putErrLn errs
 where initState = defaultState { compOptions = opts }


--- Checks if the given string corresponds to a Curry module and
--- returns the actual file path
--- @param mn - the (relative) path to the Curry module with or without extension
--- @return `Just path` if the module was found, `Nothing` if not
locateCurryFile :: String -> IO (Maybe FilePath)
locateCurryFile mn = do
  exists <- doesFileExist mn
  if exists
    then return (Just mn)
    else let modname = stripCurrySuffix mn
             tfcyname = typedFlatCurryFileName modname
          in lookupModuleSourceInLoadPath modname >>=
             maybe (-- try to find a FlatCurry file without source
                    getLoadPathForModule modname >>=
                    lookupFileInPath tfcyname [""] )
                   (\ (_,fn) -> return (Just fn))

makeModule :: [(ModuleIdent, Source)] -> State -> ((ModuleIdent, Source), Int)
           -> IO State
makeModule mods state mod@((mid, (fn, imps, _)), _)
  | optForce opts = compileModule modCnt state mod
  | otherwise     = do
                    depFiles <- getDepFiles
                    smake (destFile (optTraceFailure opts)
                                    (optOutputSubdir opts) mid fn)
                          depFiles
                          (compileModule modCnt state mod)
                          (loadAnalysis modCnt state mod)
  where
    getDepFiles = do
      hasExternals <- doesFileExist extFile
      let ownModule = fn : [extFile | hasExternals]
      let imported  = map (\i -> destFile (optTraceFailure opts)
                                          (optOutputSubdir opts)
                                          i
                               $ fst3 $ fromJust $ lookup i mods) imps
      return $ ownModule ++ imported
    extFile = externalFile fn
    modCnt = length mods
    opts = compOptions state

writeAnalysis :: Options -> ModuleIdent -> FilePath -> AnalysisResult -> IO ()
writeAnalysis opts mid fn analysis = do
  showDetail opts $ "Writing Analysis file " ++ ndaFile
  writeQTermFileInDir ndaFile (showAnalysisResult analysis)
    where ndaFile = analysisFile (optOutputSubdir opts) mid fn

readAnalysis :: Options -> ModuleIdent -> FilePath -> IO AnalysisResult
readAnalysis opts mid fn = do
  showDetail opts $ "Reading Analysis file " ++ ndaFile
  readAnalysisResult <$> readQTermFile ndaFile
    where ndaFile = analysisFile (optOutputSubdir opts) mid fn

loadAnalysis :: Int -> State -> ((ModuleIdent, Source), Int) -> IO State
loadAnalysis total state ((mid, (fn, _, _)), current) = do
  showStatus opts $ compMessage (current, total) "Analyzing" mid (fn, ndaFile)
  (types, ndAna, hoType, hoCons, hoFunc) <- readAnalysis opts mid fn
  return state { typeMap      = typeMap state      `union` types
               , ndResult     = ndResult state     `union` ndAna
               , hoResultType = hoResultType state `union` hoType
               , hoResultCons = hoResultCons state `union` hoCons
               , hoResultFunc = hoResultFunc state `union` hoFunc
               }
    where
      ndaFile = analysisFile (optOutputSubdir opts) mid fn
      opts = compOptions state

compileModule :: Int -> State -> ((ModuleIdent, Source), Int) -> IO State
compileModule total state ((mid, (fn, _, tfcyFileName)), current) = do
  rawTfcy <- readCompleteFile tfcyFileName
  showStatus opts $ compMessage (current, total) "Compiling" mid (fn, dest)

  let tfcy = filterPrelude opts (read rawTfcy)
  dump DumpTypedFlat opts typedName (show tfcy)

  showDetail opts "Lifting case expressions"
  let pLifted = liftCases True tfcy
  dump DumpLifted opts liftedName (show pLifted)

  showDetail opts "Eliminate calls to cond"
  let pElim = eliminateCond pLifted
  dump DumpEliminated opts elimName (show pElim)

  showDetail opts "Default locally polymorphic sub-expressions"
  let pDefaulted = defaultPolymorphic pElim
  dump DumpDefaulted opts defaultedName (show pDefaulted)

  showDetail opts "Extending imports"
  let pExtImports = fixMissingImports pDefaulted
  dump DumpExtImports opts extImportsName (show pExtImports)

  showDetail opts "Renaming symbols"
  let renamed@(Prog _ _ ts _ _)  = rename (unAnnProg pExtImports)
  dump DumpRenamed opts renamedName (show renamed)

  showDetail opts "Transforming functions"
  transFuncs <- runIOES (trProg renamed) state
  let ((ahsFun@(AH.Prog n imps _ funs ops), modAnalysisResult), state')
        = either error id transFuncs
  writeAnalysis (compOptions state') mid fn modAnalysisResult
  dump DumpFunDecls opts funDeclName (show ahsFun)

  showDetail opts "Transforming type declarations"
  let typeDecls = transTypes (hoResultCons state') ts
  dump DumpTypeDecls opts typeDeclName (show typeDecls)

  showDetail opts "Combining to Abstract Haskell"
  let ahs = (AH.Prog n (defaultModules ++ imps) typeDecls funs ops)

  -- TODO: HACK: manually patch export of type class curry into Prelude
  let ahsPatched = patchPreludeExports ahs
  dump DumpTranslated opts abstractHsName (show ahsPatched)

  showDetail opts "Integrating external declarations"
  integrated <- integrateExternals opts ahsPatched fn

  showDetail opts $ "Generating Haskell module " ++ dest
  writeFileInDir dest integrated

  showDetail opts $ "Writing auxiliary info file " ++ funcInfo
  writeQTermFileInDir funcInfo (extractFuncInfos funs)

  showDetail opts $ "Done"
  return state'

    where
    typedName      = tfcyFile $ withBaseName (++ "Typed"     ) mid
    extImportsName = tfcyFile $ withBaseName (++ "ExtImports") mid
    liftedName     = tfcyFile $ withBaseName (++ "Lifted"    ) mid
    elimName       = tfcyFile $ withBaseName (++ "ElimCond"  ) mid
    defaultedName  = tfcyFile $ withBaseName (++ "Defaulted" ) mid
    renamedName    = fcyFile $ withBaseName (++ "Renamed"   ) mid
    funDeclName    = ahsFile $ withBaseName (++ "FunDecls"  ) mid
    typeDeclName   = ahsFile $ withBaseName (++ "TypeDecls" ) mid
    abstractHsName = ahsFile mid
    dest           = destFile (optTraceFailure opts) (optOutputSubdir opts) mid fn
    funcInfo       = funcInfoFile (optOutputSubdir opts) mid fn
    opts           = compOptions state
    fcyFile f      = withExtension (const ".fcy") f
    tfcyFile f     = withExtension (const ".tfcy") f
    ahsFile f      = withExtension (const ".ahs") f

-- Extract some basic information (deterministic, IO) about all functions
extractFuncInfos :: [AH.FuncDecl] -> [(AH.QName, Bool)]
extractFuncInfos funs =
  map (\fd -> (AHG.funcName fd, isIO (AHG.typeOf fd))) funs
 where
  isIO AH.Untyped      = False
  isIO (AH.CType _ ty) = withIOResult ty

  withIOResult (AH.TVar            _) = False
  withIOResult (AH.FuncType     _ ty) = withIOResult ty
  withIOResult (AH.TCons        tc _) = tc == (curryPrelude, "C_IO")
  withIOResult (AH.ForallType _ _ ty) = withIOResult ty

-- Patch Prelude in order to add some exports for predefined items
patchPreludeExports :: AH.Prog -> AH.Prog
patchPreludeExports p@(AH.Prog m imps td fd od)
  | m == curryPrelude = AH.Prog m imps (applyDecl:curryDecl:td)
                                (toCurryString:fd) od
  | otherwise         = p
 where
  applyDecl     = AH.Type (curryPrelude, "C_Apply") AH.Public [] []
  curryDecl     = AH.Type (curryPrelude, "Curry") AH.Public [] []
  toCurryString = AH.Func "" (curryPrelude, "toCurryString") 1 AH.Public
                          AH.Untyped AH.External

compMessage :: (Int, Int) -> String -> String -> (FilePath, FilePath) -> String
compMessage (curNum, maxNum) what m (src, dst)
  =  '[' : lpad (length sMaxNum) (show curNum) ++ " of " ++ sMaxNum  ++ "]"
  ++ ' ' : rpad 9 what ++ ' ' : rpad 16 m
  ++ " ( " ++ normalise src ++ ", " ++ normalise dst ++ " )"
  where sMaxNum  = show maxNum

filterPrelude :: Options -> AProg a -> AProg a
filterPrelude opts p@(AProg m imps td fd od)
  | noPrelude = AProg m (filter (/= prelude) imps) td fd od
  | otherwise = p
  where noPrelude = NoImplicitPrelude `elem` optExtensions opts

--
integrateExternals :: Options -> AH.Prog -> FilePath -> IO String
integrateExternals opts (AH.Prog m is td fd od) fn = do
  exts <- lookupExternals opts (dropExtension fn)
  let (pragmas, extimps, extdecls) = splitExternals exts
  return $ intercalate "\n" $ filter notNull
    [ unlines (defaultPragmas ++ pragmas)
    , AHP.pPrint (AHP.ppHeader ppOpts m td fd)
    , "\n" ++ AHP.pPrint (AHP.ppImports ppOpts is)
    , "\n" ++ unlines extimps
    , AHP.pPrint (AHP.ppDecls ppOpts od td fd)
    , "\n" ++ unlines extdecls
    ]
 where
  defaultPragmas = [ "{-# LANGUAGE MagicHash #-}"
                   , "{-# LANGUAGE ScopedTypeVariables #-}"
                   , "{-# LANGUAGE Rank2Types #-}"
                   , "{-# LANGUAGE CPP #-}"
                   , "{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}"
                   , "{-# OPTIONS_GHC -fno-warn-missing-methods #-}"
                   , "#if __GLASGOW_HASKELL__ >= 800"
                   , "{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}"
                   , "#else"
                   , "{-# OPTIONS_GHC -fno-warn-duplicate-constraints #-}"
                   , "#endif"
                   ]
  ppOpts = AHP.defaultOptions { AHP.traceFailure  = optTraceFailure opts
                              , AHP.currentModule = m
                              , AHP.qualImpModule = isCurryModule }

-- lookup an external file for a module and return either the content or an
-- empty String
lookupExternals :: Options -> FilePath -> IO String
lookupExternals opts fn = do
  exists <- doesFileExist extName
  if exists
    then showDetail opts    "External file found" >> readCompleteFile extName
    else showDetail opts "No external file found" >> return ""
    where extName = externalFile fn

-- Split an external file into a pragma String, a list of imports and the rest
-- TODO: This is a bloody hack
splitExternals :: String -> ([String], [String], [String])
splitExternals content = ( dropTrailingSpaces pragmas
                         , dropTrailingSpaces imports
                         , decls)
  where
  dropTrailingSpaces = reverse . dropWhile (all isSpace) . reverse
  (pragmas, rest ) = span isPragma (lines content)
  (imports, decls) = span isImport rest
  isPragma line    = all isSpace line || "{-#"    `isPrefixOf` line
  isImport line    = all isSpace line || "import" `isPrefixOf` line
                                      || "#"      `isPrefixOf` line
                                      || isComment line
  isComment line   = "-- " `isPrefixOf` line
                     && not ("-- #endimport" `isPrefixOf` line)

--- Dump an intermediate result to a file
dump :: DumpFormat -> Options -> FilePath -> String -> IO ()
dump format opts file src = when (format `elem` optDump opts) $ do
  showDetail opts $ "Dumping " ++ file
  writeFileInDir (withDirectory (</> optOutputSubdir opts) file) src

rename :: Prog -> Prog
rename p@(Prog name imports _ _ _) =
  Prog (renameModule name) (map renameModule imports) td fd od where
  (Prog _ _ td fd od) = updQNamesInProg renameQName p

defaultModules :: [String]
defaultModules = [basics]

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x
