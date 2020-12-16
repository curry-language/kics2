------------------------------------------------------------------------------
--- This library defines I/O actions to read and write
--- type-annotated FlatCurry programs.
---
--- @author Michael Hanus
--- @version July 2020
------------------------------------------------------------------------------

module FlatCurry.Annotated.Files where

import System.FilePath  (takeFileName, (</>), (<.>))
import System.Directory (doesFileExist, getFileWithSuffix)
import System.FrontendExec
                        ( FrontendParams, FrontendTarget (..), defaultParams
                        , setQuiet
                        , callFrontend, callFrontendWithParams)
import System.CurryPath ( lookupModuleSourceInLoadPath, getLoadPathForModule
                        , inCurrySubdir, stripCurrySuffix
                        )
import Data.Maybe       (isNothing)
import ReadShowTerm     (readUnqualifiedTerm, showTerm) -- for faster reading
import FlatCurry.Annotated.Types

--- Transforms a name of a Curry program (with or without suffix ".curry"
--- or ".lcurry") into the name of the file containing the
--- corresponding type-annotated FlatCurry program.
annotatedFlatCurryFileName :: String -> String
annotatedFlatCurryFileName prog = inCurrySubdir (stripCurrySuffix prog) <.> "tafcy"

--- Gets the standard type-annotated FlatCurry file location
--- for a given Curry module name.
--- The Curry source program must exist in the Curry load path,
--- otherwise an error is raised.
annotatedFlatCurryFilePath :: String -> IO String
annotatedFlatCurryFilePath mname = do
  mbsrc <- lookupModuleSourceInLoadPath mname
  case mbsrc of
    Nothing      -> error $ "Curry source file for module '" ++ mname ++
                            "' not found!"
    Just (dir,_) -> return (annotatedFlatCurryFileName (dir </> mname))

--- I/O action which parses a Curry program and returns the corresponding
--- type-annotated FlatCurry program.
--- The argument is the module path (without suffix ".curry"
--- or ".lcurry") and the result is a type-annotated FlatCurry term
--- representing this program.
readAnnotatedFlatCurry :: String -> IO (AProg TypeExpr)
readAnnotatedFlatCurry progname =
   readAnnotatedFlatCurryWithParseOptions progname (setQuiet True defaultParams)

--- I/O action which parses a Curry program
--- with respect to some parser options and returns the
--- corresponding FlatCurry program.
--- This I/O action is used by `readAnnotatedFlatCurry`.
--- @param progfile - the program file name (without suffix ".curry")
--- @param options - parameters passed to the front end
readAnnotatedFlatCurryWithParseOptions :: String -> FrontendParams
                                   -> IO (AProg TypeExpr)
readAnnotatedFlatCurryWithParseOptions progname options = do
  mbsrc <- lookupModuleSourceInLoadPath progname
  case mbsrc of
    Nothing -> do -- no source file, try to find FlatCurry file in load path:
      loadpath <- getLoadPathForModule progname
      filename <- getFileWithSuffix
                    (annotatedFlatCurryFileName (takeFileName progname)) [""]
                    loadpath
      readAnnotatedFlatCurryFile filename
    Just (dir,_) -> do
      callFrontendWithParams TAFCY options progname
      readAnnotatedFlatCurryFile (annotatedFlatCurryFileName (dir </> takeFileName progname))

--- Reads a type-annotated FlatCurry program from a file in `.tafcy` format
--- where the file name is provided as the argument.
readAnnotatedFlatCurryFile :: String -> IO (AProg TypeExpr)
readAnnotatedFlatCurryFile filename = do
  filecontents <- readAnnotatedFlatCurryFileRaw filename
  -- ...with generated Read class instances (slow!):
  --return (read filecontents)
  -- ...with built-in generic read operation (faster):
  return (readUnqualifiedTerm ["FlatCurry.Annotated.Types", "FlatCurry.Types",
                               "Prelude"]
                              filecontents)
 where
  readAnnotatedFlatCurryFileRaw fname = do
    extfcy <- doesFileExist fname
    if extfcy
      then readFile fname
      else do
        let subdirfilename = inCurrySubdir fname
        exdirtfcy <- doesFileExist subdirfilename
        if exdirtfcy
          then readFile subdirfilename
          else error $ "EXISTENCE ERROR: Typed FlatCurry file '" ++
                       fname ++ "' does not exist"

--- Writes a type-annotated FlatCurry program into a file in `.tafcy` format.
--- The file is written in the standard location for intermediate files,
--- i.e., in the 'annotatedFlatCurryFileName' relative to the directory of the
--- Curry source program (which must exist!).
writeAnnotatedFlatCurry :: AProg TypeExpr -> IO ()
writeAnnotatedFlatCurry prog@(AProg mname _ _ _ _) = do
  fname <- annotatedFlatCurryFilePath mname
  writeAnnotatedFlatCurryFile fname prog

--- Writes a type-annotated FlatCurry program into a file in ".tafcy" format.
--- The first argument must be the name of the target file
--- (with suffix `.fcy`).
writeAnnotatedFlatCurryFile :: String -> AProg TypeExpr -> IO ()
writeAnnotatedFlatCurryFile file prog = writeFile file (showTerm prog)

