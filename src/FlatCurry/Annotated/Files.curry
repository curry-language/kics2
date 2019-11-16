------------------------------------------------------------------------------
--- This library supports meta-programming, i.e., the manipulation of
--- Curry programs in Curry. This library defines I/O actions
--- to read Curry programs and transform them into this representation.
---
--- @author Michael Hanus
--- @version December 2018
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
import ReadShowTerm     (readUnqualifiedTerm, showTerm)
import FlatCurry.Annotated.Types

readTypedFlatCurry :: String -> IO (AProg TypeExpr)
readTypedFlatCurry progname =
   readTypedFlatCurryWithParseOptions progname (setQuiet True defaultParams)

readTypedFlatCurryWithParseOptions :: String -> FrontendParams -> IO (AProg TypeExpr)
readTypedFlatCurryWithParseOptions progname options = do
  mbsrc <- lookupModuleSourceInLoadPath progname
  case mbsrc of
    Nothing -> do -- no source file, try to find FlatCurry file in load path:
      loadpath <- getLoadPathForModule progname
      filename <- getFileWithSuffix
                    (typedFlatCurryFileName (takeFileName progname)) [""]
                    loadpath
      readTypedFlatCurryFile filename
    Just (dir,_) -> do
      callFrontendWithParams TFCY options progname
      readTypedFlatCurryFile (typedFlatCurryFileName (dir </> takeFileName progname))

typedFlatCurryFileName :: String -> String
typedFlatCurryFileName prog = inCurrySubdir (stripCurrySuffix prog) <.> "tfcy"

readTypedFlatCurryFile :: String -> IO (AProg TypeExpr)
readTypedFlatCurryFile filename = do
  filecontents <- readTypedFlatCurryFileRaw filename
  -- ...with generated Read class instances (slow!):
  --return (read filecontents)
  -- ...with built-in generic read operation (faster):
  return (readUnqualifiedTerm ["FlatCurry.Annotated.Types", "FlatCurry.Types",
                               "Prelude"]
                              filecontents)

readTypedFlatCurryFileRaw :: String -> IO String
readTypedFlatCurryFileRaw filename = do
  extfcy <- doesFileExist filename
  if extfcy
   then readFile filename
   else do let subdirfilename = inCurrySubdir filename
           exdirtfcy <- doesFileExist subdirfilename
           if exdirtfcy
            then readFile subdirfilename
            else error ("EXISTENCE ERROR: Typed FlatCurry file '" ++ filename ++
                        "' does not exist")
