------------------------------------------------------------------------------
--- This library supports meta-programming, i.e., the manipulation of
--- Curry programs in Curry. This library defines I/O actions
--- to read Curry programs and transform them into this representation.
---
--- @author Michael Hanus, Finn Teegen
--- @version December 2018
------------------------------------------------------------------------------

module FlatCurry.Files where

import System.Directory    ( doesFileExist, getFileWithSuffix
                           , findFileWithSuffix )
import System.FilePath     ( takeFileName, (</>), (<.>))
import System.CurryPath    ( inCurrySubdir, stripCurrySuffix
                           , lookupModuleSourceInLoadPath, getLoadPathForModule
                           )
import System.FrontendExec ( FrontendParams, FrontendTarget (..), defaultParams
                           , setQuiet, callFrontendWithParams
                           )
import ReadShowTerm        (readUnqualifiedTerm, showTerm)


import FlatCurry.Types

--- I/O action which parses a Curry program and returns the corresponding
--- FlatCurry program.
--- Thus, the argument is the module path (without suffix ".curry"
--- or ".lcurry") and the result is a FlatCurry term representing this
--- program.
readFlatCurry :: String -> IO Prog
readFlatCurry progname =
   readFlatCurryWithParseOptions progname (setQuiet True defaultParams)

--- I/O action which parses a Curry program
--- with respect to some parser options and returns the
--- corresponding FlatCurry program.
--- This I/O action is used by the standard action `readFlatCurry`.
--- @param progfile - the program file name (without suffix ".curry")
--- @param options - parameters passed to the front end
readFlatCurryWithParseOptions :: String -> FrontendParams -> IO Prog
readFlatCurryWithParseOptions progname options = do
  mbsrc <- lookupModuleSourceInLoadPath progname
  case mbsrc of
    Nothing -> do -- no source file, try to find FlatCurry file in load path:
      loadpath <- getLoadPathForModule progname
      filename <- getFileWithSuffix 
                     (flatCurryFileName (takeFileName progname)) [""]
                     loadpath
      readFlatCurryFile filename
    Just (dir,_) -> do
      callFrontendWithParams FCY options progname
      readFlatCurryFile (flatCurryFileName (dir </> takeFileName progname))

--- Transforms a name of a Curry program (with or without suffix ".curry"
--- or ".lcurry") into the name of the file containing the
--- corresponding FlatCurry program.
flatCurryFileName :: String -> String
flatCurryFileName prog = inCurrySubdir (stripCurrySuffix prog) <.> "fcy"

--- Transforms a name of a Curry program (with or without suffix ".curry"
--- or ".lcurry") into the name of the file containing the
--- corresponding FlatCurry program.
flatCurryIntName :: String -> String
flatCurryIntName prog = inCurrySubdir (stripCurrySuffix prog) <.> "fint"

--- I/O action which reads a FlatCurry program from a file in ".fcy" format.
--- In contrast to `readFlatCurry`, this action does not parse
--- a source program. Thus, the argument must be the name of an existing
--- file (with suffix ".fcy") containing a FlatCurry program in ".fcy"
--- format and the result is a FlatCurry term representing this program.
readFlatCurryFile :: String -> IO Prog
readFlatCurryFile filename = do
  exfcy <- doesFileExist filename
  if exfcy
   then readExistingFCY filename
   else do let subdirfilename = inCurrySubdir filename
           exdirfcy <- doesFileExist subdirfilename
           if exdirfcy
            then readExistingFCY subdirfilename
            else error ("EXISTENCE ERROR: FlatCurry file '" ++ filename ++
                        "' does not exist")
 where
   readExistingFCY fname = do
     filecontents <- readFile fname
     return (readUnqualifiedTerm ["FlatCurry.Types","Prelude"] filecontents)

--- I/O action which returns the interface of a Curry module, i.e.,
--- a FlatCurry program containing only "Public" entities and function
--- definitions without rules (i.e., external functions).
--- The argument is the file name without suffix ".curry"
--- (or ".lcurry") and the result is a FlatCurry term representing the
--- interface of this module.
readFlatCurryInt :: String -> IO Prog
readFlatCurryInt progname = do
   readFlatCurryIntWithParseOptions progname (setQuiet True defaultParams)

--- I/O action which parses Curry program
--- with respect to some parser options and returns the FlatCurry
--- interface of this program, i.e.,
--- a FlatCurry program containing only "Public" entities and function
--- definitions without rules (i.e., external functions).
--- The argument is the file name without suffix ".curry"
--- (or ".lcurry") and the result is a FlatCurry term representing the
--- interface of this module.
readFlatCurryIntWithParseOptions :: String -> FrontendParams -> IO Prog
readFlatCurryIntWithParseOptions progname options = do
  mbsrc <- lookupModuleSourceInLoadPath progname
  case mbsrc of
    Nothing -> do -- no source file, try to find FlatCurry file in load path:
      loadpath <- getLoadPathForModule progname
      filename <- getFileWithSuffix
                    (flatCurryIntName (takeFileName progname)) [""]
                    loadpath
      readFlatCurryFile filename
    Just (dir,_) -> do
      callFrontendWithParams FINT options progname
      readFlatCurryFile (flatCurryIntName (dir </> takeFileName progname))

--- Writes a FlatCurry program into a file in ".fcy" format.
--- The first argument must be the name of the target file
--- (with suffix ".fcy").
writeFCY :: String -> Prog -> IO ()
writeFCY file prog = writeFile file (showTerm prog)

--- Returns the name of the FlatCurry file of a module in the load path,
--- if this file exists.
lookupFlatCurryFileInLoadPath :: String -> IO (Maybe String)
lookupFlatCurryFileInLoadPath modname =
  getLoadPathForModule modname >>=
  findFileWithSuffix (flatCurryFileName modname) [""]

--- Returns the name of the FlatCurry file of a module in the load path,
--- if this file exists.
getFlatCurryFileInLoadPath :: String -> IO String
getFlatCurryFileInLoadPath modname =
  getLoadPathForModule modname >>=
  getFileWithSuffix (flatCurryFileName modname) [""]
