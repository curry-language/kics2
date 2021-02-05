------------------------------------------------------------------------------
--- This library defines I/O actions to read and write
--- annotated FlatCurry programs.
---
--- @author Michael Hanus
--- @version December 2020
------------------------------------------------------------------------------

module KiCS2.FlatCurry.Annotated.Files where

import KiCS2.System.CurryPath    ( lookupModuleSourceInLoadPath
                                 , inCurrySubdir, stripCurrySuffix )
import System.Directory          ( doesFileExist )
import System.FilePath           ( (</>), (<.>) )

import FlatCurry.Annotated.Types

--- Transforms a name of a Curry program (with or without suffix ".curry"
--- or ".lcurry") into the name of the file containing the
--- corresponding annotated FlatCurry program.
annotatedFlatCurryFileName :: String -> String
annotatedFlatCurryFileName prog =
  inCurrySubdir (stripCurrySuffix prog) <.> "afcy"

--- Gets the standard annotated FlatCurry file location
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

--- where the file name is provided as the argument.
readAnnotatedFlatCurryFile :: Read a => String -> IO (AProg a)
readAnnotatedFlatCurryFile filename = do
  filecontents <- readAnnotatedFlatCurryFileRaw filename
  return (read filecontents)
 where
  readAnnotatedFlatCurryFileRaw fname = do
    exafcy <- doesFileExist fname
    if exafcy
      then readFile fname
      else do
        let subdirfilename = inCurrySubdir fname
        exdirafcy <- doesFileExist subdirfilename
        if exdirafcy
          then readFile subdirfilename
          else error $ "EXISTENCE ERROR: Annotated FlatCurry file '" ++
                       fname ++ "' does not exist"

--- Writes an annotated FlatCurry program into a file in `.afcy` format.
--- The file is written in the standard location for intermediate files,
--- i.e., in the 'annotatedFlatCurryFileName' relative to the directory of the
--- Curry source program (which must exist!).
writeAnnotatedFlatCurry :: Show a => AProg a -> IO ()
writeAnnotatedFlatCurry prog@(AProg mname _ _ _ _) = do
  fname <- annotatedFlatCurryFilePath mname
  writeAnnotatedFlatCurryFile fname prog

--- Writes an annotated FlatCurry program into a file in ".afcy" format.
--- The first argument must be the name of the target file
--- (with suffix `.afcy`).
writeAnnotatedFlatCurryFile :: Show a => String -> AProg a -> IO ()
writeAnnotatedFlatCurryFile file prog = writeFile file (show prog)
