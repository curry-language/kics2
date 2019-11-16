--- --------------------------------------------------------------------------
--- A collection of operations for dealing with file names
---
--- @author  Bjoern Peemoeller
--- @version July 2012
--- --------------------------------------------------------------------------
module Files
  ( -- File name modification
    withComponents, withDirectory, withBaseName, withExtension
    -- file creation
  , writeFileInDir, writeQTermFileInDir
    -- file deletion
  , removeFileIfExists, (</?>), lookupFileInPath, getFileInPath
  ) where

import System.Directory
  ( createDirectory, createDirectoryIfMissing, doesDirectoryExist
  , doesFileExist, removeFile
  )
import System.FilePath
  ( FilePath, joinPath, (</>), (<.>), isAbsolute, searchPathSeparator
  , splitFileName, splitExtension, splitDirectories, takeDirectory
  )
import Control.Monad (when)
import Data.List     (intersperse, isPrefixOf, last, scanl1)
import ReadShowTerm  (writeQTermFile)

--- Apply functions to all parts of a file name
withComponents :: (String -> String) -- change path
               -> (String -> String) -- change base name
               -> (String -> String) -- change suffix
               -> String -> String
withComponents pf bf sf fn = pf path </> bf base <.> sf suffix
  where (path, bassfx) = splitFileName fn
        (base, suffix) = splitExtension bassfx

--- Apply a function to the directory component of a file path
withDirectory :: (String -> String) -> String -> String
withDirectory f fn = withComponents f id id fn

--- Apply a function to the base name component of a file path
withBaseName :: (String -> String) -> String -> String
withBaseName f fn = withComponents id f id fn

--- Apply a function to the extension component of a file path
withExtension :: (String -> String) -> String -> String
withExtension f fn = withComponents id id f fn

--- write the 'String' into a file where the file name may contain a path.
--- The corresponding directories are created first if missing.
writeFileInDir :: String -> String -> IO ()
writeFileInDir file content = do
  createDirectoryIfMissing True $ takeDirectory file
  writeFile file content

--- write the 'String' into a file where the file name may contain a path.
--- The corresponding directories are created first if missing.
writeQTermFileInDir :: FilePath -> a -> IO ()
writeQTermFileInDir file content = do
  createDirectoryIfMissing True $ takeDirectory file
  writeQTermFile file content

--- This operation removes the specified file only if it exists.
removeFileIfExists :: FilePath -> IO ()
removeFileIfExists file = do
  exists <- doesFileExist file
  when exists $ removeFile file

--- `dir </?> subdir` appends only the path components of `subdir` to `dir`
--- which are not already the suffix if `dir`. Examples:
---
--- `"debug"              </?> ".curry/kics2"` -> "debug/.curry/kics2"
--- `"debug/.curry"       </?> ".curry/kics2"` -> "debug/.curry/kics2"
--- `"debug/.curry/kics2" </?> ".curry/kics2"` -> "debug/.curry/kics2"
(</?>) :: FilePath -> FilePath -> FilePath
fn </?> sfx = joinPath $ reverse $ add (reverse $ splitDirectories sfx)
                                       (reverse $ splitDirectories fn)
  where
  add []         dirs = dirs
  add dir@(d:ds) dirs
    | dir `isPrefixOf` dirs = dirs
    | otherwise             = d : add ds dirs

--- Looks up the first file with a possible extension in a list of directories.
--- Returns Nothing if such a file does not exist.
lookupFileInPath :: FilePath -> [String] -> [FilePath] -> IO (Maybe FilePath)
lookupFileInPath file exts path
  | isAbsolute file = lookupExtFile file exts
  | otherwise       = lookupFile path
  where
    lookupFile []         = return Nothing
    lookupFile (dir:dirs) = do
      mbfile <- lookupExtFile (dir </> file) exts
      maybe (lookupFile dirs) (return . Just) mbfile

    lookupExtFile _ []         = return Nothing
    lookupExtFile f (e:es) = do
      let fExt = f <.> e
      exists <- doesFileExist fExt
      if exists then return (Just fExt) else lookupExtFile f es

--- Gets the first file with a possible suffix in a list of directories.
--- An error message is delivered if there is no such file.
getFileInPath :: FilePath -> [String] -> [FilePath] -> IO FilePath
getFileInPath file exts path = do
  mbfile <- lookupFileInPath file exts path
  maybe (error $ "File " ++ file ++ " not found in path " ++
                 concat (intersperse [searchPathSeparator] path))
        return
        mbfile
