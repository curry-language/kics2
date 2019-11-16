--- Library for accessing the directory structure of the
--- underlying operating system.
---
--- @author Michael Hanus
--- @version January 2013
--- @category general

module System.Directory
  ( doesFileExist, doesDirectoryExist, getFileSize, getModificationTime
  , getCurrentDirectory, setCurrentDirectory
  , getDirectoryContents, createDirectory, createDirectoryIfMissing
  , removeDirectory, renameDirectory
  , getHomeDirectory, getTemporaryDirectory
  , getAbsolutePath
  , removeFile, renameFile, copyFile
  , findFileWithSuffix, getFileWithSuffix
  ) where

import System.FilePath    ( FilePath, (</>), splitDirectories, isAbsolute
                          , normalise, pathSeparator, searchPathSeparator)
import System.Environment (getEnv, isWindows)
import Data.List          (isPrefixOf, scanl1, last, intersperse)
import Data.Time          (ClockTime)


--- Returns true if the argument is the name of an existing file.
doesFileExist :: FilePath -> IO Bool
doesFileExist fname = prim_doesFileExist $## fname

prim_doesFileExist :: FilePath -> IO Bool
prim_doesFileExist external

--- Returns true if the argument is the name of an existing directory.
doesDirectoryExist :: FilePath -> IO Bool
doesDirectoryExist dir = prim_doesDirectoryExist $## dir

prim_doesDirectoryExist :: FilePath -> IO Bool
prim_doesDirectoryExist external

--- Returns the size of the file.
getFileSize :: FilePath -> IO Int
getFileSize fname = prim_fileSize $## fname

prim_fileSize :: FilePath -> IO Int
prim_fileSize external

--- Returns the modification time of the file.
getModificationTime :: FilePath -> IO ClockTime
getModificationTime fname = prim_getModificationTime $## fname

prim_getModificationTime :: FilePath -> IO ClockTime
prim_getModificationTime external

--- Returns the current working directory.
getCurrentDirectory :: IO FilePath
getCurrentDirectory external

--- Sets the current working directory.
setCurrentDirectory :: FilePath -> IO ()
setCurrentDirectory dir = prim_setCurrentDirectory $## dir

prim_setCurrentDirectory :: FilePath -> IO ()
prim_setCurrentDirectory external

--- Returns the list of all entries in a directory.
getDirectoryContents :: FilePath -> IO [FilePath]
getDirectoryContents dir = prim_getDirectoryContents $## dir

prim_getDirectoryContents :: FilePath -> IO [FilePath]
prim_getDirectoryContents external

--- Creates a new directory with the given name.
createDirectory :: FilePath -> IO ()
createDirectory dir = prim_createDirectory $## dir

prim_createDirectory :: FilePath -> IO ()
prim_createDirectory external

--- Creates a new directory with the given name if it does not already exist.
--- If the first parameter is `True` it will also create all missing
--- parent directories.
createDirectoryIfMissing :: Bool -> FilePath -> IO ()
createDirectoryIfMissing createParents path
  = if createParents then createDirs parents
                     else createDirs [last parents]
 where
  parents = scanl1 (</>) $ splitDirectories $ path

  createDirs []     = return ()
  createDirs (d:ds) = do
    exists <- doesDirectoryExist d
    if exists then return () else createDirectory d
    createDirs ds

--- Deletes a directory from the file system.
removeDirectory :: FilePath -> IO ()
removeDirectory dir = prim_removeDirectory $## dir

prim_removeDirectory :: FilePath -> IO ()
prim_removeDirectory external

--- Renames a directory.
renameDirectory :: FilePath -> FilePath -> IO ()
renameDirectory dir1 dir2 = (prim_renameDirectory $## dir1) $## dir2

prim_renameDirectory :: FilePath -> FilePath -> IO ()
prim_renameDirectory external

--- Returns the home directory of the current user.
getHomeDirectory :: IO FilePath
getHomeDirectory = if isWindows
                      then getEnv "USERPROFILE"
                      else getEnv "HOME"

--- Returns the temporary directory of the operating system.
getTemporaryDirectory :: IO FilePath
getTemporaryDirectory = if isWindows then getEnv "TMP" else return "/tmp"

--- Convert a path name into an absolute one.
--- For instance, a leading `~` is replaced by the current home directory.
getAbsolutePath :: FilePath -> IO FilePath
getAbsolutePath path
  | isAbsolute path        = return (normalise path)
  | path == "~"            = getHomeDirectory
  | "~/" `isPrefixOf` path = do homedir <- getHomeDirectory
                                return (normalise (homedir </> drop 2 path))
  | otherwise              = do curdir <- getCurrentDirectory
                                return (normalise (curdir </> path))

--- Deletes a file from the file system.
removeFile :: FilePath -> IO ()
removeFile file = prim_removeFile $## file

prim_removeFile :: FilePath -> IO ()
prim_removeFile external

--- Renames a file.
renameFile :: FilePath -> FilePath -> IO ()
renameFile file1 file2 = (prim_renameFile $## file1) $## file2

prim_renameFile :: FilePath -> FilePath -> IO ()
prim_renameFile external

--- Copy the contents from one file to another file
copyFile :: FilePath -> FilePath -> IO ()
copyFile src dest = readFile src >>= writeFile dest

--- Looks up the first file with a possible suffix in a list of directories.
--- Returns Nothing if such a file does not exist.
findFileWithSuffix :: String -> [String] -> [String] -> IO (Maybe String)
findFileWithSuffix file suffixes path =
  if isAbsolute file
  then lookupFirstFileWithSuffix file suffixes
  else lookupFirstFile path
 where
   lookupFirstFile [] = return Nothing
   lookupFirstFile (dir:dirs) = do
     mbfile <- lookupFirstFileWithSuffix (dir++pathSeparator:file) suffixes
     maybe (lookupFirstFile dirs) (return . Just) mbfile

   lookupFirstFileWithSuffix _ [] = return Nothing
   lookupFirstFileWithSuffix f (suf:sufs) = do
     let fsuf = f++suf
     exfile <- doesFileExist fsuf
     if exfile then return (Just fsuf)
               else lookupFirstFileWithSuffix f sufs

--- Gets the first file with a possible suffix in a list of directories.
--- An error message is delivered if there is no such file.
getFileWithSuffix :: String -> [String] -> [String] -> IO String
getFileWithSuffix file suffixes path = do
  mbfile <- findFileWithSuffix file suffixes path
  maybe (error $ "File "++file++" not found in path "++
                 concat (intersperse [searchPathSeparator] path))
        return
        mbfile
