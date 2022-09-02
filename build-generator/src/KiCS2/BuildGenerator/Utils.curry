module KiCS2.BuildGenerator.Utils
  ( findWithSuffix, walk
  ) where

import Control.Monad ( join )
import Data.List ( isSuffixOf )
import System.Directory ( getDirectoryContents, doesDirectoryExist )
import System.FilePath ( FilePath, (</>) )

-- | Recursively finds all files with the given suffix in the directory.
findWithSuffix :: String -> FilePath -> IO [FilePath]
findWithSuffix suffix = walk (suffix `isSuffixOf`)

-- | Recursively finds all files matching the given predicate in the directory.
walk :: (FilePath -> Bool) -> FilePath -> IO [FilePath]
walk f path | not (f path) = return [path]
            | otherwise    = do
  isDir <- doesDirectoryExist path
  if isDir
    then do
      childs <- map (path </>) . filter (not . flip elem [".", ".."]) <$> getDirectoryContents path
      join <$> mapM (walk f) childs
    else return [path]
