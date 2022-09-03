module KiCS2.BuildGenerator.Utils
  ( concatMapM, forM_, findWithSuffix, walk
  ) where

import Control.Monad ( join )
import Control.Monad.IO.Class ( MonadIO (..) )
import Data.List ( isSuffixOf )
import System.Directory ( getDirectoryContents, doesDirectoryExist )
import System.FilePath ( FilePath, (</>) )

-- | Flattening map, but over monads.
concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f = (join <$>) . mapM f

-- | Flipped mapM_.
forM_ :: Monad m => [a] -> (a -> m b) -> m ()
forM_ = flip mapM_

-- | Recursively finds all files with the given suffix in the directory.
findWithSuffix :: MonadIO m => String -> FilePath -> m [FilePath]
findWithSuffix suffix = (filter (suffix `isSuffixOf`) <$>) . walk (const True)

-- | Recursively finds all files matching the given predicate in the directory.
walk :: MonadIO m => (FilePath -> Bool) -> FilePath -> m [FilePath]
walk f path | not (f path) = return [path]
            | otherwise    = liftIO $ do
  isDir <- doesDirectoryExist path
  if isDir
    then do
      childs <- map (path </>) . filter (not . flip elem [".", ".."]) <$> getDirectoryContents path
      concatMapM (walk f) childs
    else return [path]
