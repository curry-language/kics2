module KiCS2.BuildGenerator.Utils
  ( concatMapM, forM_, findWithSuffix, walk, listDir
  , replaceSingle, replace, takeIdentifier, isIdentifierChar
  , mapTail, withReverse
  ) where

import Control.Monad ( join )
import Control.Monad.IO.Class ( MonadIO (..) )
import Data.Char ( isAlphaNum )
import Data.List ( isSuffixOf, splitOn, intercalate )
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
      childs <- listDir path
      concatMapM (walk f) childs
    else return [path]

-- | Lists the files in a directory.
listDir :: MonadIO m => FilePath -> m [FilePath]
listDir path = liftIO $ map (path </>) . filter (not . flip elem [".", ".."]) <$> getDirectoryContents path

-- | Replaces a value in a Functor.
replaceSingle :: (Functor f, Eq a) => a -> a -> f a -> f a
replaceSingle x y = fmap $ \x' -> if x == x' then y else x'

-- | Replaces a list of values.
replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace old new = intercalate new . splitOn old

-- | Whether this is a character valid for use in an identifier.
isIdentifierChar :: Char -> Bool
isIdentifierChar c = isAlphaNum c || c == '_'

-- | Extract an identifier prefix.
takeIdentifier :: String -> String
takeIdentifier = takeWhile isIdentifierChar

-- | Maps over the tail of a list.
mapTail :: (a -> a) -> [a] -> [a]
mapTail f xs = case xs of
  []     -> []
  (v:vs) -> v : (f <$> vs)

-- | Performs an operation on the reversed list.
withReverse :: ([a] -> [a]) -> [a] -> [a]
withReverse f = reverse . f . reverse
