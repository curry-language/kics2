module KiCS2.BuildGenerator.Imports
  ( readImports, parseImports
  ) where

import Control.Monad.IO.Class ( MonadIO (..) )
import Data.List ( isPrefixOf )
import KiCS2.BuildGenerator.Utils ( isIdentifierChar )

-- | Reads the imports of a Curry or Haskell source file.
readImports :: MonadIO m => FilePath -> m [String]
readImports = liftIO . (parseImports <$>) . readFile

-- | Parses the imports from the given Curry or Haskell source code.
parseImports :: String -> [String]
parseImports = map (takeWhile (\c -> isIdentifierChar c || c == '.') . dropWhile isSpace . drop (length prefix))
             . filter (prefix `isPrefixOf`)
             . lines
  where
    prefix = "import"
