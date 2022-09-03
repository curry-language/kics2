module KiCS2.BuildGenerator.Configure
  ( configureFile
  ) where

import Control.Monad ( join )
import Control.Monad.IO.Class ( MonadIO (..) )
import Data.List ( splitOn )
import KiCS2.BuildGenerator.Options ( Options (..), optionVars )
import KiCS2.BuildGenerator.Utils ( replace, mapTail, takeIdentifier )
import System.FilePath ( dropExtension )

-- | Substitutes the options into a .in file.
configureFile :: MonadIO m => Options -> FilePath -> m ()
configureFile o inPath = liftIO $ do
  s <- readFile inPath
  let outPath = dropExtension inPath
      s' = configureString o s
  writeFile outPath s'
  
-- | Substitutes the options into a string.
configureString :: Options -> String -> String
configureString o = join . mapTail configureVar . splitOn "$"
  where
    vars = optionVars o
    configureVar chunk =
      let name = takeIdentifier chunk
          rest = drop (length name) chunk
      in case lookup name vars of
        Just value -> value ++ rest
        Nothing    -> error $ "Variable $" ++ name ++ " is not defined!"
