module KiCS2.BuildGenerator.Configure
  ( configureFile
  ) where

import Control.Monad ( join, unless )
import Control.Monad.IO.Class ( MonadIO (..) )
import Data.List ( splitOn )
import KiCS2.BuildGenerator.Options ( Options (..), optionVars )
import KiCS2.BuildGenerator.Utils ( replace, mapTail, takeIdentifier )
import System.FilePath ( dropExtension )
import System.Directory ( doesFileExist )

-- | Substitutes the options into a .in file (though only if something changed).
configureFile :: MonadIO m => Options -> FilePath -> m ()
configureFile o inPath = liftIO $ do
  template <- readFile inPath

  let outPath = dropExtension inPath
      configured = configureString o template
  
  exists <- doesFileExist outPath
  unless exists $
    writeFile outPath configured
  
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
