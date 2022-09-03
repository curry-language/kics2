module KiCS2.BuildGenerator.Configure
  ( configureFile
  ) where

import Control.Monad.IO.Class ( MonadIO (..) )
import KiCS2.BuildGenerator.Options ( Options (..), optionVars )
import KiCS2.BuildGenerator.Utils ( replace )
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
configureString o = foldr (.) id $ (\(k, v) -> replace ('$':k) v) <$> optionVars o
