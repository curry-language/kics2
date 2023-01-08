----------------------------------------------------------------------
--- Some operations to handle the KiCS2 resource configuration file
--- that is stored in $HOME/.kics2rc
---
--- @author  Michael Hanus
--- @version April 2015
----------------------------------------------------------------------

module KiCS2.RCFile (readRC, rcValue, setRCProperty, extractRCArgs, updateRCDefs)
  where

import Data.PropertyFile
import Data.Char        (toLower)
import Data.List        (partition, sort)
import Control.Monad    (unless)
import System.Directory (getHomeDirectory, doesFileExist, copyFile, renameFile)
import System.FilePath  ((</>), (<.>))

import KiCS2.Utils        (strip)
import KiCS2.InstallationPaths (kics2HomeDir)

defaultRC :: IO FilePath
defaultRC = (</> "kics2rc.default") <$> kics2HomeDir

--- Location of the rc file of a user.
--- After bootstrapping, one can also use Distribution.rcFileName
--- The name of the file specifying configuration parameters of the
--- current distribution. This file must have the usual format of
--- property files (see description in module PropertyFile).
rcFileName :: IO FilePath
rcFileName = (</> ".kics2rc") <$> getHomeDirectory

--- Reads the rc file. If it is not present, the standard file
--- from the distribution will be copied.
readRC :: IO [(String, String)]
readRC = do
  rcName   <- rcFileName
  rcExists <- doesFileExist rcName
  defRC    <- defaultRC
  catch (if rcExists then updateRC else copyFile defRC rcName)
    (const (return ()))
  -- check again existence of user rc file:
  newrcExists <- doesFileExist rcName
  readPropertyFile (if newrcExists then rcName else defRC)

rcKeys :: [(String, String)] -> [String]
rcKeys = sort . map fst

--- Reads the rc file (which must be present) and compares the definitions
--- with the distribution rc file. If the set of variables is different,
--- update the rc file with the distribution but keep the user's definitions.
updateRC :: IO ()
updateRC = do
  rcName    <- rcFileName
  defRC     <- defaultRC
  userprops <- readPropertyFile rcName
  distprops <- readPropertyFile defRC
  unless (rcKeys userprops == rcKeys distprops) $ do
    putStrLn $ "Updating \"" ++ rcName ++ "\"..."
    renameFile rcName $ rcName <.> "bak"
    copyFile defRC rcName
    mapM_ (\ (n, v) -> maybe (return ())
              (\uv -> unless (uv == v) $ updatePropertyFile rcName n uv)
              (lookup n userprops))
          distprops

--- Sets a property in the rc file.
setRCProperty :: String -> String -> IO ()
setRCProperty pname pval = do
  _ <- readRC -- just be to sure that rc file exists and is up-to-date
  rcName <- rcFileName
  updatePropertyFile rcName pname pval

--- Look up a configuration variable in the list of variables from the rc file.
--- Uppercase/lowercase is ignored for the variable names and the empty
--- string is returned for an undefined variable.
rcValue :: [(String, String)] -> String -> String
rcValue rcdefs var = strip $ maybe "" id $
  lookup (map toLower var) (map (\(a,b) -> (map toLower a, b)) rcdefs)


--- Extract from a list of command-line arguments rc properties
--- of the from "-Dprop=val", which must be the first arguments,
--- and return the remaining arguments and the extracted properties.
extractRCArgs :: [String] -> ([String],[(String,String)])
extractRCArgs args =
  let (dargs,otherargs) = break (\s -> take 2 s /= "-D") args
   in (otherargs, map splitDefs (map (drop 2) dargs))
 where
  splitDefs darg = case break (=='=') darg of
    (var,_:val) -> (var,val)
    _           -> (darg,"")

--- Update list of rc properties w.r.t. a list new properties.
updateRCDefs :: [(String,String)] -> [(String,String)] -> [(String,String)]
updateRCDefs orgdefs newdefs =
  map (\ (name,val) -> (name, maybe val id (lookup name newdefs))) orgdefs
