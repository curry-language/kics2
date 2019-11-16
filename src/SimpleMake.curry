module SimpleMake (smake) where

import System.Directory
import Data.Time

smake :: String -> [String] -> IO a -> IO a -> IO a
smake dest deps cmd alt = do
  destTime <- getDestTime dest
  depTimes <- getDepTimes deps
  make destTime depTimes
    where
      make Nothing _              = cmd  -- target file is not existent
      make (Just dep) destTimes
        | outOfDate dep destTimes = cmd  -- target file is out-dated
        | otherwise               = alt  -- target file is up-to-date

getDestTime :: String -> IO (Maybe ClockTime)
getDestTime fn = do
  exists <- doesFileExist fn
  if exists
    then Just <$> getModificationTime fn
    else return Nothing

getDepTimes :: [String] -> IO [ClockTime]
getDepTimes = mapM getModificationTime

-- Check whether the destination file is outdated, i.e. if any file it
-- depends on is newer
outOfDate :: ClockTime -> [ClockTime] -> Bool
outOfDate dest deps = any (> dest) deps
