module SimpleMake (smake) where

import System.Directory
import Data.Time

smake :: String -> [String] -> IO a -> IO a -> IO a
smake dest deps cmd alt = do
  destTime <- getDestTime dest
  depTimes <- getDepTimes deps

  case (destTime, depTimes) of
    (Just dt, Just dts) | outOfDate dt dts -> cmd -- target file is up-to-date
                        | otherwise        -> alt -- target file is outdated
    _                                      -> cmd -- either target file or not all deps exist

getDestTime :: String -> IO (Maybe ClockTime)
getDestTime fn = do
  exists <- doesFileExist fn
  if exists
    then Just <$> getModificationTime fn
    else return Nothing

getDepTimes :: [String] -> IO (Maybe [ClockTime])
getDepTimes = fmap sequence . mapM getDestTime

-- Check whether the destination file is outdated, i.e. if any file it
-- depends on is newer
outOfDate :: ClockTime -> [ClockTime] -> Bool
outOfDate dest deps = any (> dest) deps
