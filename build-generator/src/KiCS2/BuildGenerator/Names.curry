module KiCS2.BuildGenerator.Names
  ( curryToHsFileName, relativizeToSrc
  , curryToHsFilePath
  ) where

import Data.List ( intercalate, splitOn )
import System.FilePath ( (<.>), (</>), dropExtension, pathSeparator, takeFileName, replaceFileName )

-- | Converts a file name like `Module.curry` to `Curry_Module.hs`.
curryToHsFileName :: String -> String
curryToHsFileName = (<.> "hs") . ("Curry_" ++) . dropExtension

-- | Relativizes a file path to the innermost `src` folder.
-- A file path like `/a/b/c/src/Some/Module.curry`, for example, is mapped to `Some/Module.curry`.
relativizeToSrc :: FilePath -> FilePath
relativizeToSrc = intercalate [pathSeparator] . reverse . takeWhile (/= "src") . reverse . splitOn [pathSeparator]

-- | Converts a path like `/a/b/c/src/Some/Module.curry` to `<out dir>/kics2-<version>/Some/Curry_Module.hs`.
curryToHsFilePath :: FilePath -> FilePath -> FilePath
curryToHsFilePath outDir = (outDir </>)
                         . (\n -> replaceFileName n $ curryToHsFileName $ takeFileName n)
                         . relativizeToSrc
