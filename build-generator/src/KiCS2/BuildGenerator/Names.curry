module KiCS2.BuildGenerator.Names
  ( curryToHsFileName, relativizeToSrc, parentSrcDir
  , curryFilePathToMod, curryToHsFilePath
  ) where

import Data.List ( intercalate, splitOn )
import KiCS2.BuildGenerator.Utils ( replaceSingle, withReverse )
import System.FilePath ( (<.>), (</>), dropExtension, pathSeparator, takeFileName, replaceFileName )

-- | Converts a file name like `Module.curry` to `Curry_Module.hs`.
curryToHsFileName :: String -> String
curryToHsFileName = (<.> "hs") . ("Curry_" ++) . dropExtension

-- | Relativizes a file path to the innermost `src` folder.
-- A file path like `/a/b/c/src/Some/Module.curry`, for example, is mapped to `Some/Module.curry`.
relativizeToSrc :: FilePath -> FilePath
relativizeToSrc = withPath . withReverse $ takeWhile (/= "src")

-- | Finds the innermost `src` folder.
parentSrcDir :: FilePath -> FilePath
parentSrcDir = withPath . withReverse $ dropWhile (/= "src")

-- | Performs an operation on the path segments.
withPath :: ([String] -> [String]) -> FilePath -> FilePath
withPath f = intercalate [pathSeparator] . f . splitOn [pathSeparator]

-- | Converts a file path to a module name.
curryFilePathToMod :: FilePath -> String
curryFilePathToMod = replaceSingle pathSeparator '.' . dropExtension . relativizeToSrc

-- | Converts a path like `/a/b/c/src/Some/Module.curry` to `<out dir>/kics2-<version>/Some/Curry_Module.hs`.
curryToHsFilePath :: FilePath -> FilePath -> FilePath
curryToHsFilePath outDir = (outDir </>)
                         . (\n -> replaceFileName n $ curryToHsFileName $ takeFileName n)
                         . relativizeToSrc
