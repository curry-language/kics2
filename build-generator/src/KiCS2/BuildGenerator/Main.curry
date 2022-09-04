module KiCS2.BuildGenerator.Main
  ( main
  ) where

import KiCS2.BuildGenerator.Build.Compiler ( compilerNinja )
import KiCS2.BuildGenerator.Build.Frontend ( frontendNinja )
import KiCS2.BuildGenerator.Build.Libraries ( librariesNinja )
import KiCS2.BuildGenerator.Configure ( configureFile )
import KiCS2.BuildGenerator.Rule.Curry ( curryNinja )
import KiCS2.BuildGenerator.Rule.Haskell ( haskellNinja )
import KiCS2.BuildGenerator.Rule.Utils ( utilsNinja )
import KiCS2.BuildGenerator.Options ( Options (..), defaultOptions, parseOptions, optionsNinja )
import KiCS2.BuildGenerator.Utils ( findWithSuffix, forM_ )
import Language.Ninja.Builder ( NinjaBuilder, execNinjaBuilder, comment, whitespace )
import Language.Ninja.Pretty ( ppNinja )
import System.Environment ( getArgs )

-- | The source for the generated `build.ninja` file.
topLevelNinja :: Options -> NinjaBuilder ()
topLevelNinja o = do
  comment "Options"
  optionsNinja o
  whitespace

  comment "Rules"
  utilsNinja o
  curryNinja o
  haskellNinja o
  whitespace

  comment "Builds"
  frontendNinja o
  librariesNinja o
  compilerNinja o

-- | Applies `.in` templates and generates the `build.ninja` file.
main :: IO ()
main = do
  args <- getArgs
  case parseOptions "generate-build" args of
    Right opts -> do
      putStrLn "Finding .in templates..."
      inPaths <- findWithSuffix ".in" $ optRootDir opts

      forM_ inPaths $ \inPath -> do
        putStrLn $ "Configuring " ++ inPath ++ "..."
        configureFile opts inPath

      putStrLn "Generating build.ninja..."
      ninja <- execNinjaBuilder $ topLevelNinja opts
      writeFile "build.ninja" $ ppNinja ninja
    Left e     -> putStrLn e
