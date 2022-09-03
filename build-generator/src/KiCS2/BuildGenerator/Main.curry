module KiCS2.BuildGenerator.Main
  ( main
  ) where

import KiCS2.BuildGenerator.Build.Compiler ( compilerNinja )
import KiCS2.BuildGenerator.Build.Frontend ( frontendNinja )
import KiCS2.BuildGenerator.Rule.Curry ( curryNinja )
import KiCS2.BuildGenerator.Rule.Stack ( stackNinja )
import KiCS2.BuildGenerator.Rule.Utils ( utilsNinja )
import KiCS2.BuildGenerator.Options ( Options, defaultOptions, parseOptions, optionsNinja )
import Language.Ninja.Builder ( NinjaBuilder, execNinjaBuilder, comment, whitespace )
import Language.Ninja.Pretty ( ppNinja )
import System.Environment ( getArgs )

topLevelNinja :: Options -> NinjaBuilder ()
topLevelNinja o = do
  comment "Options"
  optionsNinja o
  whitespace

  comment "Rules"
  utilsNinja o
  curryNinja o
  stackNinja o
  whitespace

  comment "Builds"
  frontendNinja o
  compilerNinja o

main :: IO ()
main = do
  args <- getArgs
  case parseOptions "generate-build" args of
    Right opts -> do
      ninja <- execNinjaBuilder $ topLevelNinja opts
      writeFile "build.ninja" $ ppNinja ninja
    Left e     -> putStrLn e
