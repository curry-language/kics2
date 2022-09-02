module KiCS2.BuildGenerator.Main
  ( main
  ) where

import KiCS2.BuildGenerator.Build.Frontend ( frontendNinja )
import KiCS2.BuildGenerator.Rule.Stack ( stackNinja )
import KiCS2.BuildGenerator.Options ( Options, defaultOptions)
import Language.Ninja.Builder ( NinjaBuilder, execNinjaBuilder, build, rule )
import Language.Ninja.Pretty ( ppNinja )

topLevelNinja :: Options -> NinjaBuilder ()
topLevelNinja o = do
  -- Rules
  stackNinja o

  -- Builds
  frontendNinja o

main :: IO ()
main = do
  let opts = defaultOptions
  ninja <- execNinjaBuilder $ topLevelNinja opts
  writeFile "build.ninja" $ ppNinja ninja
