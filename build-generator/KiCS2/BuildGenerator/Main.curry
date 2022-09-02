module KiCS2.BuildGenerator.Main
  ( main
  ) where

import Ninja.Builder ( execNinjaBuilder, build, rule )
import Ninja.Pretty ( ppNinja )
import Ninja.Types

-- TODO: Add actual rules

main :: IO ()
main = writeFile "build.ninja" $ ppNinja $ execNinjaBuilder $ do
  build $ ["a"] :. ("b", [])
  build $ ["test"] :. ("demo", ["a"]) ||. ["order-only"]
