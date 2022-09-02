module KiCS2.BuildGenerator.Main
  ( main
  ) where

import KiCS2.BuildGenerator.Ninja

-- TODO: Add actual rules

main :: IO ()
main = writeFile "build.ninja" $ ppNinja $ execNinjaBuilder $ do
  build $ ["a"] :. ("b", [])
  build $ ["test"] :. ("demo", ["a"]) ||. ["order-only"]
