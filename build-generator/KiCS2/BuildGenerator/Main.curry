module KiCS2.BuildGenerator.Main
  ( main
  ) where

import KiCS2.BuildGenerator.Ninja

-- TODO: Add actual rules
-- TODO: Add a monad for conveniently building Ninjas

main :: IO ()
main = writeFile "build.ninja" $ ppNinja $ Ninja
  { ninjaBuilds =
    [ ["a"] :. ("b", [])
    , ["test"] :. ("demo", ["a"]) ||. ["order-only"]
    ]
  , ninjaRules =
    [
    ]
  }
