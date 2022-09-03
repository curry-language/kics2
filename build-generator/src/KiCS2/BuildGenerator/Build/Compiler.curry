module KiCS2.BuildGenerator.Build.Compiler
  ( compilerNinja
  ) where

import KiCS2.BuildGenerator.Options ( Options (..), optRootDir, optPackageJson, optDotCpmDir
                                    , optKics2cBin, optKics2iBin
                                    )
import KiCS2.BuildGenerator.Utils ( findWithSuffix, concatMapM, forM_ )
import Language.Ninja.Types
import Language.Ninja.Builder ( NinjaBuilder, build, rule )
import System.FilePath ( (</>) )

-- | The Ninja source for building the compiler.
compilerNinja :: Options -> NinjaBuilder ()
compilerNinja o = do
  let srcDir   = optRootDir o </> "src"
      depsDir  = optDotCpmDir o </> "packages"
      pkgJson  = optPackageJson o
      bins     =
        [ ("KiCS2.Compile", optKics2cBin o)
        , ("KiCS2.REPL",    optKics2iBin o)
        ]

  srcs <- concatMapM (findWithSuffix ".curry") [srcDir, depsDir]

  forM_ bins $ \(main, bin) -> do
    build $ ([bin] :. ("curry", [pkgJson]) |. srcs)
      { buildVariables =
          [ "main" =. main
          ]
      }
