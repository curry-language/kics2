module KiCS2.BuildGenerator.Build.Meta
  ( metaNinja
  ) where

import KiCS2.BuildGenerator.Options ( Options (..), optRootDir )
import KiCS2.BuildGenerator.Utils ( findWithSuffix )
import Language.Ninja.Builder ( NinjaBuilder, build )
import Language.Ninja.Types
import System.FilePath ( (</>) )

-- | The Ninja source for (re)building the `build.ninja` itself.
metaNinja :: Options -> NinjaBuilder ()
metaNinja o = do
  let rootDir = optRootDir o
      generatorMain = "KiCS2.BuildGenerator.Main"
      generatorDir = rootDir </> "build-generator"
      generatorPkgJson = generatorDir </> "package.json"
      generatorSrcDir = generatorDir </> "src"
      generatorBin = generatorDir </> "bin" </> "generate-build"
  
  generatorSrcs <- findWithSuffix ".curry" generatorSrcDir

  build $ ([generatorBin] :. ("curry", [generatorPkgJson]) |. generatorSrcs)
    { buildVariables =
        [ "main" =. generatorMain
        ]
    }

  build $ ([rootDir </> "build.ninja"] :. ("run", [generatorBin]))
    { buildGenerator = True
    }
