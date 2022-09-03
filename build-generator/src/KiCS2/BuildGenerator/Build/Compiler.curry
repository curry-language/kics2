module KiCS2.BuildGenerator.Build.Compiler
  ( compilerNinja
  ) where

import Data.List ( intercalate )
import KiCS2.BuildGenerator.Options ( Options (..), optRootDir, optPackageJson, optDotCpmDir
                                    , optKics2cBin, optKics2iBin, optLocalBinDir, optLibDir
                                    , optVersion, optSrcDir, optBootDir
                                    )
import KiCS2.BuildGenerator.Utils ( findWithSuffix, concatMapM, forM_, listDir, replace )
import Language.Ninja.Types
import Language.Ninja.Builder ( NinjaBuilder, build, rule )
import System.FilePath ( (</>), (<.>), makeRelative, dropExtension, pathSeparator )

-- | The Ninja source for building the compiler.
compilerNinja :: Options -> NinjaBuilder ()
compilerNinja o = do
  let srcDir      = optSrcDir o
      depsDir     = optDotCpmDir o </> "packages"
      pkgJson     = optPackageJson o
      compileMain = "KiCS2.Compile"
      replMain    = "KiCS2.REPL"
      bins        =
        [ (compileMain, "compiler", optKics2cBin o)
        , (replMain,    "REPL",     optKics2iBin o)
        ]

  srcs <- concatMapM (findWithSuffix ".curry") [srcDir, depsDir]

  -- Compiling kics2c/i directly using $curry

  forM_ bins $ \(main, description, bin) -> do
    build $ ([bin] :. ("curry", [pkgJson]) |. srcs)
      { buildVariables =
          [ "main" =. main
          ]
      , buildDescription = Just $ "Building " ++ description ++ " (" ++ main ++ ")"
      }

  -- Bootstrapping

  depNames <- listDir depsDir

  let stage i = "stage" ++ show i
      stageDir i = optLocalBinDir o </> stage i
      stageBin i = stageDir i </> "kics2c"
  
  build $ ([stageBin 1] :. ("cp", [optKics2cBin o]))
  
  forM_ [2..3] $ \i -> do
    let description = "Building stage " ++ show i ++ " compiler"

    let outDir = stageDir i </> ".curry" </> "kics2-" ++ optVersion o
        srcPath = (srcDir </>) . (<.> "curry") . replace '.' pathSeparator
        compileMainSrc = srcPath compileMain
        hsPath = (outDir </>) . (<.> "hs") . dropExtension . makeRelative srcDir
        hsSrcs = hsPath <$> srcs

    let depSrcDirs = (</> "src") <$> depNames
        libDir = optLibDir o
        currypath = intercalate ":" $ libDir : depSrcDirs

    build $ (hsSrcs :. ("kics2c", [compileMainSrc]) |. [stageBin (i - 1)])
      { buildDescription = Just $ description ++ " (Curry -> Haskell)"
      , buildVariables =
          [ "kics2c" =. stageBin (i - 1)
          , "kics2copts" =. "-v2 --parse-options=-Wall -o" ++ outDir ++ " -i" ++ currypath
          ]
      }

    let bootDir = optBootDir o
        compileBootHsSrc = bootDir </> "CompileBoot.hs"

    build $ ([stageBin i] :. ("ghc", [compileBootHsSrc]) |. hsSrcs)
      { buildDescription = Just $ description ++ " (Haskell -> Native)"
      , buildVariables =
          [ "ghcopts" =. "$ghcopts --make -v1 -cpp"
          ]
      }
