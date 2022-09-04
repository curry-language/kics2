module KiCS2.BuildGenerator.Build.Compiler
  ( compilerNinja
  ) where

import Data.List ( intercalate, splitOn )
import KiCS2.BuildGenerator.Names ( curryToHsFilePath )
import KiCS2.BuildGenerator.Options ( Options (..), optRootDir, optPackageJson, optDotCpmDir
                                    , optKics2cBin, optKics2iBin, optLocalBinDir, optLibDir
                                    , optVersion, optSrcDir, optBootDir
                                    )
import KiCS2.BuildGenerator.Utils ( findWithSuffix, concatMapM, forM_, listDir, replaceSingle )
import Language.Ninja.Types
import Language.Ninja.Builder ( NinjaBuilder, build, rule )
import System.FilePath ( (</>), (<.>), makeRelative, dropExtension, pathSeparator, replaceFileName, takeFileName )

-- | The Ninja source for building the compiler.
compilerNinja :: Options -> NinjaBuilder ()
compilerNinja o = do
  let srcDir          = optSrcDir o
      depsDir         = optDotCpmDir o </> "packages"
      pkgJson         = optPackageJson o
      compileMain     = "KiCS2.Compile"
      replMain        = "KiCS2.REPL"
      bins            =
        [ (compileMain, "compiler", optKics2cBin o)
        , (replMain,    "REPL",     optKics2iBin o)
        ]

  depNames <- listDir depsDir
  let depSrcDirs = (</> "src") <$> depNames

  srcs <- concatMapM (findWithSuffix ".curry") $ srcDir : depSrcDirs

  -- Compiling kics2c/i directly using $curry

  forM_ bins $ \(main, description, bin) -> do
    build $ ([bin] :. ("curry", [pkgJson]) |. srcs)
      { buildVariables =
          [ "main" =. main
          ]
      , buildDescription = Just $ "Building " ++ description ++ " (" ++ main ++ ")"
      }

  -- Bootstrapping

  let stage i = "stage" ++ show i
      stageDir i = optLocalBinDir o </> stage i
      stageBin i = stageDir i </> "kics2c"
  
  build $ [stageBin 1] :. ("cp", [optKics2cBin o])
  
  forM_ [2..3] $ \i -> do
    let description = "Building stage " ++ show i ++ " compiler"

    let outDir = stageDir i </> ".curry" </> "kics2-" ++ optVersion o
        hsSrcs = curryToHsFilePath outDir <$> srcs
        libDir = optLibDir o
        kics2cIncludes = intercalate ":" $ libDir : depSrcDirs
        ghcIncludes = "TODO" -- TODO

    build $ (hsSrcs :. ("kics2c", []) |. stageBin (i - 1) : srcs)
      { buildDescription = Just $ description ++ " (Curry -> Haskell)"
      , buildVariables =
          [ "mod" =. compileMain
          , "kics2c" =. stageBin (i - 1)
          , "kics2c_opts" =. "$kics2c_opts -o" ++ outDir ++ " -i" ++ kics2cIncludes
          ]
      }

    let bootDir = optBootDir o
        compileBootHsSrc = bootDir </> "CompileBoot.hs"

    build $ ([stageBin i] :. ("ghc", [compileBootHsSrc]) |. hsSrcs)
      { buildDescription = Just $ description ++ " (Haskell -> Native)"
      , buildVariables =
          [ "ghc_opts" =. "$ghc_opts --make -i" ++ ghcIncludes
          ]
      }
