module KiCS2.BuildGenerator.Build.Compiler
  ( compilerNinja
  ) where

import Data.List ( intercalate, splitOn )
import qualified Data.Map as M
import Data.Maybe ( mapMaybe, fromJust )
import KiCS2.BuildGenerator.Names ( curryToHsFilePath, curryFilePathToMod, curryToHsFileName, parentSrcDir )
import KiCS2.BuildGenerator.Imports ( readImports )
import KiCS2.BuildGenerator.Options ( Options (..), optRootDir, optPackageJson, optDotCpmDir
                                    , optKics2cBin, optKics2iBin, optLocalBinDir, optLibDir
                                    , optVersion, optSrcDir, optBootDir, optCurry
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

  kics2Srcs <- findWithSuffix ".curry" srcDir
  depSrcs <- concatMapM (findWithSuffix ".curry") depSrcDirs
  let srcs = kics2Srcs ++ depSrcs
      mods = curryFilePathToMod <$> srcs
      modMap = M.fromList $ zip mods srcs

  -- Compiling kics2c/i directly using $curry

  -- TODO: Custom monad with an Options reader + some queried state (e.g. the $curry version)

  -- FIXME: Instead of hardcoding paths we should query $curry for its version
  --        and, depending on whether it's kics2 or pakcs, use its hs/pl files as outputs.
  --        We currently assume to always be compiling with $curry = kics2.

  let outDir src = parentSrcDir src </> ".curry" </> "kics2-3.0.0"
      hsPath src = curryToHsFilePath (outDir src) src

  forM_ (zip mods srcs) $ \(mod, src) -> do
    imports <- readImports src
    let importSrcs = mapMaybe (flip M.lookup modMap) imports
        importHsPaths = hsPath <$> importSrcs

    build $ ([hsPath src] :. ("curry", []) |. src : importHsPaths)
      { buildVariables =
          [ "module" =. mod
          , "root" =. parentSrcDir src
          ]
      }

  forM_ bins $ \(main, description, bin) -> do
    let src = fromJust $ M.lookup main modMap

    build $ ([bin] :. ("curryexe", [pkgJson]) |. [hsPath src])
      { buildVariables =
          [ "main" =. main
          ]
      , buildDescription = Just $ "Building " ++ description ++ " (" ++ main ++ ")"
      }

  -- Bootstrapping

  {- TODO: Readd bootstrapping once we have fixed the individual builds

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
          [ "mods" =. compileMain
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
  -}
