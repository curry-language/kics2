module KiCS2.BuildGenerator.Build.Libraries
  ( librariesNinja
  ) where

import KiCS2.BuildGenerator.Names ( curryToHsFilePath, curryFilePathToMod )
import KiCS2.BuildGenerator.Options ( Options (..), optRootDir, optLibDir, optLibSrcDir, optKics2cBin, optKics2cBin )
import KiCS2.BuildGenerator.Utils ( findWithSuffix, forM_ )
import Language.Ninja.Builder ( NinjaBuilder, build, rule )
import Language.Ninja.Types
import System.FilePath ( (</>), (<.>), makeRelative )

-- | The Ninja source for building the libraries.
librariesNinja :: Options -> NinjaBuilder ()
librariesNinja o = do
  let libDir = optLibDir o
      libSrcDir = optLibSrcDir o
      kics2cBin = optKics2cBin o
  
  libRelSrcs <- (makeRelative libSrcDir <$>) <$> findWithSuffix ".curry" (libSrcDir </> "src")

  let libSrcs = (libDir </>) <$> libRelSrcs
      libMods = curryFilePathToMod <$> libSrcs
      libOutDir = libDir </> ".curry" </> "kics2-" ++ optVersion o
      libHsSrcs = curryToHsFilePath libOutDir <$> libSrcs
  
  forM_ (zip libRelSrcs libMods) $ \(relSrc, mod) ->
    build $ ([libDir </> relSrc] :. ("cp", [libSrcDir </> relSrc]))
      { buildDescription = Just $ "Copying libraries: " ++ mod
      }

  build $ (libHsSrcs :. ("kics2c", []) |. kics2cBin : libSrcs)
      { buildDescription = Just "Building libraries..."
      , buildVariables =
          [ "mods" =. unwords libMods
          , "kics2c" =. kics2cBin
          , "kics2c_opts" =. "$kics2c_opts -o" ++ libOutDir ++ " -i" ++ libDir
          ]
      }
