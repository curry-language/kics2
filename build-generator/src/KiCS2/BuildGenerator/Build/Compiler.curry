module KiCS2.BuildGenerator.Build.Compiler
  ( compilerNinja
  ) where

import KiCS2.BuildGenerator.Options ( Options (..) )
import Language.Ninja.Types
import Language.Ninja.Builder ( NinjaBuilder, build, rule )

-- | The Ninja source for building the compiler.
compilerNinja :: Options -> NinjaBuilder ()
compilerNinja o = do
  return ()
