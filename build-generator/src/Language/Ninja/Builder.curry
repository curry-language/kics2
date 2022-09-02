module Language.Ninja.Builder
  ( NinjaBuilder
  , build, rule
  , execNinjaBuilder
  ) where

import Control.Monad.Trans.Writer ( WriterT, tell, execWriterT )
import Language.Ninja.Types

type NinjaBuilder = WriterT Ninja IO

build :: Build -> NinjaBuilder ()
build b = tell $ mempty { ninjaBuilds = [b] }

rule :: Rule -> NinjaBuilder ()
rule r = tell $ mempty { ninjaRules = [r] }

execNinjaBuilder :: NinjaBuilder a -> IO Ninja
execNinjaBuilder = execWriterT
