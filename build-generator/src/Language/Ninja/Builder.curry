module Language.Ninja.Builder
  ( NinjaBuilder
  , build, rule, var, comment, whitespace
  , execNinjaBuilder
  ) where

import Control.Monad.Trans.Writer ( WriterT, tell, execWriterT )
import Language.Ninja.Types

type NinjaBuilder = WriterT Ninja IO

build :: Build -> NinjaBuilder ()
build b = tell $ Ninja [BuildStmt b]

rule :: Rule -> NinjaBuilder ()
rule r = tell $ Ninja [RuleStmt r]

var :: Var String -> NinjaBuilder ()
var v = tell $ Ninja [VarStmt v]

comment :: String -> NinjaBuilder ()
comment c = tell $ Ninja [CommentStmt c]

whitespace :: NinjaBuilder ()
whitespace = tell $ Ninja [WhitespaceStmt]

execNinjaBuilder :: NinjaBuilder a -> IO Ninja
execNinjaBuilder = execWriterT
