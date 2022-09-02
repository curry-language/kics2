module Ninja.Builder
  ( NinjaBuilder (..)
  , build, rule
  , execNinjaBuilder
  ) where

import Ninja.Types

-- Effectively a (Writer Ninja), however declared separately to avoid a dependency on transformers
data NinjaBuilder a = NinjaBuilder (a, Ninja)

instance Functor NinjaBuilder where
  fmap f (NinjaBuilder (x, n)) = NinjaBuilder (f x, n)

instance Applicative NinjaBuilder where
  pure x = NinjaBuilder (x, mempty)
  NinjaBuilder (f, n1) <*> NinjaBuilder (x, n2) = NinjaBuilder (f x, n1 `mappend` n2)

instance Monad NinjaBuilder where
  return = pure
  NinjaBuilder (x, n1) >>= f = let NinjaBuilder (y, n2) = f x
                               in NinjaBuilder (y, n1 `mappend` n2)

build :: Build -> NinjaBuilder ()
build b = NinjaBuilder ((), mempty { ninjaBuilds = [b] })

rule :: Rule -> NinjaBuilder ()
rule r = NinjaBuilder ((), mempty { ninjaRules = [r] })

execNinjaBuilder :: NinjaBuilder a -> Ninja
execNinjaBuilder (NinjaBuilder (_, n)) = n
