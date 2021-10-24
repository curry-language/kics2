-- Main module for KiCS2 REPL kics2i

module Main where
import Basics
import KiCS2.Curry_REPL

-- TODO: Make the REPL fully deterministic.
--       One source of nondeterminism seems to be ReadShowTerm
--       (invoked from AbstractCurry.Files), whose functions
--       apparently when invoked always get classified as being
--       nondeterministic (due to the Data dictionary?)

main :: IO ()
main = evalIO nd_C_main
-- main = evalDIO d_C_main
