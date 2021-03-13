-- Main module for KiCS2 compiler kics2c

module Main where
import Basics
import KiCS2.Curry_Compile
main = evalDIO d_C_main
