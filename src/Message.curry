--- --------------------------------------------------------------------------
--- Output of compiler messages.
---
--- @author  Björn Peemöller
--- @version April 2014
--- --------------------------------------------------------------------------
module Message where

import Control.Monad (unless)
import System.IO     (hFlush, hPutStrLn, stderr, stdout)

import CompilerOpts

putErrLn :: String -> IO ()
putErrLn msg = hPutStrLn stderr ("Compilation Error: " ++ msg) >> hFlush stderr

showStatus :: Options -> String -> IO ()
showStatus opts msg = showLevel VerbStatus opts msg

showAnalysis :: Options -> String -> IO ()
showAnalysis opts msg = showLevel VerbAnalysis opts msg

showDetail :: Options -> String -> IO ()
showDetail opts msg = showLevel VerbDetails opts msg

showLevel :: Verbosity -> Options -> String -> IO ()
showLevel level opts msg = unless (optVerbosity opts < level)
                                  (putStrLn msg >> hFlush stdout)
