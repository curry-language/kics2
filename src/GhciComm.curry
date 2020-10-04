--- --------------------------------------------------------------------------
--- Communication with the Glasgow Haskell Compiler (interactive)
---
--- @author  Michael Hanus, Bjoern Peemoeller
--- @version June 2012
--- --------------------------------------------------------------------------
module GhciComm
  ( GhciComm, initGhciComm, stopGhciComm, restartGhciComm
  , evalMainCmd, evalCustomCmd
  ) where

import Control.Applicative (when)
import System.IO     (Handle, hClose, hFlush, hGetLine, hPutStrLn)
import System.IOExts (connectToCommand)
import Data.Time     (calendarTimeToString, getLocalTime)

--- Information for communication with ghci
--- (ghc(i) command with arguments, handle, verbosity flag)
data GhciComm = GhciComm String Handle Bool

--- Initialize a new ghci communication
initGhciComm :: String -> Bool -> IO GhciComm
initGhciComm cmd verbose = do
  hdl <- connectToCommand cmd
  let comm = GhciComm cmd hdl verbose
  evalCustomCmd comm "putStrLn \"\""
  return comm

--- terminate a ghci communication
stopGhciComm :: GhciComm -> IO ()
stopGhciComm comm@(GhciComm _ hdl v) = do
  hPutStrLnGhci comm ":quit"
  when v (hGetLine hdl >>= putStrLn)
  hClose hdl

--- restart a ghci communication
restartGhciComm :: GhciComm -> String -> Bool -> IO GhciComm
restartGhciComm comm@(GhciComm cmd0 _ _) cmd v
  | cmd == cmd0 = hPutStrLnGhci comm ":reload" >> return comm
  | otherwise   = stopGhciComm comm >> initGhciComm cmd v

--- send "main" to ghci and print the results
evalMainCmd :: GhciComm -> Bool -> IO ()
evalMainCmd comm@(GhciComm _ hdl _) timings = do
  hPutStrLnGhci comm (if timings then ":set +s" else ":unset +s")
  evalCustomCmd comm "safeExec main"
  when timings (hGetLine hdl >>= putStrLn)

--- send an IO expression to ghci and print the stdout data from ghci
evalCustomCmd :: GhciComm -> String -> IO ()
evalCustomCmd comm@(GhciComm _ hdl _) cmd = do
  ctime <- getLocalTime
  let stopstring = "???" ++ reverse (calendarTimeToString ctime) ++ "==="
  hPutStrLnGhci comm $ cmd ++ " >> putStrLn \"" ++ stopstring ++ "\""
  hPrintLinesBefore stopstring
 where
  hPrintLinesBefore stop = do
    line <- hGetLine hdl
    -- TODO/STYLE: Use 'unless' again once re-added to 'Applicative'
    when (line /= stop) $ putStrLn line >> hPrintLinesBefore stop

--- Send a string to the ghci handle
hPutStrLnGhci :: GhciComm -> String -> IO ()
hPutStrLnGhci (GhciComm _ h v) s = do
  when v $ putStrLn $  "SEND TO GHCI> " ++ s
  hPutStrLn h s
  hFlush h
