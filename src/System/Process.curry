module System.Process 
  ( getPID, system, exitWith, sleep
  ) where

import System.Environment

--- Returns the process identifier of the current Curry process.

getPID :: IO Int
getPID external

--- Executes a shell command and return with the exit code of the command.
--- An exit status of zero means successful execution.

system :: String -> IO Int
system cmd = do
  envs <- getEnvironment
  prim_system $## (concatMap envToExport envs ++ escapedCmd)
 where
  win       = isWindows
  -- This is a work around for GHC ticket #5376
  -- (http://hackage.haskell.org/trac/ghc/ticket/5376)
  escapedCmd = if win then '\"' : cmd ++ "\""
                      else cmd
  envToExport (var, val) =
    if win
    then "set " ++ var ++ "=" ++ concatMap escapeWinSpecials val ++ " && "
    else var ++ "='" ++ concatMap encodeShellSpecials val
         ++ "' ; export " ++ var ++ " ; "

  escapeWinSpecials c = if c `elem` "<>|&^" then ['^', c]
                                            else [c]
  encodeShellSpecials c = if c == '\'' then map chr [39,34,39,34,39]
                                       else [c]

prim_system :: String -> IO Int
prim_system external

getEnvironment :: IO [(String, String)]
getEnvironment external

--- Terminates the execution of the current Curry program
--- and returns the exit code given by the argument.
--- An exit code of zero means successful execution.

exitWith :: Int -> IO _
exitWith exitcode = prim_exitWith $# exitcode

prim_exitWith :: Int -> IO _
prim_exitWith external

--- The evaluation of the action (sleep n) puts the Curry process
--- asleep for n seconds.

sleep :: Int -> IO ()
sleep n = prim_sleep $# n

prim_sleep :: Int -> IO ()
prim_sleep external
