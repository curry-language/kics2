module KiCS2.InstallationPaths
  ( kics2HomeDir, ghcExec
  ) where

import System.Environment ( getEnv )

--- Installation directory of KiCS2.
kics2HomeDir :: IO String
kics2HomeDir = getEnv "KICS2HOME"

--- The GHC executable to be used by KiCS2.
ghcExec :: IO String
ghcExec = do
  k2home <- kics2HomeDir
  return $ unwords $
    [ "stack"
    , "--stack-yaml", k2home ++ "/stack.yaml"
    , "--stack-root", k2home ++ "/.stack"
    , "exec"
    , "--"
    , "ghc"
    ]
