--- --------------------------------------------------------------------------
--- This is the main module of the interactive system.
--- It implements the Read-Eval-Print loop for KiCS2
---
--- @author Michael Hanus, Bjoern Peemoeller, Finn Teegen
--- @version August 2019
--- --------------------------------------------------------------------------
module REPL where

import AbstractCurry.Types hiding (preludeName)
import AbstractCurry.Files
import AbstractCurry.Select
import Language.Curry.Distribution ( baseVersion, installDir )
import System.Directory
import System.FilePath    ( (</>), (<.>)
                          , splitSearchPath, splitFileName, splitExtension
                          , searchPathSeparator, findFileWithSuffix)
import System.Environment (system, getArgs, getEnv, getPID, exitWith)
import System.IO
import System.IOExts
import Data.Char          (isAlpha, isAlphaNum, isDigit, isSpace, toLower)
import Data.List          (intercalate, intersperse, isPrefixOf, nub, sort)
import Data.Time
import Numeric            (readNat)
import ReadShowTerm       (readsTerm)

import Files                (removeFileIfExists)
import GhciComm             (stopGhciComm)
import qualified Installation as Inst
import Names                (funcInfoFile, moduleNameToPath)
import RCFile
import Utils                ( showMonoTypeExpr, showMonoQualTypeExpr
                            , notNull, strip )

import System.CurryPath     ( inCurrySubdir, lookupModuleSource, stripCurrySuffix
                            , sysLibPath )
import System.FrontendExec

import Linker

--- Result of compiling main goal
data GoalCompile
  = GoalError                             -- error occurred
  | GoalWithoutBindings CurryProg         -- goal does not contain free vars
  | GoalWithBindings CurryProg Int String -- number of vars / new goal
 deriving Eq

--- Show an error message
writeErrorMsg :: String -> IO ()
writeErrorMsg msg = putStrLn ("ERROR: " ++ msg)

-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  rcFileDefs <- readRC
  args       <- getArgs
  let (nodefargs,defargs) = extractRCArgs args
      (mainargs,rtargs)   = break (=="--") nodefargs
      rcDefs              = updateRCDefs rcFileDefs defargs
      furtherRcDefs       = filter (\da -> fst da `notElem` map fst rcFileDefs)
                                   defargs
      rst = initReplState
              { kics2Home = Inst.installDir
              , rcvars    = rcDefs
              , rtsArgs   = if null rtargs then "" else unwords (tail rtargs)
              }
  ipath  <- defaultImportPaths rst
  if null furtherRcDefs
   then processArgsAndStart
          rst { importPaths = ipath }
          (map strip (words (rcValue (rcvars rst) "defaultparams")) ++ mainargs)
   else putStrLn $ "Error: rc property name '" ++ fst (head furtherRcDefs) ++
                   "' not found in kics2rc file!"

--- The default import paths of KiCS2.
--- It consists of the path defined by the environment variable CURRYPATH,
--- and the "libraries" property defined in ~/.kics2rc
defaultImportPaths :: ReplState -> IO [String]
defaultImportPaths rst = do
  currypath <- getEnv "CURRYPATH"
  let rclibs = rcValue (rcvars rst) "libraries"
  return $ filter (/= ".") $ splitSearchPath currypath ++ splitSearchPath rclibs

defaultImportPathsWith :: ReplState -> String -> IO [String]
defaultImportPathsWith rst dirs = do
  defipath <- defaultImportPaths rst
  adirs    <- mapIO getAbsolutePath (splitSearchPath dirs)
  return (adirs ++ defipath)

processArgsAndStart :: ReplState -> [String] -> IO ()
processArgsAndStart rst []
  | quit rst  = cleanUpAndExitRepl rst
  | otherwise = do
      getBanner >>= writeVerboseInfo rst 1
      writeVerboseInfo rst 1
        "Type \":h\" for help  (contact: kics2@curry-lang.org)"
      repl rst
processArgsAndStart rst (arg:args)
  -- ignore empty arguments which can be provided by single or double quotes
  | null      arg = processArgsAndStart rst args
  -- ignore '--nocypm' or '--noreadline' (already processed by kics2 script)
  | arg == "--nocypm" || arg == "--noreadline"
  = processArgsAndStart rst args
  | arg == "-V" || arg == "--version"
  = getBanner >>= putStrLn  >> processArgsAndStart rst { quit = True} args
  | arg == "--compiler-name"
  = putStrLn "kics2"        >> processArgsAndStart rst { quit = True} args
  | arg == "--numeric-version"
  = putStrLn numericVersion >> processArgsAndStart rst { quit = True} args
  | arg == "--base-version"
  = putStrLn baseVersion    >> processArgsAndStart rst { quit = True} args
  | arg == "-h" || arg == "--help" || arg == "-?"
  = printHelp >> cleanUpAndExitRepl rst
  | isCommand arg = do
    let (cmdargs, more) = break isCommand args
    mbrst <- processCommand rst (tail (unwords (arg:cmdargs)))
    maybe printHelp (\rst' -> processArgsAndStart rst' more) mbrst
  | otherwise
  = writeErrorMsg ("unknown command: " ++ unwords (arg:args)) >> printHelp
 where
  printHelp = printHelpOnInteractive >> printHelpOnTools

--- May a `String` be a REPL command?
isCommand :: String -> Bool
isCommand s = case s of
  ':' : _ -> True
  _       -> False

printHelpOnInteractive :: IO ()
printHelpOnInteractive = putStrLn $ unlines
  [ "Invoke interactive environment:"
  , ""
  , "    kics2 <options> <commands>"
  , ""
  , "with options:"
  , ""
  , "-h|--help|-?      : show this message and quit"
  , "-V|--version      : show version and quit"
  , "--compiler-name   : show the compiler name `kics2' and quit"
  , "--numeric-version : show the compiler version number and quit"
  , "--base-version    : show the version of the base libraries and quit"
  , "--nocypm          : do not invoke `cypm' to compute package load path"
  , "--noreadline      : do not use input line editing via command `rlwrap'"
  , "-Dprop=val        : define kics2rc property `prop' as `val'"
  , "<commands>        : list of commands of the KiCS2 environment"
  , "                    (run `kics2 :h :q' to see the list of all commands)"
  , ""
  ]

printHelpOnTools :: IO ()
printHelpOnTools = putStrLn $ unlines
  [ "Invoke some tool:"
  , ""
  , "    kics2 <tool> <tool specific options>"
  , ""
  , "where <tool> is one of:"
  , ""
  , "cypm     : Curry package manager"
  , "frontend : Curry front end"
  , ""
  , "To get more help about the usage of a tool, type"
  , ""
  , "    kics2 <tool> -h"
  ]

--- Retrieve the KiCS2 banner
getBanner :: IO String
getBanner = do
  logo <- readCompleteFile $ Inst.installDir </> "include" </> "logo" <.> "txt"
  return (logo ++ version)
 where
  version =
    "Version " ++ numericVersion ++
    (if Inst.buildVersion == 0 then "" else "-b" ++ show Inst.buildVersion) ++
    " of " ++ Inst.compilerDate ++
    " (installed at " ++ Inst.installDate ++ ")"

--- Show numeric version number (without build version)
numericVersion :: String
numericVersion =
  intercalate "."
    (map show [Inst.majorVersion, Inst.minorVersion, Inst.revisionVersion])

-- ---------------------------------------------------------------------------

-- The main read-eval-print loop:
repl :: ReplState -> IO ()
repl rst = do
  putStr (calcPrompt rst) >> hFlush stdout
  eof <- isEOF
  if eof
    then cleanUpAndExitRepl rst
    else do getLine >>= processInput rst . strip

calcPrompt :: ReplState -> String
calcPrompt rst = subst (prompt rst)
 where
  loaded = unwords (mainMod rst : addMods rst)
  subst []       = []
  subst [c]      = [c]
  subst (c:d:cs) = case c of
    '%' -> case d of
      '%' -> '%' : cs
      's' -> loaded ++ subst cs
      _   -> c : d : subst cs
    _   -> c : subst (d:cs)

-- Clean resources of REPL and terminate it with exit status.
cleanUpAndExitRepl :: ReplState -> IO ()
cleanUpAndExitRepl rst = do
  terminateSourceProgGUIs rst
  exitWith (exitStatus rst)

processInput :: ReplState -> String -> IO ()
processInput rst g
  | null g      = repl rst
  | isCommand g = do mbrst <- processCommand rst (strip (tail g))
                     maybe (repl (setExitStatus 1 rst))
                           (\rst' -> if (quit rst') then cleanUpAndExitRepl rst'
                                                    else repl rst')
                           mbrst
  | otherwise   = evalExpression rst g >>= repl

--- Evaluate an expression w.r.t. currently loaded modules
evalExpression :: ReplState -> String -> IO ReplState
evalExpression rst expr = do
  (rst', status) <- compileProgramWithGoal rst (not (useGhci rst)) expr
  rst'' <-
    if status==MainError || (useGhci rst' && not (interactive rst'))
    then return rst'
    else execMain rst' status expr
  cleanMainGoalFile rst''
  return rst''

-- Check whether the main module import the module "Unsafe".
importUnsafeModule :: ReplState -> IO Bool
importUnsafeModule rst =
  if "Unsafe" `elem` (addMods rst)
  then return True
  else do
    let acyMainModFile = abstractCurryFileName (mainMod rst)
        frontendParams = currentFrontendParams rst
    catch (callFrontendWithParams ACY frontendParams (mainMod rst) >>
           readAbstractCurryFile acyMainModFile >>= \p ->
           return ("Unsafe" `elem` imports p))
          (\_ -> return (mainMod rst /= "Prelude")) -- just to be safe

-- Compute the front-end parameters for the current state:
currentFrontendParams :: ReplState -> FrontendParams
currentFrontendParams rst =
    setQuiet       True
  $ setFullPath    (loadPaths rst)
  $ setExtended    (rcValue (rcvars rst) "curryextensions" /= "no")
  $ setOverlapWarn (rcValue (rcvars rst) "warnoverlapping" /= "no")
  $ setSpecials    (parseOpts rst)
    defaultParams

-- ---------------------------------------------------------------------------
-- Main goal file stuff
-- ---------------------------------------------------------------------------

writeSimpleMainGoalFile :: ReplState -> String -> IO ()
writeSimpleMainGoalFile rst goal = writeMainGoalFile rst [] Nothing goal

-- write the file with the main goal where necessary imports
-- and possibly a type string is provided:
writeMainGoalFile :: ReplState -> [String] -> Maybe String -> String -> IO ()
writeMainGoalFile rst imports mtype goal = writeFile mainGoalFile $
  unlines $ [noMissingSigs]
         ++ map ("import " ++) allImports
         ++ maybe [] (\ts -> ["kics2MainGoal :: " ++ ts]) mtype
         ++ ["kics2MainGoal = " ++ goal]
  where allImports    = nub $ mainMod rst : addMods rst ++ imports
        noMissingSigs = "{-# OPTIONS_CYMAKE -W no-missing-signatures #-}"

-- Remove mainGoalFile and auxiliaries
cleanMainGoalFile :: ReplState -> IO ()
cleanMainGoalFile rst = unless keepfiles $ do
  system $ Inst.installDir </>  "bin" </> "cleancurry " ++ mainGoalFile
  removeFileIfExists mainGoalFile
 where keepfiles = rcValue (rcvars rst) "keepfiles" == "yes"

-- Generate, read, and delete .acy file of main goal file.
-- Return Nothing if some error occurred during parsing.
getAcyOfMainGoal :: ReplState -> IO (Maybe CurryProg)
getAcyOfMainGoal rst = do
  let mainGoalProg    = stripCurrySuffix mainGoalFile
      acyMainGoalFile = --abstractCurryFileName mainGoalProg
                         inCurrySubdir (stripCurrySuffix mainGoalProg) ++ ".acy"
      frontendParams  = currentFrontendParams rst
  prog <- catch (callFrontendWithParams ACY frontendParams mainGoalProg >>
                 tryReadACYFile acyMainGoalFile)
                (\_ -> return Nothing)
  removeFileIfExists acyMainGoalFile
  return prog
--   acyExists <- doesFileExist acyMainGoalFile
--   if not acyExists
--     then return Nothing
--     else do
--       acySize <- fileSize acyMainGoalFile
--       if acySize == 0
--         then return Nothing
--         else do
--           prog <- tryReadACYFile acyMainGoalFile
--           removeFile acyMainGoalFile
--           return prog

getAcyOfExpr :: ReplState -> String -> IO (Maybe CurryProg)
getAcyOfExpr rst expr = do
  writeSimpleMainGoalFile rst expr
  mbProg <- getAcyOfMainGoal rst
  removeFileIfExists mainGoalFile
  return mbProg

-- Show the type of goal w.r.t. main program:
showTypeOfGoal :: ReplState -> String -> IO Bool
showTypeOfGoal rst goal = do
  mbProg <- getAcyOfExpr rst goal
  maybe (return False)
        (\ (CurryProg _ _ _ _ _ _ [CFunc _ _ _ qty _] _) -> do
          putStrLn $ goal ++ " :: " ++ showMonoQualTypeExpr False qty
          return True)
        mbProg

-- Get the module of a function visible in the main program:
getModuleOfFunction :: ReplState -> String -> IO String
getModuleOfFunction rst funname = do
  mbprog <- getAcyOfExpr rst $
    if isAlpha (head funname) then funname else '(' : funname ++ ")"
  return $ maybe ""
                 (\ (CurryProg _ _ _ _  _ _ [CFunc _ _ _ _ mainrules] _) ->
                    modOfMain mainrules)
                 mbprog
 where modOfMain r = case r of
        [CRule [] (CSimpleRhs (CSymbol (m, _)) [])]       -> m
        [CRule [] (CGuardedRhs [(_, CSymbol (m, _))] [])] -> m
        _                                                 -> ""

-- Compile main program with goal:
compileProgramWithGoal :: ReplState -> Bool -> String
                       -> IO (ReplState, MainCompile)
compileProgramWithGoal rst createExecutable goal =
  if (safeExec rst)
  then do -- check for imports of Unsafe
    unsafeused <- importUnsafeModule rst
    if unsafeused
      then do writeErrorMsg "Import of 'Unsafe' not allowed in safe mode!"
              return (setExitStatus 1 rst, MainError)
      else compileProgGoal
  else compileProgGoal
 where
  compileProgGoal = do
    let infoFile = funcInfoFile (outputSubdir rst) mainModuleIdent mainGoalFile
    removeFileIfExists infoFile
    removeFileIfExists $ abstractCurryFileName mainGoalFile
    writeSimpleMainGoalFile rst goal
    goalstate <- getAcyOfMainGoal rst >>= insertFreeVarsInMainGoal rst goal
    if goalstate == GoalError
      then return (setExitStatus 1 rst, MainError)
      else do
        let (newprog, newgoal, bindings) =
              case goalstate of
                GoalWithBindings p n g -> (p, g   , Just n )
                GoalWithoutBindings p  -> (p, goal, Nothing)
                _                      -> error "REPL.compileProgramWithGoal"
        typeok <- makeMainGoalMonomorphic rst newprog newgoal
        if typeok
          then do
            status <- compileCurryProgram rst mainGoalFile
            exinfo <- doesFileExist infoFile
            if status == 0 && exinfo
              then createAndCompileMain rst createExecutable goal bindings
              else return (setExitStatus 1 rst, MainError)
          else return (setExitStatus 1 rst, MainError)

-- Insert free variables occurring in the main goal as components
-- of the main goal so that their bindings are shown
-- The status of the main goal is returned.
insertFreeVarsInMainGoal :: ReplState -> String -> Maybe CurryProg
                         -> IO GoalCompile
insertFreeVarsInMainGoal _   _    Nothing     = return GoalError
insertFreeVarsInMainGoal rst goal (Just prog) = case prog of
  CurryProg _ _ _ _ _ _ [mfunc@(CFunc _ _ _ (CQualType _ ty) _)] _ -> do
    let freevars           = freeVarsInFuncRule mfunc
        (exp, whereclause) = breakWhereFreeClause goal
    if (safeExec rst) && isIOType ty
      then do writeErrorMsg "Operation not allowed in safe mode!"
              return GoalError
      else
        if null freevars
            || not (showBindings rst)
            || isPrtChoices (ndMode rst)
            || isIOType ty
            || length freevars > 10 -- due to limited size of tuples used
                                    -- in PrintBindings
            || null whereclause
          then return (GoalWithoutBindings prog)
          else do
            let newgoal = unwords $
                  ["(",exp,",["] ++
                  intersperse "," (map (\v-> "\"" ++ v ++ "\"") freevars) ++
                  ["]"] ++ map (\v->',':v) freevars ++ ")":[whereclause]
            writeVerboseInfo rst 2 $
              "Adding printing of bindings for free variables: " ++
                intercalate "," freevars
            writeSimpleMainGoalFile rst newgoal
            mbprog <- getAcyOfMainGoal rst
            return (maybe GoalError
                          (\p -> GoalWithBindings p (length freevars) newgoal)
                          mbprog)
  _ -> error "REPL.insertFreeVarsInMainGoal"
 where
  isPrtChoices c = case c of
    PrtChoices _ -> True
    _            -> False
  freeVarsInFuncRule f = case f of
    CFunc _ _ _ _ (CRule _ rhs : _) -> freeVarsInRhs rhs
    _ -> error "REPL.insertFreeVarsInMainGoal.freeVarsInFuncRule"

  freeVarsInRhs rhs = case rhs of
    CSimpleRhs  _ ldecls -> concatMap lvarName ldecls
    CGuardedRhs _ ldecls -> concatMap lvarName ldecls

  lvarName ldecl = case ldecl of CLocalVars vs -> map snd vs
                                 _             -> []

  -- Breaks a main expression into an expression and a where...free clause.
  -- If the where clause is not present, this part is empty.
  breakWhereFreeClause mainexp =
    let revmainexp = reverse mainexp
     in if take 4 revmainexp == "eerf"
        then let woWhere = findWhere (drop 4 revmainexp)
              in if null woWhere
                 then (mainexp,"")
                 else (reverse woWhere, drop (length woWhere) mainexp)
        else (mainexp,"")
   where
    findWhere [] = []
    findWhere (c:cs) | isSpace c && take 6 cs == "erehw " = drop 6 cs
                     | otherwise                          = findWhere cs

--- If the main goal is polymorphic, make it monomorphic by adding a type
--- declaration where type variables are replaced by type "()". Before,
--- type variables with a numeric constraint like "Num"/"Integral" or
--- "Fractional" are defaulted to the types "Int" or "Float", respectively.
--- The type of the main goal is only allowed to contain numeric constraints.
--- If the main goal has type "IO t" where t is monomorphic, t /= (),
--- and t is not a function, then ">>= print" is added to the goal.
--- The result is False if the main goal contains some error.
makeMainGoalMonomorphic :: ReplState -> CurryProg -> String -> IO Bool
makeMainGoalMonomorphic rst prog goal = case prog of
  CurryProg _ _ _ _ _ _ [CFunc _ _ _ qty _] _ ->
    makeMainGoalMonomorphic' rst qty goal
  _ -> error "REPL.makeMainGoalMonomorphic"

makeMainGoalMonomorphic' :: ReplState -> CQualTypeExpr -> String -> IO Bool
makeMainGoalMonomorphic' rst qty@(CQualType _ ty) goal
  | isFunctionalType ty = do
    writeErrorMsg "expression is of functional type"
    return False
  | isPolyType ty = case defaultQualTypeExpr qty of
    CQualType (CContext []) defTy -> do
      when (defTy /= ty) $ writeVerboseInfo rst 2 $
        "Defaulted type of main expression: " ++ showMonoTypeExpr False defTy
      writeMainGoalFile rst (modsOfType defTy)
                        (Just $ showMonoTypeExpr True defTy) goal
      when (isPolyType defTy) $ writeVerboseInfo rst 2 $
        "Type of main expression \"" ++ showMonoTypeExpr False defTy
        ++ "\" made monomorphic by replacing type variables by \"()\""
      return True
    _ -> do
      writeErrorMsg "cannot handle arbitrary overloaded top-level expressions"
      return False
  | otherwise = do
    unless (newgoal == goal) $ writeSimpleMainGoalFile rst newgoal
    return True
 where newgoal = if isIOReturnType ty
                 then '(' : goal ++ ") Prelude.>>= Prelude.print"
                 else goal

-- Defaults type variables with a numeric constraint like "Num"/"Integeral" or
-- "Fractional" to the types "Int" or "Float", respectively. Moreover,
-- existing "Eq", "Ord", "Read", and "Show" constraints for the same
-- type variable are removed.
defaultQualTypeExpr :: CQualTypeExpr -> CQualTypeExpr
defaultQualTypeExpr (CQualType (CContext cs) ty) =
  defaultQualTypeExpr' cs (CQualType (CContext []) ty)

defaultQualTypeExpr' :: [CConstraint] -> CQualTypeExpr -> CQualTypeExpr
defaultQualTypeExpr' [] qty = qty
defaultQualTypeExpr' (c:cs) (CQualType (CContext cs2) ty) = case c of
  (("Prelude", ptype), CTVar tv) ->
    if ptype `elem` ["Num","Integral","Fractional"]
      then let defptype = if ptype == "Fractional" then "Float" else "Int"
           in defaultQualTypeExpr'
                (removeConstraints tv defptype cs)
                (CQualType (CContext (removeConstraints tv defptype cs2))
                   (substTypeVar tv (CTCons ("Prelude", defptype)) ty))
      else defaultQualTypeExpr' cs (CQualType (CContext (cs2 ++ [c])) ty)
  _ -> defaultQualTypeExpr' cs (CQualType (CContext (cs2 ++ [c])) ty)
 where
  removeConstraints _  _        []       = []
  removeConstraints tv dflttype (c3:cs3) = case c3 of
    (("Prelude", cls), CTVar tv2)
      | tv == tv2 && cls `elem` ["Eq", "Ord", "Read", "Show"]
      -> removeConstraints tv dflttype cs3
      | tv == tv2 && dflttype == "Int" && cls == "Enum"
      -> removeConstraints tv dflttype cs3
    _ -> c3 : removeConstraints tv dflttype cs3

-- Replaces a type variable with a type expression.
substTypeVar :: CTVarIName -> CTypeExpr -> CTypeExpr -> CTypeExpr
substTypeVar tv def te@(CTVar      tv2) = if tv == tv2 then def else te
substTypeVar _  _   te@(CTCons       _) = te
substTypeVar tv def (CFuncType te1 te2) =
  CFuncType (substTypeVar tv def te1) (substTypeVar tv def te2)
substTypeVar tv def (CTApply   te1 te2) =
  CTApply (substTypeVar tv def te1) (substTypeVar tv def te2)

-- Compile a Curry program with kics2 compiler:
compileCurryProgram :: ReplState -> String -> IO Int
compileCurryProgram rst curryprog = do
  timekics2Cmd <- getTimeCmd rst "KiCS2 compilation" kics2Cmd
  writeVerboseInfo rst 2 $ "Executing: " ++ timekics2Cmd
  system timekics2Cmd
 where
  kics2Bin  = kics2Home rst </> "bin" </> ".local" </> "kics2c"
  kics2Opts = unwords $
    -- pass current value of "bindingoptimization" property to compiler:
    [ "-Dbindingoptimization=" ++ rcValue (rcvars rst) "bindingoptimization"
    , "-v" ++ show (transVerbose (verbose rst))
    , "-i" ++ intercalate [searchPathSeparator] (loadPaths rst)
    ] ++
    (if null (parseOpts rst)
    then []
    else ["--parse-options=\"" ++ parseOpts rst ++ "\""])
      ++ (if traceFailure rst then ["--trace-failure"] else [])
  kics2Cmd  = unwords [kics2Bin, kics2Opts, cmpOpts rst, curryprog]
  transVerbose n | n == 3    = 2
                 | n >= 4    = 3
                 | otherwise = n

--- Execute main program and show run time:
execMain :: ReplState -> MainCompile -> String -> IO ReplState
execMain rst _ mainexp = do -- _ was cmpstatus
  let mainCall  = unwords [mainCmd, rtsArgs rst]
      mainCmd   = "." </> outputSubdir rst </> "Main"
  timecmd <- getTimeCmd rst "Execution" mainCall
  writeVerboseInfo rst 1 $ "Evaluating expression: " ++ strip mainexp
  writeVerboseInfo rst 3 $ "Executing: " ++ timecmd
  cmdstatus <- system timecmd
  unless (cmdstatus == 0) $
    putStrLn ("Evaluation terminated with non-zero status " ++ show cmdstatus)
  return (setExitStatus cmdstatus rst)

-- ---------------------------------------------------------------------------
-- Processing of REPL commands
-- ---------------------------------------------------------------------------

-- Process a command of the REPL.
-- The result is either just a new ReplState or Nothing if an error occurred.
processCommand :: ReplState -> String -> IO (Maybe ReplState)
processCommand rst cmds
  | null cmds        = skipCommand "unknown command"
  | head cmds == '!' = unsafeExec rst $ processSysCall rst (strip $ tail cmds)
  | otherwise        = case matchedCmds of
      []            -> skipCommand $ "unknown command: ':" ++ cmds ++ "'"
      [(fcmd, act)] -> if fcmd `elem` ["eval","load","quit","reload"]
                       then act rst (strip args)
                       else unsafeExec rst $ act rst (strip args)
      (_:_:_)       -> skipCommand $ "ambiguous command: ':" ++ cmds ++ "'"
 where (cmd, args) = break (==' ') cmds
       matchedCmds = filter (isPrefixOf (map toLower cmd) . fst) replCommands

unsafeExec :: ReplState -> IO (Maybe ReplState) -> IO (Maybe ReplState)
unsafeExec rst act =
  if safeExec rst
  then skipCommand "Operation not allowed in safe mode!"
  else act

-- all available REPL commands
replCommands :: [(String, ReplState -> String -> IO (Maybe ReplState))]
replCommands =
  [ ("?"          , processHelp        )
  , ("add"        , processAdd         )
  , ("browse"     , processBrowse      )
  , ("cd"         , processCd          )
  , ("compile"    , processCompile     )
  , ("edit"       , processEdit        )
  , ("eval"       , processEval        )
  , ("fork"       , processFork        )
  , ("help"       , processHelp        )
  , ("interface"  , processInterface   )
  , ("load"       , processLoad        )
  , ("programs"   , processPrograms    )
  , ("reload"     , processReload      )
  , ("quit"       , processQuit        )
  , ("save"       , processSave        )
  , ("set"        , processSetOption   )
  , ("source"     , processSource      )
  , ("show"       , processShow        )
  , ("type"       , processType        )
  , ("usedimports", processUsedImports )
  ]

--- Skip an erroneous command with an error message
skipCommand :: String -> IO (Maybe ReplState)
skipCommand msg = writeErrorMsg msg >> return Nothing

--- Execute a call to a system command
processSysCall :: ReplState -> String -> IO (Maybe ReplState)
processSysCall rst cmd
  | null cmd  = skipCommand "missing system command"
  | otherwise = system cmd >> return (Just rst)

--- Process :quit command
processQuit :: ReplState -> String -> IO (Maybe ReplState)
processQuit rst _ = return (Just rst { quit = True })

--- Process :help command
processHelp :: ReplState -> String -> IO (Maybe ReplState)
processHelp rst _ = do
  printHelpOnCommands
  putStrLn "... or type any <expression> to evaluate\n"
  return (Just rst)

--- Process :compile command
processCompile :: ReplState -> String -> IO (Maybe ReplState)
processCompile rst args = processLoad rst args >>= maybe (return Nothing)
  (\rst' -> do
    rst'' <- compileModuleWithGHC rst' (mainMod rst')
    return (Just rst'')
  )

--- Process :load command
processLoad :: ReplState -> String -> IO (Maybe ReplState)
processLoad rst args = do
  rst' <- terminateSourceProgGUIs rst
  let dirmodname = stripCurrySuffix args
  if null dirmodname
    then skipCommand "missing module name"
    else do
    let (dirname, modname) = splitFileName dirmodname
    mbrst <- if (dirname == "./") then return (Just rst')
             else do putStrLn $ "Changing working directory to "++dirname
                     processCd rst' dirname
    maybe (return Nothing)
     (\rst2 ->
       lookupModuleSource (loadPaths rst2) modname >>=
       maybe (skipCommand $ "source file of module "++dirmodname++" not found")
             (\ (_,fn) ->
                 readAndProcessSourceFileOptions rst2 fn >>=
                 maybe (return Nothing)
                   (\rst3 -> compileCurryProgram rst3 modname >>
                   return (Just rst3 { mainMod = modname, addMods = [] }))
             ))
     mbrst

--- Process :reload command
processReload :: ReplState -> String -> IO (Maybe ReplState)
processReload rst args
  | mainMod rst == preludeName rst
  = skipCommand "no program loaded!"
  | null (stripCurrySuffix args)
  = compileCurryProgram rst (mainMod rst) >> return (Just rst)
  | otherwise
  = skipCommand "superfluous argument"

--- Process :add command
processAdd :: ReplState -> String -> IO (Maybe ReplState)
processAdd rst args
  | null args = skipCommand "Missing module name"
  | otherwise = Just `liftIO` foldIO add rst (words args)
  where
    add rst' m = let mdl = stripCurrySuffix m in
      if validModuleName mdl
      then do
        mbf <- findFileWithSuffix (moduleNameToPath mdl) [".curry", ".lcurry"]
                                  (loadPaths rst')
        case mbf of
          Nothing -> do
            writeErrorMsg $ "Source file of module '" ++ mdl ++ "' not found"
            return rst'
          Just _  -> return rst' { addMods = insert mdl (addMods rst') }
      else do writeErrorMsg $ "Illegal module name (ignored): " ++ mdl
              return rst'

    insert m []        = [m]
    insert m ms@(n:ns)
      | m < n     = m : ms
      | m == n    = ms
      | otherwise = n : insert m ns

--- Is a string a valid module name?
validModuleName :: String -> Bool
validModuleName = all (\c -> isAlphaNum c || c == '_' || c == '.')

--- Process expression evaluation
processEval :: ReplState -> String -> IO (Maybe ReplState)
processEval rst args = evalExpression rst args >>= return . Just

--- Process :type command
processType :: ReplState -> String -> IO (Maybe ReplState)
processType rst args = do
  typeok <- showTypeOfGoal rst args
  return (if typeok then Just rst else Nothing)

--- Process :cd command
processCd :: ReplState -> String -> IO (Maybe ReplState)
processCd rst args = do
  dirname <- getAbsolutePath args
  exists  <- doesDirectoryExist dirname
  if exists then setCurrentDirectory dirname >> return (Just rst)
            else skipCommand $ "directory does not exist"

--- Process :programs command
processPrograms :: ReplState -> String -> IO (Maybe ReplState)
processPrograms rst _ = printAllLoadPathPrograms rst >> return (Just rst)

--- Process :edit command
processEdit :: ReplState -> String -> IO (Maybe ReplState)
processEdit rst args = do
  modname <- getModuleName rst args
  mbf <- findFileWithSuffix (moduleNameToPath modname) [".curry", ".lcurry"]
                            (loadPaths rst)
  editenv <- getEnv "EDITOR"
  let editcmd  = rcValue (rcvars rst) "editcommand"
      editprog = if null editcmd then editenv else editcmd
  if null editenv && null editcmd
    then skipCommand "no editor defined"
    else maybe (skipCommand "source file not found")
          (\fn -> system (editprog++" "++fn++"& ") >> return (Just rst))
          mbf

--- Process :source command
processSource :: ReplState -> String -> IO (Maybe ReplState)
processSource rst args
  | null args   = skipCommand "missing function name"
  | null dotfun = do m <- getModuleOfFunction rst args
                     if null m
                       then skipCommand "function not found"
                       else showFunctionInModule rst m args
  | otherwise   = showFunctionInModule rst mod (tail dotfun)
  where (mod, dotfun) = break (== '.') args

--- Extract a module name, possibly prefixed by a path, from an argument,
--- or return the current module name if the argument is the empty string.
getModuleName :: ReplState -> String -> IO String
getModuleName rst args =
  if null args
  then return (mainMod rst)
  else let (dirname, mname) = splitFileName (stripCurrySuffix args)
        in if dirname == "./"
           then return mname
           else getAbsolutePath (stripCurrySuffix args)

--- Process :show command
processShow :: ReplState -> String -> IO (Maybe ReplState)
processShow rst args = do
  modname <- getModuleName rst args
  mbf <- findFileWithSuffix (moduleNameToPath modname) [".curry", ".lcurry"]
                            (loadPaths rst)
  case mbf of
    Nothing -> skipCommand "source file not found"
    Just fn -> do
      pager <- getEnv "PAGER"
      let rcshowcmd = rcValue (rcvars rst) "showcommand"
          showprog  = if not (null rcshowcmd)
                        then rcshowcmd
                        else (if null pager then "cat" else pager)
      system $ showprog ++ ' ' : fn
      putStrLn ""
      return (Just rst)

processInterface :: ReplState -> String -> IO (Maybe ReplState)
processInterface rst args = do
  modname <- getModuleName rst args
  checkAndCallCpmTool "curry-showflat" "showflatcurry"
    (\toolexec -> execCommandWithPath rst toolexec ["-int", modname])

processBrowse :: ReplState -> String -> IO (Maybe ReplState)
processBrowse rst args
  | notNull $ stripCurrySuffix args = skipCommand "superfluous argument"
  | otherwise                       = checkForWish $ do
      writeVerboseInfo rst 1 "Starting Curry Browser in separate window..."
      checkAndCallCpmTool "curry-browse" "currybrowse"
        (\toolexec -> execCommandWithPath rst toolexec [mainMod rst])

processUsedImports :: ReplState -> String -> IO (Maybe ReplState)
processUsedImports rst args = do
  let modname  = if null args then mainMod rst else stripCurrySuffix args
  checkAndCallCpmTool "curry-usedimports" "importusage"
    (\toolexec -> execCommandWithPath rst toolexec [modname])

processSave :: ReplState -> String -> IO (Maybe ReplState)
processSave rst args
  | mainMod rst == preludeName rst = skipCommand "no program loaded"
  | otherwise = do
    (rst', status) <- compileProgramWithGoal rst True
                      (if null args then "main" else args)
    unless (status == MainError) $ do
      renameFile ("." </> outputSubdir rst' </> "Main") (mainMod rst')
      writeVerboseInfo rst' 1 ("Executable saved in '" ++ mainMod rst' ++ "'")
    cleanMainGoalFile rst'
    return (Just rst')

processFork :: ReplState -> String -> IO (Maybe ReplState)
processFork rst args
  | mainMod rst == preludeName rst = skipCommand "no program loaded"
  | otherwise = do
    (rst', status) <- compileProgramWithGoal rst True
                      (if null args then "main" else args)
    unless (status == MainError) $ do
      pid <- getPID
      let execname = "." </> outputSubdir rst' </> "kics2fork" ++ show pid
      renameFile ("." </> outputSubdir rst' </> "Main") execname
      writeVerboseInfo rst' 3 ("Starting executable '" ++ execname ++ "'...")
      system ("( "++execname++" && rm -f "++execname++ ") "++
              "> /dev/null 2> /dev/null &") >> done
    cleanMainGoalFile rst'
    return (Just rst')

-- Process setting of an option
processSetOption :: ReplState -> String -> IO (Maybe ReplState)
processSetOption rst option
  | null (strip option) = printOptions rst >> return (Just rst)
  | otherwise           = case matched of
      []           -> skipCommand $ "unknown option: '" ++ option ++ "'"
      [(_,act)]    -> act rst (strip args)
      _            -> skipCommand $ "ambiguous option: ':" ++ option ++ "'"
 where (opt, args)  = break (==' ') option
       matched      = filter (isPrefixOf (map toLower opt) . fst)
                             (availOptions rst)

-- In a global installation, the option for setting the identifier supply
-- is not available:
availOptions :: ReplState
             -> [(String, ReplState -> String -> IO (Maybe ReplState))]
availOptions rst = filter installOpts replOptions
  where installOpts (opt, _) = localCompile rst
                               || opt `notElem` ["supply"]

replOptions :: [(String, ReplState -> String -> IO (Maybe ReplState))]
replOptions =
  [ ("paths"        , setOptionPath                                   )
  , ("bfs"          , \r _ -> return (Just r { ndMode       = BFS   }))
  , ("dfs"          , \r _ -> return (Just r { ndMode       = DFS   }))
  , ("prdfs"        , \r _ -> return (Just r { ndMode       = PrDFS }))
  , ("debugsearch"  , \r _ -> return (Just r { ndMode       = DEBUG }))
  , ("choices"      , setOptionNDMode PrtChoices 10                   )
  , ("ids"          , setOptionNDMode IDS        100                  )
  , ("parallel"     , setOptionNDMode Par        0                    )
  , ("supply"       , setOptionSupply                                 )
  , ("v0"           , \r _ -> return (Just r { verbose      = 0     }))
  , ("v1"           , \r _ -> return (Just r { verbose      = 1     }))
  , ("v2"           , \r _ -> return (Just r { verbose      = 2     }))
  , ("v3"           , \r _ -> return (Just r { verbose      = 3     }))
  , ("v4"           , \r _ -> return (Just r { verbose      = 4     }))
  , ("prompt"       , setPrompt                                       )
  , ("+interactive" , \r _ -> return (Just r { interactive  = True  }))
  , ("-interactive" , \r _ -> return (Just r { interactive  = False }))
  , ("+first"       , \r _ -> return (Just r { firstSol     = True  }))
  , ("-first"       , \r _ -> return (Just r { firstSol     = False }))
  , ("+optimize"    , \r _ -> return (Just r { optim        = True  }))
  , ("-optimize"    , \r _ -> return (Just r { optim        = False }))
  , ("+bindings"    , \r _ -> return (Just r { showBindings = True  }))
  , ("-bindings"    , \r _ -> return (Just r { showBindings = False }))
  , ("+time"        , \r _ -> return (Just r { showTime     = True  }))
  , ("-time"        , \r _ -> return (Just r { showTime     = False }))
  , ("+trace"       , \r _ -> return (Just r { traceFailure = True  }))
  , ("-trace"       , \r _ -> return (Just r { traceFailure = False }))
  , ("+profile"     , \r _ -> return (Just r { profile      = True  }))
  , ("-profile"     , \r _ -> return (Just r { profile      = False }))
  , ("+local"       , setLocalMode                                    )
  , ("-local"       , \r _ -> return (Just r { localCompile = False }))
  , ("+ghci"        , \r _ -> return (Just r { useGhci      = True  }))
  , ("-ghci"        , setNoGhci                                       )
  , ("safe"         , \r _ -> return (Just r { safeExec     = True  }))
  , ("prelude"      , \r a -> return (Just r { preludeName  = a     }))
  , ("parser"       , \r a -> return (Just r { parseOpts    = a     }))
  , ("cmp"          , \r a -> return (Just r { cmpOpts      = a     }))
  , ("ghc"          , \r a -> return (Just r { ghcOpts      = a     }))
  , ("rts"          , \r a -> return (Just r { rtsOpts      = a     }))
  , ("args"         , \r a -> return (Just r { rtsArgs      = a     }))
  ]

-- Try to set the local compilation mode.
-- If the lib directory is not writable, a warning is issued and the
-- local mode is not set.
setLocalMode :: ReplState -> String -> IO (Maybe ReplState)
setLocalMode rst _ = do
  pid <- getPID
  let libdir = installDir </> "lib"
      testfile = libdir </> "xxx" ++ show pid
  catch (writeFile testfile "" >> removeFile testfile)
        (\_-> do putStrLn $ "Warning: no write permission on `" ++ libdir ++ "'"
                 putStrLn "Local compilation mode may not work!")
  return $ Just rst { localCompile = True }

setPrompt :: ReplState -> String -> IO (Maybe ReplState)
setPrompt rst p
  | null rawPrompt = skipCommand "no prompt specified"
  | otherwise  = case head rawPrompt of
    '"' -> case readsTerm rawPrompt of
      [(strPrompt, [])] -> return (Just rst { prompt = strPrompt })
      _                 -> skipCommand "could not parse prompt"
    _   -> return (Just rst { prompt = rawPrompt })
 where rawPrompt = strip p

setNoGhci :: ReplState -> String -> IO (Maybe ReplState)
setNoGhci rst _ = do
  maybe done stopGhciComm (ghcicomm rst)
  return $ Just rst { useGhci = False, ghcicomm = Nothing }

setOptionPath :: ReplState -> String -> IO (Maybe ReplState)
setOptionPath rst args = do
  ipath <- if null args then defaultImportPaths rst
                        else defaultImportPathsWith rst args
  return (Just rst { importPaths = ipath })

setOptionNDMode :: (Int -> NonDetMode) -> Int
                -> ReplState -> String -> IO (Maybe ReplState)
setOptionNDMode mode defDepth rst args
  | null args = return (Just rst { ndMode = mode defDepth })
  | otherwise = case readNat args of
      Nothing    -> skipCommand "illegal number"
      Just (n,s) -> if null (strip s)
                      then return (Just rst { ndMode = mode n })
                      else skipCommand "illegal number"

setOptionSupply :: ReplState -> String -> IO (Maybe ReplState)
setOptionSupply rst args
  | args `elem` allSupplies = return (Just rst { idSupply = args })
  | otherwise               = skipCommand "unknown identifier supply"
 where allSupplies = ["integer", "ghc", "ioref", "pureio", "giants"]

printOptions :: ReplState -> IO ()
printOptions rst = putStrLn $ unlines $ filter notNull
  [ "Options for ':set' command:"
  , "path <paths>    - set additional search paths for imported modules"
  , "prdfs           - set search mode to primitive depth-first search"
  , "dfs             - set search mode to depth-first search"
  , "bfs             - set search mode to breadth-first search"
  , "ids [<n>]       - set search mode to iterative deepening (initial depth <n>)"
  , "parallel [<n>]  - set search mode to parallel search with <n> threads"
  , "choices [<n>]   - set search mode to print the choice structure as a tree"
  , "                  (up to level <n>)"
  , "debugsearch     - set search mode to print debugging information"
  , ifLocal rst
    "supply <I>      - set idsupply implementation (ghc|giants|integer|ioref|pureio)"
  , "v<n>            - verbosity level"
  , "                    0: quiet (errors and warnings only)"
  , "                    1: frontend status messages (default)"
  , "                    2: kics2c status messages"
  , "                    3: ghc status messages"
  , "                    4: analysis information"
  , "prompt <prompt> - set the user prompt"
  , "+/-interactive  - turn on/off interactive execution of main goal"
  , "+/-first        - turn on/off printing only first solution"
  , "+/-optimize     - turn on/off optimization"
  , "+/-bindings     - show bindings of free variables in initial goal"
  , "+/-time         - show compilation and execution time"
  , "+/-trace        - trace failure in deterministic expression"
  , ifProfiling
    "+/-profile      - compile with GHC's profiling capabilities"
  , "+/-local        - use local libraries instead of cabal packages"
  , "+/-ghci         - use ghci instead of ghc to evaluate main expression"
  , "safe            - safe execution mode without I/O actions"
  , "prelude <name>  - name of the standard prelude"
  , "parser  <opts>  - additional options passed to parser (front end)"
  , "cmp     <opts>  - additional options passed to KiCS2 compiler"
  , "ghc     <opts>  - additional options passed to GHC"
  , "rts     <opts>  - run-time options for ghc (+RTS <opts> -RTS)"
  , "args    <args>  - run-time arguments passed to main program"
  , showCurrentOptions rst
  ]

-- Show string if the local compilation/linking mode is used:
ifLocal :: ReplState -> String -> String
ifLocal rst s = if localCompile rst then s else ""

ifProfiling :: String -> String
ifProfiling s = if Inst.withProfiling then s else ""

showCurrentOptions :: ReplState -> String
showCurrentOptions rst = intercalate "\n" $ filter notNull
  [ "\nCurrent settings:"
  , "import paths      : " ++ intercalate [searchPathSeparator]
                                          ("." : importPaths rst)
  , "search mode       : " ++ case (ndMode rst) of
      PrDFS         -> "primitive non-monadic depth-first search"
      DEBUG         -> "debugging information for search"
      PrtChoices d  -> "show choice tree structure up to level " ++ show d
      DFS           -> "depth-first search"
      BFS           -> "breadth-first search"
      IDS d         -> "iterative deepening (initial depth: " ++ show d ++ ")"
      Par s         -> "parallel search with " ++ show s ++ " threads"
  , ifLocal rst $
    "idsupply          : " ++ idSupply rst
  , "prelude           : " ++ preludeName rst
  , "parser options    : " ++ parseOpts rst
  , "compiler options  : " ++ cmpOpts rst
  , "ghc options       : " ++ ghcOpts rst
  , "run-time options  : " ++ rtsOpts rst
  , "run-time arguments: " ++ rtsArgs rst
  , "verbosity         : " ++ show (verbose rst)
  , "prompt            : " ++ show (prompt rst)
  , unwords $ filter notNull
    [               showOnOff (interactive rst)  ++ "interactive"
    ,               showOnOff (firstSol rst)     ++ "first"
    ,               showOnOff (optim rst)        ++ "optimize"
    ,               showOnOff (showBindings rst) ++ "bindings"
    ,               showOnOff (showTime rst)     ++ "time"
    ,               showOnOff (traceFailure rst) ++ "trace"
    , ifProfiling $ showOnOff (profile rst)      ++ "profile"
    ,               showOnOff (localCompile rst) ++ "local"
    ,               showOnOff (useGhci rst)      ++ "ghci"
    ]
  ]
 where showOnOff b = if b then "+" else "-"

printHelpOnCommands :: IO ()
printHelpOnCommands = putStrLn $ unlines
  [ "Commands (can be abbreviated to a prefix if unique)"
  , ":load <prog>       - load program '<prog>.[l]curry' as main module"
  , ":add  <m1> .. <mn> - add modules '<m1>' to '<mn>' to currently loaded modules"
  , ":reload            - recompile currently loaded modules"
  , ":compile <prog>    - like ':load <prog>' but also compile Haskell code"
  , ":eval <expr>       - evaluate expression <expr>"
  , ":type <expr>       - show type of expression <expr>"
  , ":programs          - show names of all Curry programs available in load path"
  , ":cd <dir>          - change current directory to <dir>"
  , ":edit              - load source of currently loaded module into editor"
  , ":edit <m>          - load source of module <m> into editor"
  , ":show              - show currently loaded source program"
  , ":show <m>          - show source of module <m>"
  , ":source <f>        - show source of (visible!) function <f>"
  , ":source <m>.<f>    - show source of function <f> in module <m>"
  , ":browse            - browse program and its imported modules"
  , ":interface         - show interface of currently loaded module"
  , ":interface <m>     - show interface of module <m>"
  , ":usedimports       - show all used imported functions/constructors"
  , ":set <option>      - set an option"
  , ":set               - see help on options and current options"
  , ":save              - save executable with main expression 'main'"
  , ":save <expr>       - save executable with main expression <expr>"
  , ":fork <expr>       - fork new process evaluating <expr>"
  , ":help              - show this message"
  , ":!<command>        - execute <command> in shell"
  , ":quit              - leave the system"
  ]

--- Print all Curry programs in current load path
printAllLoadPathPrograms :: ReplState -> IO ()
printAllLoadPathPrograms rst = mapIO_ printDirPrograms (loadPaths rst)
 where
  printDirPrograms dir = do
    putStrLn $ "Curry programs in directory '" ++ dir ++ "':"
    progs <- getDirPrograms "" dir
    putStrLn $ unwords $ mergeSort $ progs
    putStrLn ""

  getDirPrograms prefix dir = do
    exdir <- doesDirectoryExist dir
    files <- if exdir then getDirectoryContents dir else return []
    subprogs <- mapIO (\d -> getDirPrograms (prefix ++ d ++ ".") (dir </> d))
                      (filter (\f -> let c = head f in c>='A' && c<='Z') files)
    return $ concat subprogs ++
      concatMap (\f -> let (base, sfx) = splitExtension f
                        in if sfx `elem` [".curry", ".lcurry"] && notNull base
                             then [prefix ++ base]
                             else [])
                files


-- Showing source code of functions via SourcProgGUI tool.
-- If necessary, open a new connection and remember it in the repl state.
showFunctionInModule :: ReplState -> String -> String -> IO (Maybe ReplState)
showFunctionInModule rst mod fun =
  checkForWish $
  checkAndCallCpmTool "curry-showsource" "sourceproggui" $ \spgui -> do
    writeVerboseInfo rst 1 $
      "Showing source code of function '" ++ mod ++ "." ++ fun ++
      "' in separate window..."
    let spguicmd = "CURRYPATH=" ++
                   intercalate [searchPathSeparator]
                               (importPaths rst ++ sysLibPath) ++
                   " && export CURRYPATH && " ++ spgui ++ " " ++ mod
    writeVerboseInfo rst 2 $ "Executing: " ++ spguicmd
    (rst',h') <- maybe (do h <- connectToCommand spguicmd
                           return (rst {sourceguis = (mod,(fun,h))
                                                     : sourceguis rst }, h))
                       (\ (f,h) -> do
                           hPutStrLn h ('-':f)
                           hFlush h
                           return (rst {sourceguis = updateFun (sourceguis rst)
                                                               mod fun }, h))
                       (lookup mod (sourceguis rst))
    hPutStrLn h' ('+':fun)
    hFlush h'
    return (Just rst')
 where
  updateFun []                _  _  = []
  updateFun ((m,(f,h)):sguis) md fn =
    if m==md then (m,(fn,h)):sguis
             else (m,(f,h)) : updateFun sguis md fn

-- Check whether some CPM tool is available, i.e., either in the current
-- path or in the CPM bin directory. If it is not available,
-- skip the command with an error message how to install the tool from
-- the package (specified in the second argument). Otherwise, continue with
-- the last argument by passing the name of the CPM tool.
checkAndCallCpmTool :: String -> String -> (String -> IO (Maybe ReplState))
                    -> IO (Maybe ReplState)
checkAndCallCpmTool tool package continue = do
  excmd <- system $ "which " ++ tool ++ " > /dev/null"
  if excmd == 0
    then continue tool
    else do homedir <- getHomeDirectory
            let cpmtoolfile = homedir </> ".cpm" </> "bin" </> tool
            excpm <- doesFileExist cpmtoolfile
            if excpm
              then continue cpmtoolfile
              else skipCommand errmsg
 where
  errmsg = "'" ++ tool ++ "' not found. Install it by: 'cypm install " ++
           package ++ "'!"

-- Execute some command (first argument) with some arguments (second argument).
-- The current load path is exported to the command via the environment
-- variable CURRYPATH.
execCommandWithPath :: ReplState -> String -> [String]
                    -> IO (Maybe ReplState)
execCommandWithPath rst cmd args = do
  let setpath = "CURRYPATH=" ++
                intercalate [searchPathSeparator]
                            (importPaths rst ++ sysLibPath) ++
                " && export CURRYPATH && "
      syscmd = setpath ++ cmd ++ ' ' : unwords args
  writeVerboseInfo rst 2 $ "Executing: " ++ syscmd
  system syscmd >> return (Just rst)

-- Check whether some system command is available. If it is not available,
-- skip the command with the given error message, otherwise continue with
-- the last argument.
checkForCommand :: String -> String -> IO (Maybe ReplState)
                -> IO (Maybe ReplState)
checkForCommand cmd errmsg continue = do
  excmd <- system $ "which " ++ cmd ++ " > /dev/null"
  if (excmd>0) then skipCommand errmsg
               else continue

-- Check whether the windowing shell "wish" is available.
checkForWish :: IO (Maybe ReplState) -> IO (Maybe ReplState)
checkForWish =
  checkForCommand "wish"
    "Windowing shell `wish' not found. Please install package `tk'!"

-- ---------------------------------------------------------------------------
-- Read KiCS2 options in a Curry source file
-- Source file options are comments of the form
-- {-# KiCS2_OPTION <opt> #-}
-- occurring before the module header where <opt> is an option
-- of KiCS2 (i.e., ":set <opt>" is a valid KiCS2 command).
-- These options are read and processed when a module is loaded (not reloaded!).

readAndProcessSourceFileOptions :: ReplState -> String -> IO (Maybe ReplState)
readAndProcessSourceFileOptions rst fname = do
  opts <- readSourceFileOptions fname
  unless (null opts) $ writeVerboseInfo rst 1 $
    "Source file options: " ++ intercalate " | " (map unwords opts)
  processSourceFileOptions rst opts

processSourceFileOptions :: ReplState -> [[String]] -> IO (Maybe ReplState)
processSourceFileOptions rst []     = return (Just rst)
processSourceFileOptions rst (o:os) =
  processSetOption rst (unwords o) >>=
  maybe (return Nothing) (\rst' -> processSourceFileOptions rst' os)

readSourceFileOptions :: String -> IO [[String]]
readSourceFileOptions fname = do
  h <- openFile fname ReadMode
  headers <- readHeaderLines h
  hClose h
  return (filter notNull (map getOptions (filter isOptionComment headers)))
 where
  isOptionComment s = take 3 s == "{-#" -- #-}

  getOptions s =
   let optwords = words (extractOptionString (drop 3 s))
    in if null optwords || map toLower (head optwords) /= "kics2_option"
       then []
       else tail optwords

  extractOptionString []     = ""
  extractOptionString (c:cs) = case cs of
    []  -> ""
    [_] -> ""
    _   -> if (c : take 2 cs) == "#-}" then "" else c : extractOptionString cs

  readHeaderLines h = do
    eof <- hIsEOF h
    if eof then return []
           else do line <- hGetLine h
                   if isModuleStart line
                     then return []
                     else do lines <- readHeaderLines h
                             return (strip line : lines)
   where isModuleStart l = take 6 l `elem` ["module", "import"] ||
                           (let (w, _) = break isSpace l
                             in notNull w && all isAlphaNum w)

-- Terminate all open SourceProgGUIs
terminateSourceProgGUIs :: ReplState -> IO ReplState
terminateSourceProgGUIs rst
  | null sguis = return rst
  | otherwise  = do
      writeVerboseInfo rst 1 "Terminating source program GUIs..."
      catch (mapIO_ (\ (_,(_,h)) -> hPutStrLn h "q" >> hFlush h >> hClose h) sguis)
            (\_ -> done)
      return rst { sourceguis = [] }
 where sguis = sourceguis rst
