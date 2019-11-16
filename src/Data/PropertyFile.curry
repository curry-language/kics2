------------------------------------------------------------------------------
--- A library to read and update files containing properties in the usual
--- equational syntax, i.e., a property is defined by a line of the form
--- `prop=value` where `prop` starts with a letter.
--- All other lines (e.g., blank lines or lines starting with `#` are
--- considered as comment lines and are ignored.
---
--- @author Michael Hanus
--- @version August 2006
--- @category general
------------------------------------------------------------------------------

module Data.PropertyFile
  ( readPropertyFile, updatePropertyFile
  , getPropertyFromFile, getPropertiesFromFile
  )
 where

import Data.Char
import System.Directory
import System.IOExts

------------------------------------------------------------------------------
--- Reads a property file and returns the list of properties.
--- Returns empty list if the property file does not exist.
readPropertyFile :: String -> IO [(String,String)]
readPropertyFile file = do
  pfexists <- doesFileExist file
  if pfexists
   then do rcs <- readCompleteFile file -- to avoid open file handles
           return $ splitEqs . filter (\l->not (null l) && isAlpha (head l))
                             . lines $ rcs
   else return []
 where
  splitEqs [] = []
  splitEqs (eq:eqs) = case break (=='=') eq of
     (prop,_:val) -> (prop,val) : splitEqs eqs
     _            -> splitEqs eqs


--- Update a property in a property file or add it, if it is not already
--- there.
--- @param file - the name of the property file
--- @param pname - the name of the property
--- @param pvalue - the new value of the property
updatePropertyFile :: String -> String -> String -> IO ()
updatePropertyFile file pname pval = do
  props <- readPropertyFile file
  if lookup pname props == Nothing
   then appendFile file (pname++"="++pval++"\n")
   else changePropertyInFile file pname pval

--- Change a property in a property file.
changePropertyInFile :: String -> String -> String -> IO ()
changePropertyInFile file pname pval = do
  updateFile (\rcs -> unlines . map changeProp . lines $ rcs) file
 where
  changeProp l = let (s1,s2) = break (=='=') l
                  in if null l || not (isAlpha (head l)) || null s2
                     then l
                     else if s1==pname then s1++"="++pval else l

------------------------------------------------------------------------------
--- Looks up the value of a property stored in a property file.
--- Uppercase/lowercase is ignored for the property names.
getPropertyFromFile :: String -> String -> IO (Maybe String)
getPropertyFromFile propfile propname = do
  props <- readPropertyFile propfile
  return $ lookup (map toLower propname)
                  (map (\ (a, b) -> (map toLower a, b)) props)

--- Looks up the values of properties stored in a property file.
--- Uppercase/lowercase is ignored for the variable names.
getPropertiesFromFile :: String -> [String] -> IO [Maybe String]
getPropertiesFromFile propfile propnames = do
  props <- readPropertyFile propfile
  return (map (flip lookup (map (\ (a, b) -> (map toLower a, b)) props))
              (map (map toLower) propnames))

------------------------------------------------------------------------------
