------------------------------------------------------------------------------ 
--- Library for converting ground terms to strings and vice versa. 
--- 
--- @author Michael Hanus 
--- @version April 2005 
--- @category general 
------------------------------------------------------------------------------ 
 
module ReadShowTerm(showTerm,showQTerm,readQTerm,readsQTerm, 
                    readsUnqualifiedTerm,readUnqualifiedTerm,readsTerm,readTerm, 
                    readQTermFile,readQTermListFile, 
                    writeQTermFile,writeQTermListFile) where 
 
import Data.Char(isSpace) 
 
--- Transforms a ground(!) term into a string representation 
--- in standard prefix notation. 
--- Thus, showTerm suspends until its argument is ground. 
--- This function is similar to the prelude function <code>show</code> 
--- but can read the string back with <code>readUnqualifiedTerm</code> 
--- (provided that the constructor names are unique without the module 
--- qualifier). 
showTerm :: _ -> String 
showTerm x = prim_showTerm $## x 
 
prim_showTerm :: _ -> String 
prim_showTerm external 
 
--- Transforms a ground(!) term into a string representation 
--- in standard prefix notation. 
--- Thus, showTerm suspends until its argument is ground. 
--- Note that this function differs from the prelude function <code>show</code> 
--- since it prefixes constructors with their module name 
--- in order to read them back with <code>readQTerm</code>. 
showQTerm :: _ -> String 
showQTerm x = prim_showQTerm $## x 
 
prim_showQTerm :: _ -> String 
prim_showQTerm external 
 
--- Transform a string containing a term in standard prefix notation 
--- without module qualifiers into the corresponding data term. 
--- The first argument is a non-empty list of module qualifiers that are tried to 
--- prefix the constructor in the string in order to get the qualified constructors 
--- (that must be defined in the current program!). 
--- In case of a successful parse, the result is a one element list 
--- containing a pair of the data term and the remaining unparsed string. 
 
readsUnqualifiedTerm :: [String] -> String -> [(_,String)] 
readsUnqualifiedTerm [] _ = 
  error "ReadShowTerm.readsUnqualifiedTerm: list of module prefixes is empty" 
readsUnqualifiedTerm (prefix:prefixes) s = 
  readsUnqualifiedTermWithPrefixes (prefix:prefixes) s 
 
readsUnqualifiedTermWithPrefixes :: [String] -> String -> [(_,String)] 
readsUnqualifiedTermWithPrefixes prefixes s = 
  (prim_readsUnqualifiedTerm $## prefixes) $## s 
 
prim_readsUnqualifiedTerm :: [String] -> String -> [(_,String)] 
prim_readsUnqualifiedTerm external 
 
--- Transforms a string containing a term in standard prefix notation 
--- without module qualifiers into the corresponding data term. 
--- The first argument is a non-empty list of module qualifiers that are tried to 
--- prefix the constructor in the string in order to get the qualified constructors 
--- (that must be defined in the current program!). 
--- 
--- Example: <code>readUnqualifiedTerm ["Prelude"] "Just 3"</code> 
--- evaluates to <code>(Just 3)</code> 
 
readUnqualifiedTerm :: [String] -> String -> _ 
readUnqualifiedTerm prefixes s = case result of 
  [(term,tail)] 
     -> if all isSpace tail then term 
        else error ("ReadShowTerm.readUnqualifiedTerm: no parse, unmatched string after term: "++tail) 
  [] ->  error "ReadShowTerm.readUnqualifiedTerm: no parse" 
  _  ->  error "ReadShowTerm.readUnqualifiedTerm: ambiguous parse" 
 where result = readsUnqualifiedTerm prefixes s 
 --- For backward compatibility. Should not be used since their use can be problematic 
--- in case of constructors with identical names in different modules. 
readsTerm :: String -> [(_,String)] 
readsTerm s = prim_readsUnqualifiedTerm [] $## s 
 
--- For backward compatibility. Should not be used since their use can be problematic 
--- in case of constructors with identical names in different modules. 
readTerm :: String -> _ 
readTerm s = case result of 
  [(term,tail)] 
     -> if all isSpace tail then term 
        else error ("ReadShowTerm.readTerm: no parse, unmatched string after term: "++tail) 
  [] ->  error "ReadShowTerm.readTerm: no parse" 
  _  ->  error "ReadShowTerm.readTerm: ambiguous parse" 
 where result = prim_readsUnqualifiedTerm [] $## s 
 
--- Transforms a string containing a term in standard prefix notation 
--- with qualified constructor names into the corresponding data term. 
--- In case of a successful parse, the result is a one element list 
--- containing a pair of the data term and the remaining unparsed string. 
 
readsQTerm :: String -> [(_,String)] 
readsQTerm s = prim_readsQTerm $## s 
 
prim_readsQTerm :: String -> [(_,String)] 
prim_readsQTerm external 
 
--- Transforms a string containing a term in standard prefix notation 
--- with qualified constructor names into the corresponding data term. 
 
readQTerm :: String -> _ 
readQTerm s = case result of 
  [(term,tail)] -> if all isSpace tail then term 
                   else error "ReadShowTerm.readQTerm: no parse" 
  [] ->  error "ReadShowTerm.readQTerm: no parse" 
  _  ->  error "ReadShowTerm.readQTerm: ambiguous parse" 
 where result = readsQTerm s 
 
--- Reads a file containing a string representation of a term 
--- in standard prefix notation and returns the corresponding data term. 
 
readQTermFile :: String -> IO _ 
readQTermFile file = readFile file >>= return . readQTerm 
 
--- Reads a file containing lines with string representations of terms 
--- of the same type and returns the corresponding list of data terms. 
 
readQTermListFile :: String -> IO [_] 
readQTermListFile file = readFile file >>= return . map readQTerm . lines 
 
--- Writes a ground term into a file in standard prefix notation. 
--- @param filename - The name of the file to be written. 
--- @param term - The term to be written to the file as a string. 
 
writeQTermFile :: String -> _ -> IO () 
writeQTermFile filename term = writeFile filename (showQTerm term) 
 
--- Writes a list of ground terms into a file. 
--- Each term is written into a separate line which might be useful 
--- to modify the file with a standard text editor. 
--- @param filename - The name of the file to be written. 
--- @param terms - The list of terms to be written to the file. 
 
writeQTermListFile :: String -> [_] -> IO () 
writeQTermListFile filename terms = 
    writeFile filename (unlines (map showQTerm terms)) 
 
