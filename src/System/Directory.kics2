import System.Directory
import System.IO
import System.Time

external_d_C_prim_doesFileExist :: Curry_Prelude.C_String -> Cover -> ConstStore
                                -> Curry_Prelude.C_IO Curry_Prelude.C_Bool
external_d_C_prim_doesFileExist s _ _ = toCurry doesFileExist s

external_d_C_prim_doesDirectoryExist :: Curry_Prelude.C_String -> Cover -> ConstStore
                                     -> Curry_Prelude.C_IO Curry_Prelude.C_Bool
external_d_C_prim_doesDirectoryExist s _ _ = toCurry doesDirectoryExist s

external_d_C_prim_fileSize :: Curry_Prelude.C_String -> Cover -> ConstStore
                           -> Curry_Prelude.C_IO Curry_Prelude.C_Int
external_d_C_prim_fileSize s _ _ = toCurry
  (\f -> do h <- openFile f ReadMode
            i <- hFileSize h
            hClose h
            return i
  ) s

external_d_C_prim_getModificationTime :: Curry_Prelude.C_String -> Cover -> ConstStore
                                      -> Curry_Prelude.C_IO Curry_Time.C_ClockTime
external_d_C_prim_getModificationTime s _ _ = toCurry getModificationTime s

external_d_C_getCurrentDirectory :: Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_String)
external_d_C_getCurrentDirectory _ _ = toCurry getCurrentDirectory

external_d_C_prim_setCurrentDirectory :: Curry_Prelude.C_String -> Cover -> ConstStore
                                      -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
external_d_C_prim_setCurrentDirectory s _ _ = toCurry setCurrentDirectory s

external_d_C_prim_getDirectoryContents :: Curry_Prelude.C_String -> Cover -> ConstStore
                                       -> Curry_Prelude.C_IO (Curry_Prelude.OP_List (Curry_Prelude.C_String))
external_d_C_prim_getDirectoryContents s _ _ = toCurry getDirectoryContents s

external_d_C_prim_createDirectory :: Curry_Prelude.C_String -> Cover -> ConstStore
                                  -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
external_d_C_prim_createDirectory s _ _ = toCurry createDirectory s

external_d_C_prim_removeFile :: Curry_Prelude.C_String -> Cover -> ConstStore
                             -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
external_d_C_prim_removeFile s _ _ = toCurry removeFile s

external_d_C_prim_removeDirectory :: Curry_Prelude.C_String -> Cover -> ConstStore
                                  -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
external_d_C_prim_removeDirectory s _ _ = toCurry removeDirectory s

external_d_C_prim_renameFile :: Curry_Prelude.C_String -> Curry_Prelude.C_String -> Cover -> ConstStore
                             -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
external_d_C_prim_renameFile s1 s2 _ _ = toCurry renameFile s1 s2

external_d_C_prim_renameDirectory :: Curry_Prelude.C_String -> Curry_Prelude.C_String
                                  -> Cover -> ConstStore 
                                  -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
external_d_C_prim_renameDirectory s1 s2 _ _= toCurry renameDirectory s1 s2
