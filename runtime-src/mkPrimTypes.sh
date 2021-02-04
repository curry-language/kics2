#!/bin/bash

SRC=PrimTypes.curry
DEST=./.curry/kics2/Curry_PrimTypes.hs # FIXME: Use versioned kics2 folder

function replace ()
{
   cat $1 | sed -e "s/C_IntPrim/Int#/" \
  | sed "s/\(showsPrec d (C_Int x1) = \).*$/\1shows (I# x1)/" \
  | sed "s/\(showsPrec d (C_CurryInt x1) = \).*$/\1(\\x -> shows (I# (curryint2primint x))) \$!! x1/" \
  | sed "s/\(readsPrec d s = \).*C_Int.*$/\1map readInt (readsPrec d s) where readInt (I# i, s) = (C_Int i, s)/" \
  | sed 's/\(generate s = Choices_C_Int \).*$/\1 (freeID [1] s) [C_CurryInt (generate (leftSupply s))]/' \
  | sed "s/(\$!!) cont (C_Int x1).*$/(\$!!) cont x@(C_Int _) = cont x/" \
  | sed "s/(\$##) cont (C_Int x1).*$/(\$##) cont x@(C_Int _) = cont x/" \
  | sed "/(\$!<) cont (C_Int x1)/d" \
  | sed "s/\(searchNF search cont \)(C_Int x1) = .*$/\1x@(C_Int _) = cont x/" \
  | sed "s/(=\.=) (C_Int x1) (C_Int y1) =.*/(=.=) (C_Int      x1) (C_Int      y1) = if (x1 ==# y1) then C_Success else Fail_C_Success/" \
  | sed "s/\((=\.=) (C_CurryInt x1) (C_CurryInt y1).*\)$/\1\n  (=\.=) (C_Int x1) x2 = (int2integer x1) =\.= x2\n  (=\.=) x1 (C_Int x2) = x1 =\.= (int2integer x2)/" \
  | sed "/(=\.<=) (C_Int x1) (C_Int y1)/d" \
  | sed "s/\((=\.<=) (C_CurryInt x1) (C_CurryInt y1).*\)$/\1\n  (=\.<=) (C_Int x1) x2 = (int2integer x1) =\.<= x2\n  (=\.<=) x1 (C_Int x2) = x1 =\.<= (int2integer x2)/" \
  | sed "s/\(bind i (C_Int x2) = \).*$/\1bind i (int2integer x2)/" \
  | sed "s/\(lazyBind i (C_Int x2) = \).*$/\1lazyBind i (int2integer x2)/" \
  | sed "s/\((=?=) (C_Int x1) (C_Int y1) = \).*$/\1toCurry (x1 ==# y1)/" \
  | sed "s/\((<?=) (C_Int x1) (C_Int y1) = \).*$/\1toCurry (x1 <=# y1)/" \
  | sed "s/C_FloatPrim/Float#/" \
  | sed "s/\(showsPrec d (C_Float x1) = \).*$/\1shows (F# x1)/" \
  | sed "s/\(readsPrec d s = \).*C_Float.*$/\1map readFloat (readsPrec d s) where readFloat (F# f, s) = (C_Float f, s)/" \
  | sed 's/generate s = Choices_C_Float.*$/generate _ = error "No generator for C_Float"/' \
  | sed "s/(\$!!) cont (C_Float x1).*$/(\$!!) cont x@(C_Float _) = cont x/" \
  | sed "s/(\$##) cont (C_Float x1).*$/(\$##) cont x@(C_Float _) = cont x/" \
  | sed "/(\$!<) cont (C_Float x1)/d" \
  | sed "/(=\.=) (C_Float x1) (C_Float y1)/d" \
  | sed "/(=\.<=) (C_Float x1) (C_Float y1)/d" \
  | sed "/bind i (C_Float x2) =/d" \
  | sed "/lazyBind i (C_Float x2) =/d" \
  | sed "s/\((=?=) (C_Float x1) (C_Float y1) = \).*$/\1toCurry (x1 \`eqFloat#\` y1)/" \
  | sed "s/\((<?=) (C_Float x1) (C_Float y1) = \).*$/\1toCurry (x1 \`leFloat#\` y1)/" \
  | sed "s/C_CharPrim/Char#/" \
  | sed "s/\(showsPrec d (C_Char x1) = \).*$/\1showString (show (C# x1))\n\n  showList cs = showList (map (\\\(C_Char c) -> (C# c)) cs)/" \
  | sed "s/\(readsPrec d s = \).*C_Char.*$/\1map readChar (readsPrec d s) where readChar (C# c, s) = (C_Char c, s)\n\n  readList s = map readString (readList s) where readString (cs, s) = (map (\\\(C# c) -> C_Char c) cs, s)/" \
  | sed 's/generate s = Choices_C_Char.*$/generate _ = error "No generator for C_Char"/' \
  | sed "s/(\$!!) cont (C_Char x1).*$/(\$!!) cont x@(C_Char _) = cont x/" \
  | sed "s/(\$##) cont (C_Char x1).*$/(\$##) cont x@(C_Char _) = cont x/" \
  | sed "/(\$!<) cont (C_Char x1)/d" \
  | sed "/(=\.=) (C_Char x1) (C_Char y1)/d" \
  | sed "/(=\.<=) (C_Char x1) (C_Char y1)/d" \
  | sed "/bind i (C_Char x2) =/d" \
  | sed "/lazyBind i (C_Char x2) =/d" \
  | sed "s/\((=?=) (C_Char x1) (C_Char y1) = \).*$/\1toCurry (x1 \`eqChar#\` y1)/" \
  | sed "s/\((<?=) (C_Char x1) (C_Char y1) = \).*$/\1toCurry (x1 \`leChar#\` y1)/" \
  | sed "s/C_IOPrim/IO/" \
  | sed "/showsPrec d.*C_IO/d" \
  | sed 's/instance .* Show (C_IO t0) where.*$/instance Show (C_IO a) where show = error "ERROR: no show for C_IO"/' \
  | sed "/readsPrec d.*C_IO/d" \
  | sed 's/instance .* Read (C_IO t0) where.*$/instance Read (C_IO a) where readsPrec = error "readsPrec for C_IO"/' \
  | sed "/generate.*C_IO/d" \
  | sed 's/instance .* Generable (C_IO t0) where.*$/instance Generable (C_IO a) where generate _ = error "generate for C_IO"/' \
  | sed "s/(\$!!) cont (C_IO x1).*$/(\$!!) cont io@(C_IO _) = cont io/" \
  | sed "s/(\$##) cont (C_IO x1).*$/(\$##) cont io@(C_IO _) = cont io/" \
  | sed "/(\$!<) cont (C_IO x1)/d" \
  | sed "/(=\.=) (C_IO x1) (C_IO y1)/d" \
  | sed "/(=\.<=) (C_IO x1) (C_IO y1)/d" \
  | sed "/bind i (C_IO x2) =/d" \
  | sed "/lazyBind i (C_IO x2) =/d" \
  | sed "/(=?=).*C_IO/d" \
  | sed "/(<?=).*C_IO/d" \
  | sed "s/C_IDSupply/IDSupply/" \
  | sed "/showsPrec d.*C_Func/d" \
  | sed 's/instance .* Show (C_Func t0 t1) where.*$/instance Show (C_Func a b) where show = error "ERROR: no show for Func"/' \
  | sed "/readsPrec d.*C_Func/d" \
  | sed 's/instance .* Read (C_Func t0 t1) where.*$/instance Read (C_Func a b) where readsPrec = error "readsPrec for Func"/' \
  | sed "/generate .*C_Func/d" \
  | sed 's/instance .* Generable (C_Func t0 t1) where.*$/instance Generable (C_Func a b) where generate _ = error "generate for Func"/' \
  | sed "s/(\$!!) cont (C_Func x1).*$/(\$!!) cont f@(C_Func _) = cont f/" \
  | sed "s/(\$##) cont (C_Func x1).*$/(\$##) cont f@(C_Func _) = cont f/" \
  | sed "/(\$!<) cont (C_Func x1)/d" \
  | sed "/(=\.=) (C_Func x1) (C_Func y1)/d" \
  | sed "/(=\.<=) (C_Func x1) (C_Func y1)/d" \
  | sed "/bind i (C_Func x2) =/d" \
  | sed "/lazyBind i (C_Func x2) =/d" \
  | sed "/(=?=).*C_Func/d" \
  | sed "/(<?=).*C_Func/d" \
  | sed "s/C_Func/Func/g" \
  | sed 's/instance .* Show (C_PrimData t0) where.*$/instance Show (C_PrimData a) where show = error "ERROR: no show for PrimData"/' \
  | sed "/showsPrec d.*C_PrimData/d" \
  | sed "/readsPrec d.*C_PrimData/d" \
  | sed 's/instance .* Read (C_PrimData t0) where.*$/instance Read (C_PrimData a) where readsPrec = error "readsPrec for PrimData"/' \
  | sed "/generate .*C_PrimData/d" \
  | sed 's/instance .* Generable (C_PrimData t0) where.*$/instance Generable (C_PrimData a) where generate _ = error "generate for PrimData"/' \
  | sed 's/instance .* NormalForm (C_PrimData t0) where.*$/instance NormalForm (C_PrimData a) where/' \
  | sed "s/(\$!!) cont (C_PrimData x1).*$/(\$!!) cont p@(C_PrimData _) = cont p/" \
  | sed "s/(\$##) cont (C_PrimData x1).*$/(\$##) cont p@(C_PrimData _) = cont p/" \
  | sed "/(\$!<) cont (C_PrimData x1)/d" \
  | sed 's/instance .* Unifiable (C_PrimData t0) where.*$/instance Unifiable (C_PrimData a) where/' \
  | sed "/(=\.=) (C_PrimData x1) (C_PrimData y1)/d" \
  | sed "/(=\.<=) (C_PrimData x1) (C_PrimData y1)/d" \
  | sed "/bind i (C_PrimData x2) =/d" \
  | sed "/lazyBind i (C_PrimData x2) =/d" \
  | sed "/(=?=).*C_PrimData/d" \
  | sed "/(<?=).*C_PrimData/d" \
  | sed "s/C_PrimData/PrimData/g" \
  | sed "s/PrimTypes/Prelude/g" \
  | sed "s/_case_/_casePT_/g" \
  | sed '/^$/{N;/^\n$/D}' \
  > $1
}

rm -f $DEST
../bin/.local/kics2c -i../lib $SRC && replace $DEST
