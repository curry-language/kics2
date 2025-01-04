#!/bin/sh
# generate intermediate files of all libraries until everything is compiled

CURRY=kics2
FRONTEND=bin/$CURRY-frontend
FRONTENDPARAMS="-o .curry/kics2-$VERSION -D__KICS2__=$MAJORVERSION$(printf "%02d
" $MINORVERSION) --extended -Ono-remove-unused-imports -ilib AllLibraries"

compile_all() {
  $FRONTEND --flat                $FRONTENDPARAMS
  $FRONTEND --type-annotated-flat $FRONTENDPARAMS
  $FRONTEND --acy                 $FRONTENDPARAMS
  $FRONTEND --uacy                $FRONTENDPARAMS
}

TMPOUT=TMPLIBOUT
CCODE=0

while [ $CCODE = 0 ] ; do
  compile_all | tee $TMPOUT
  echo NEW COMPILED LIBRARIES IN THIS ITERATION:
  grep Compiling $TMPOUT
  CCODE=$?
done
/bin/rm -r $TMPOUT

echo "Compile Haskell targets..."
bin/$CURRY --nocypm :set parser -Ono-remove-unused-imports :l AllLibraries :eval "42::Int" :quit
