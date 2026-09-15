#!/bin/sh
# Generate intermediate files of all libraries until everything is compiled
#
# The following environment variables are used (from the main Makefile):
# CURRYSYSTEM
# LIBDIR
# VERSION
# MAJORVERSION
# MINORVERSION

# Hierarchical names of all system libraries:
LIBNAMES=`cd $LIBDIR && find * -name "*.curry"| sed 's|.curry$||' | sed 's|/|.|g'`
# put name of the Prelude at the front (to compile it first):
LIBNAMES="Prelude `echo $LIBNAMES | sed 's|Prelude||'`"

echo "Pre-compiling the following system libraries:"
echo $LIBNAMES

# name of Curry system in uppercase:
CURRYUPPER=$(echo "$CURRYSYSTEM" | tr '[:lower:]' '[:upper:]')

FRONTEND="bin/$CURRYSYSTEM-frontend"
FRONTENDPARAMS="-o .curry/$CURRYSYSTEM-$VERSION -D__"$CURRYUPPER"__=$MAJORVERSION$(printf "%02d" $MINORVERSION) -i$LIBDIR $LIBNAMES"

compile_all() {
  "$FRONTEND" --flat                       $FRONTENDPARAMS
  "$FRONTEND" --type-annotated-flat --flat $FRONTENDPARAMS
  "$FRONTEND" --acy                        $FRONTENDPARAMS
  "$FRONTEND" --uacy                       $FRONTENDPARAMS
  "$FRONTEND" --comments                   $FRONTENDPARAMS
  "$FRONTEND" --ast                        $FRONTENDPARAMS
  "$FRONTEND" --short-ast                  $FRONTENDPARAMS
}

TMPOUT=TMPLIBOUT
CCODE=0

while [ $CCODE = 0 ] ; do
  compile_all | tee "$TMPOUT"
  echo "NEW COMPILED LIBRARIES IN THIS ITERATION:"
  grep Compiling "$TMPOUT"
  CCODE=$?
done
/bin/rm -r "$TMPOUT"

echo "Compiling system libraries..."
for LIB in $LIBNAMES ; do
  bin/$CURRYSYSTEM --nocypm :set parser -Ono-remove-unused-imports :load $LIB :eval "42::Int" :quit
done

