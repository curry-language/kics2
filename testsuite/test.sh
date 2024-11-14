#!/bin/sh
# Shell script to test the current set of examples

CURRYHOME=`pwd`/..
CURRYBIN=$CURRYHOME/bin
CURRYCHECK=`command -v curry-check`

if [ ! -x "$CURRYCHECK" ] ; then
  echo "Executable 'curry-check' is not installed! Skipping tests..."
  exit 0
fi

# test for basic language features
test_lang()
{
  cd LanguageTests && $CURRYCHECK Test*.curry && cd ..
}

# tests for type classes:
test_classes()
{
  cd TypeclassTests && $CURRYCHECK Test*.curry && cd ..
}

# test features of specific Curry systems:
if [ -x "$CURRYBIN/pakcs" ] ; then
    TESTPAKCS=
elif [ -x "$CURRYBIN/kics2" ] ; then
    TESTKICS2="TestPolySubExp TestUnification"
fi

test_systems()
{
  if [ -n "$TESTPAKCS" -o -n "$TESTKICS2" ] ; then
    cd SpecialTests && $CURRYCHECK $TESTPAKCS $TESTKICS2 && cd ..
  fi
}

# run all tests:
exec_all_tests()
{
  test_lang && test_classes && test_systems
}

VERBOSE=no
if [ "$1" = "-v" ] ; then
  VERBOSE=yes
fi

# use the right Curry system for the tests:
PATH=$CURRYBIN:$PATH
export PATH

# clean up before
/bin/rm -rf */.curry

LOGFILE=`pwd`/xxx$$

if [ $VERBOSE = yes ] ; then
  exec_all_tests
  if [ $? -gt 0 ] ; then exit 1 ; fi
else
  exec_all_tests > $LOGFILE 2>&1
  if [ $? -gt 0 ] ; then
    echo "ERROR occurred during testing with CurryCheck:"
    cat $LOGFILE
    /bin/rm -f $LOGFILE
    exit 1
  fi
  /bin/rm -f $LOGFILE
fi
