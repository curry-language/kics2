#!/bin/sh

# Start interactive read-eval-print loop for KiCS2

KICS2INSTALLDIR=
# Define the main directory where KICS2 is installed:
if [ -d "$KICS2INSTALLDIR" ] ; then
  KICS2HOME=$KICS2INSTALLDIR
else
  KICS2HOME="$(dirname $(dirname $(realpath "$0")))"
fi
export KICS2HOME

# The bin directory of KiCS2:
KICS2BIN=$KICS2HOME/bin
# The directory where CPM installs the binaries:
CPMBIN="$HOME/.cpm/bin"

# check whether a requested tool is installed.
# If yes, execute it, otherwise exit with error.
check_and_exec_tool() {
  TOOLNAME=$1
  TOOLBIN="$KICS2BIN/kics2-$TOOLNAME"
  if [ -x "$TOOLBIN" ] ; then
    shift
    if [ "$TOOLNAME" = cypm ] ; then
      TOOLOPTS="-d curry_bin=$KICS2BIN/kics2"
    else
      TOOLOPTS=
    fi
    #echo "Executing:" "$TOOLBIN" $TOOLOPTS ${1+"$@"}
    exec "$TOOLBIN" $TOOLOPTS ${1+"$@"}
  else
    echo "Incomplete installation: '$TOOLBIN' not installed!"
    echo "Please run: cd $KICS2HOME && make" >&2
    exit 1
  fi
}

# check whether a tool of the distribution should be executed
case $1 in
  cypm | frontend ) check_and_exec_tool ${1+"$@"} ;;
esac

# check whether we should call CPM to compute the correct load path:
if [ ! -d "$HOME" ] ; then
  USECPM=no   # do not use CPM without a home directory
elif [ -x $KICS2BIN/cypm ] ; then
  CYPMBIN=$KICS2BIN/cypm
  USECPM=yes
elif [ -x $CPMBIN/cypm ] ; then
  CYPMBIN=$CPMBIN/cypm
  USECPM=yes
else
  USECPM=no
fi
for i in $* ; do
  case $i in
    --help | -h | -\? ) USECPM=no ;;
    --version | -V    ) USECPM=no ;;
    --nocypm | -n | --numeric-version | --compiler-name | --base-version ) USECPM=no ;;
  esac
done

if [ $USECPM = yes ] ; then
  # set CURRYPATH with 'deps' command of CPM
  CPMPATH=`"$CYPMBIN" -v quiet -d CURRYBIN="$KICS2BIN/kics2" deps -p`
  if [ $? -gt 0 ] ; then
    echo $CPMPATH
    exit 1
  fi
  if [ -n "$CURRYPATH" ] ; then
    CURRYPATH=$CURRYPATH:$CPMPATH # keep existing CURRYPATH setting
  else
    CURRYPATH=$CPMPATH
  fi
  export CURRYPATH
fi

# provide fallback curry path if no tools were installed
if [ -z "$CURRYPATH" ]; then
  export CURRYPATH=$KICS2HOME/lib
fi

REPL="$KICS2BIN/.local/kics2i"
if [ ! -x "$REPL" ] ; then
  echo "ERROR: executable '$REPL' not found!" >&2
  echo "Run: cd $KICS2HOME && make" >&2
  exit 1
fi

# use readline wrapper rlwrap if rlwrap exists, we have tty as stdin,
# and we have a home directory to store rlwrap's history:
USERLWRAP=no
if tty -s ; then
  RLWRAP=`which rlwrap`
  if [ -x "$RLWRAP" -a -d "$HOME" ] ; then
    USERLWRAP=yes
  fi
fi

for i in $* ; do
  if [ $i = "--noreadline" ] ; then
    USERLWRAP=no
  fi
done

# do not use rlwrap inside emacs:
if [ "$TERM" = dumb ] ; then
  USERLWRAP=no
fi

if [ $USERLWRAP = yes ] ; then
  exec rlwrap -c -f "$KICS2HOME/tools/rlwrap" "$REPL" ${1+"$@"}
else
  exec "$REPL" ${1+"$@"}
fi
