#!/bin/sh
# shell script to run the Docker image currylang/kics2
# with appropriate options in order to use KiCS2
# with local files and invoke tools contained in the image

# Set docker options:
# Run interactive and remove container after execution:
DOCKEROPTS="-it --rm"
# Mount current working directory and user's home directory:
DOCKEROPTS="$DOCKEROPTS -v `pwd`:`pwd` -w `pwd` -v $HOME:$HOME -e HOME=$HOME"
# Set docker user to host user:
DOCKEROPTS="$DOCKEROPTS -u $(id -u):$(id -g)"

DOCKERTAG="currylang/kics2"
DOCKERBIN=""
ENTRYPOINT=""
CURRYENTRYPOINT="/kics2/kics2/bin/kics2"
HELP=no
PSOPT=""             # name of option to process saved executable
PSCMD=""             # command to process saved executable

while [ $# -gt 0 -a -z "$ENTRYPOINT$DOCKERBIN" ]; do
  case $1 in
    --help | -h | -\? ) shift ; HELP=yes ;;
    -t                ) shift ; DOCKERTAG=$1 ; shift ;;
    -x                ) shift ; ENTRYPOINT=$(realpath "$1") ; shift ;;
    -s                ) shift ; DOCKERBIN=$(realpath "$1") ;
                        ENTRYPOINT="$DOCKERBIN.bin" ; shift ;;
    kics2             ) shift ; ENTRYPOINT="$CURRYENTRYPOINT" ;;
    cypm              ) shift ; ENTRYPOINT="/kics2/kics2/bin/cypm" ;;
    curry-check       ) shift ; ENTRYPOINT="/kics2/cpm/bin/curry-check" ;;
    curry-doc         ) shift ; ENTRYPOINT="/kics2/cpm/bin/curry-doc" ;;
    *                 ) ENTRYPOINT="$CURRYENTRYPOINT" ;;
  esac
done
if [ -z "$ENTRYPOINT" ] ; then
  ENTRYPOINT="$CURRYENTRYPOINT"
fi
DOCKEROPTS="$DOCKEROPTS --entrypoint=$ENTRYPOINT"

if [ $HELP = yes ] ; then
  echo "Usage: kics2-docker.sh [options]"
  echo ""
  echo "with options:"
  echo ""
  echo "-h|-?|--help       : show this message and quit"
  echo "-t TAG             : use docker image with tag TAG (default: $DOCKERTAG)"
  echo "-x <s> <opts>      : run saved executable <s> inside docker with <opts>"
  echo "-s <s>             : transform saved executable <s> into docker executable"
  echo "cypm <opts>        : invoke Curry Package Manager with <opts>"
  echo "curry-check <opts> : invoke CurryCheck with <opts>"
  echo "curry-doc   <opts> : invoke CurryDoc with <opts>"
  echo "kics2 <opts>       : invoke KiCS2 with <opts>"
  echo "<opts>             : invoke KiCS2 with <opts>"
  exit
fi

if [ "$ENTRYPOINT" = "$CURRYENTRYPOINT" ] ; then
  PSOPT="--process-state"
  PSCMD="$(realpath "$0") -t $DOCKERTAG -s"
fi

if [ -n "$DOCKERBIN" ] ; then
  if [ $# -gt 0 ] ; then
    echo "ERROR: Superfluous options: $*"
    exit 1
  fi
  /bin/mv "$DOCKERBIN" "$DOCKERBIN.bin"
  echo "Original executable moved to '$DOCKERBIN.bin'"
  echo "#!/bin/sh" > "$DOCKERBIN"
  echo "docker run $DOCKEROPTS $DOCKERTAG \${1+\"\$@\"}" >> "$DOCKERBIN"
  chmod 755 "$DOCKERBIN"
  echo "New docker execution script written to '$DOCKERBIN'"
else
  docker run $DOCKEROPTS $DOCKERTAG $PSOPT "$PSCMD" ${1+"$@"}
fi
