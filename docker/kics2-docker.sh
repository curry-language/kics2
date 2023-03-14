#!/bin/sh
# shell script to run the Docker image currylang/kics2
# with appropriate options in order to use KiCS2
# with local files and invoke tools contained in the image

# set docker options:
# run interactive, remove container after execution
DOCKEROPTS="-it --rm"
# mount current working directory and user's home directory
DOCKEROPTS="$DOCKEROPTS -v `pwd`:`pwd` -w `pwd` -v $HOME:$HOME -e HOME=$HOME"
# set docker user to host user
DOCKEROPTS="$DOCKEROPTS -u $(id -u):$(id -g)"

DOCKERTAG="currylang/kics2"
ENTRYPOINT=""
HELP=no

case $1 in
  --help | -h | -\? ) HELP=yes ;;
esac

if [ $HELP = yes ] ; then
  echo "Usage: kics2-docker.sh [-h|-?|--help] [-t TAG] [options]"
  echo ""
  echo "with options:"
  echo ""
  echo "-h|-?|--help       : show this message and quit"
  echo "-t TAG             : use docker image with tag TAG (default: $DOCKERTAG)"
  echo "cypm <opts>        : invoke Curry Package Manager with <opts>"
  echo "curry-check <opts> : invoke CurryCheck with <opts>"
  echo "curry-doc   <opts> : invoke CurryDoc with <opts>"
  echo "kics2 <opts>       : invoke KiCS2 with <opts>"
  echo "<opts>             : invoke KiCS2 with <opts>"
  exit
fi

# check docker image tag:
if [ $# -gt 1 -a "$1" = "-t" ] ; then
  shift ; DOCKERTAG=$1 ; shift
fi

# check whether an installed tool should be invoked:
case $1 in
  kics2             ) shift ;;
  cypm              ) shift ; ENTRYPOINT="/kics2/kics2/bin/cypm" ;;
  curry-check       ) shift ; ENTRYPOINT="/kics2/cpm/bin/curry-check" ;;
  curry-doc         ) shift ; ENTRYPOINT="/kics2/cpm/bin/curry-doc" ;;
esac

if [ -n "$ENTRYPOINT" ] ; then
  DOCKEROPTS="$DOCKEROPTS --entrypoint=$ENTRYPOINT"
fi

docker run $DOCKEROPTS $DOCKERTAG ${1+"$@"}
