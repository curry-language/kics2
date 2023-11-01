Docker image of KiCS2
=====================

This directory contains some files to create and run the
[Docker image of KiCS2](https://hub.docker.com/r/currylang/kics2).


Building a new docker image
---------------------------

If necessary, clean old image:

    > docker image rm kics2
    > docker image prune

Then build new image:

    > docker build -t kics2 .

or

    > docker build -t kics2 -f Dockerfile-kics2-... .


Uploading image to Docker Hub
-----------------------------

When the repository does not yet exist on Docker Hub:

1. Log in on https://hub.docker.com as "currylang"
2. Click on "Create Repository"
3. Choose a name ("kics2") and click create

When the repository exists on Docker Hub:

Log into the Docker Hub from command line, tag and push the local image:

    > docker login --username currylang
    > docker tag kics2 currylang/kics2:<version>
    > docker push currylang/kics2:<version>

where <version> should be something like "3.1.0"
or "latest" to update the latest version.


Running the Docker image of KiCS2
---------------------------------

For convenient invocation of KiCS2 and the tools contained in the
Docker image, one can use the shell script contained in this directory:

    > ./kics2-docker.sh

invokes the interactive REPL of KiCS2. Use

    > ./kics2-docker.sh --help

to see all options of this script.

Files
-----

CheckExample.curry : simple program used to initialize curry-check

kics2.sh : a patched version of /kics2/kics2/bin/kics2, required for
           docker/rlwrap bug
