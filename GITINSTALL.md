KiCS2: The Kiel Curry System (Version 2)
========================================

Installation Instructions for the GIT Repository
------------------------------------------------

If you want to install the up-to-date version of KiCS2
from the developer's repository, you have to clone the
[git repository](https://git-ps.informatik.uni-kiel.de/curry/kics2),
e.g., by the shell command

    git clone https://git.ps.informatik.uni-kiel.de/curry/kics2.git

This creates a new directory `kics2` containing the current version.
Go into this directory by

    cd kics2

and execute

    git submodule init
    git submodule update

in order to obtain further files managed by git in other repositories,
i.e., the frontend and the Curry system libraries shared by KiCS2 and PAKCS.

Then, each future update can be obtained by the executing

    git pull
    git submodule update

You also need the profiling libraries of Haskell in order to
install the GIT version of KiCS2. Hence, if you have not already done,
install the packages `haskell-platform-prof` or `ghc-prof`
which contain these profiling libraries.

Due to the fact that the KiCS2 compiler and interactive environment
is implemented in Curry, you need an executable Curry compiler
to install KiCS2 via bootstrapping.
Therefore, you have to install a distribution of some Curry
implementation on your machine in order to start the
bootstrapping process.
Currently, there are at least two options:

 1. Bootstrapping with KiCS2 (the faster option)

    Download and install the KiCS2 distribution from the
    [KiCS2 download site](http://www-ps.informatik.uni-kiel.de/kics2/download.html).
    Be sure to use a different directory than `kics2` for this installation,
    e.g., install this in `/tmp/kics2`.
    If you successfully installed this distribution (note that you do not
    need the complete installation so that it is sufficient to install
    this distribution by `make kernel`), you can generate the initial
    KiCS2 compiler by the command

        make bootstrap KICS2=/tmp/kics2/bin/kics2

    in the directory `kics2`.

 2. Bootstrapping with PAKCS (the slower option)

    Download and install the PAKCS implementation of Curry from the
    [PAKCS web site](http://www.informatik.uni-kiel.de/~pakcs).
    If you successfully installed PAKCS, be sure to have the command
    `pakcs` in your load path. Then you can generate the initial
    KiCS2 compiler by the command

        make bootstrap

    in the directory `kics2`.

Now you are ready to install KiCS2 with all its components by

    make

Further information is available in the installation instructions
of the KiCS2 distribution which can be found
[here](http://www-ps.informatik.uni-kiel.de/kics2/download/INSTALL.html).

-------------------------------------------------------------

Contact: [Michael Hanus](http://www.informatik.uni-kiel.de/~mh/)
