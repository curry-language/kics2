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

    git submodule update --init

in order to obtain further files managed by git in other repositories,
i.e., the frontend and the Curry system libraries shared by KiCS2 and PAKCS.

Then, each future update can be obtained by the executing

    git pull
    git submodule update

Due to the fact that the KiCS2 compiler and interactive environment
is implemented in Curry, you need an executable Curry compiler
to install KiCS2 via bootstrapping.
Therefore, you have to install a distribution of some Curry
implementation on your machine in order to start the
bootstrapping process.
Currently, there are at least two options:

 1. Bootstrapping with KiCS2 3.0.0 or newer (the faster option)

    > Note that KiCS2 3.0.0 distributions are not available on the website yet,
      therefore you may have to use PAKCS for bootstrapping for now. Of course you
      can still bootstrap with KiCS2 if you have already built version 3 somewhere.

    Download and install the KiCS2 distribution from the
    [KiCS2 download site](http://www-ps.informatik.uni-kiel.de/kics2/download.html).
    Be sure to use a different directory than this one for this installation,
    e.g., install this in `/tmp/kics2`.
    If you successfully installed this distribution (note that you do not
    need the complete installation so that it is sufficient to install
    this distribution by `make kernel`), you can generate the fully bootstrapped
    KiCS2 compiler by the command

        make CURRY=/path/to/kics2/bin/kics2

    in this directory.

 2. Bootstrapping with PAKCS 3.0.0 or newer (the slower option)

    Download and install the PAKCS implementation of Curry from the
    [PAKCS web site](http://www.informatik.uni-kiel.de/~pakcs).
    If you successfully installed PAKCS, you can generate the fully bootstrapped
    KiCS2 compiler by the command

        make CURRY=/path/to/pakcs/bin/pakcs

    in this directory. If `pakcs` is already on your `PATH`, simply
    running `make` without arguments will work too.

Once successfully built, you will have a fully bootstrapped KiCS2 compiler.
For development you may wish to rebuild the compiler, which you can do with

    make

This will build KiCS2 using `bin/kics2` in this directory, i.e. use the
bootstrapped compiler to rebuild itself.

> Note that changes to the KiCS2 compiler may make it unable to compile itself.
  Therefore it is advised to always specify which compiler you wish to
  compile KiCS2 with, e.g. another instance of KiCS2:

    make CURRY=/path/to/other/kics2/bin/kics2

Further information is available in the installation instructions
of the KiCS2 distribution which can be found
[here](http://www-ps.informatik.uni-kiel.de/kics2/download/INSTALL.html).

-------------------------------------------------------------

Contact: [Michael Hanus](http://www.informatik.uni-kiel.de/~mh/)
