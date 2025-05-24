KiCS2: The Kiel Curry System (Version 2)
========================================

Installation Instructions for the GIT Repository
------------------------------------------------

If you want to install the up-to-date version of KiCS2
from the developer's repository, you have to clone the
[git repository](https://github.com/curry-language/kics2),
e.g., by the shell command

    git clone https://github.com/curry-language/kics2.git

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

    Download and install the KiCS2 distribution from the
    [KiCS2 download site](https://www-curry-lang.org/kics2/download.html).
    Be sure to use a different directory than this one for this installation,
    e.g., install this in `/tmp/kics2`.
    If you successfully installed this distribution (note that you do not
    need the complete installation so that it is sufficient to install
    this distribution by `make kernel`), you can generate the fully bootstrapped
    KiCS2 compiler by the command

        make CURRY=[path/to/kics2]/bin/kics2

    in this directory.

 2. Bootstrapping with PAKCS 3.0.0 or newer (the slower option)

    Download and install the PAKCS implementation of Curry from the
    [PAKCS web site](https://www.curry-lang.org/pakcs).
    If you successfully installed PAKCS, you can generate the fully bootstrapped
    KiCS2 compiler by the command

        make CURRY=[path/to/pakcs]/bin/pakcs

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

    make CURRY=[path/to/other/kics2]/bin/kics2

Further information is available in the
[installation instructions of KiCS2](https://github.com/curry-language/kics2/blob/master/INSTALL.md).

-------------------------------------------------------------

Contact: [Michael Hanus](https://www.michaelhanus.de/)
