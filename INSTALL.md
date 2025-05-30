KiCS2: The Kiel Curry System (Version 2)
========================================

Installation Instructions
-------------------------

KiCS2 compiles [Curry programs](http://www.curry-language.org)
into [Haskell programs](http://www.haskell.org/).
This distribution runs on Unix-based platforms
and has been developed and tested under Linux (Debian/Ubuntu).
However, it should also run on similar platforms like macOS.
Furthermore, the installation of the KiCS2 kernel
has been successfully tested on a Windows 7 together with
[MinGW] (see below for detailed instructions).

If you want to install and run the complete system
on your computer, a Haskell implementation is required,
which will be download by [Haskell Stack](https://www.haskellstack.org/)
during the installation process. Thus, this build tool
should be on your system.

For instance, if you run Ubuntu Linux, you can easily install
these packages by

    sudo apt-get install haskell-stack

After unpacking the distribution file, go into the main directory
of the distribution (which is denoted by `kics2home` in the following):

    tar xvzf kics2-<version>.tar.gz
    cd kics2-<version>

Now you are ready to install KiCS2 with all its components by

    make

The complete installation with all tools, in particular,
the Curry Package Manager, takes some time.
If you want to install only in a kernel system to run
some simple Curry programs, you can install the kernel system by

    make kernel

This installs only the compiler and the interactive environment
to execute Curry programs.

When the installation is finished, the commands to use KiCS2
are installed in the local directory `bin`.
In order to use the system easily, add the directory `kics2home/bin`
to your path, e.g., by the command

    export PATH=kics2home/bin:$PATH

in the `bash`.
Now you can start KiCS2 via the command `kics2`.
If you do no want to have the specific version number of KiCS2
included in your path, you can set a symbolic link like

    ln -s kics2-<version> kics2

and put the directory `kics2/bin` into your path.


Windows Installation (experimental, not tested for newest release)
------------------------------------------------------------------

To install the KiCS2 kernel system (the full system currently needs some
Unix-specific mechanisms), you need the following software installed:

  - [MinGW], providing tools like `make`, `cp`, ...
  - [Haskell Stack](https://www.haskellstack.org/)

If you have these tools installed, open the MinGW shell and follow
the installation commands like for a Linux installation, i.e., for unpacking:

    tar xvzf kics2-<version>.tar.gz
    cd kics2-<version>

Now you are ready to install the KiCS2 kernel by

    make kernel

Please note that the compiler binaries do not rely on the MinGW shell, i.e.,
you can use `kics2` like other Windows binaries in a standard command line.

Notes
-----

 1. You can configure the behavior of KiCS2 by various settings
    in the file `.kics2rc` located in your user directory,
    which is created when you use KiCS2 for the first time.

 2. If you like to have support for _line editing or history functionality_
    in the KiCS2 interactive environment (as supported by the readline
    library), you should have the Unix/Linux command `rlwrap` installed
    on your local machine (e.g., by `sudo apt-get install rlwrap`).
    KiCS2 uses `rlwrap` if called on a terminal
    without the parameter `--noreadline`.


Changing system constants
-------------------------

The distribution of KiCS2 is configured with a
**maximal tuple arity of 15**, i.e., Curry programs containing larger
tuple sizes cannot be compiled. If you want to increase this size
(usually, it is preferable to change your program), you have to change
(in a source distribution of KiCS2)
two system files and install your system as follows:

 1. Change the definition of the constant `maxTupleArity` in the file
    `kics2home/frontend/src/Generators/GenAnnotatedFlatCurry.hs`
    according to your required maximal arity.
 2. Delete the files `kics2home/lib/.curry/kics2-*/Prelude.*`.
 3. Re-install KiCS2 by running `make`.

-------------------------------------------------------------

Contact: [Michael Hanus](http://www.michaelhanus.de/)

[MinGW]: http://www.mingw.org/
