KiCS2: The Kiel Curry System (Version 2)
========================================

Installation Instructions
-------------------------

KiCS2 compiles [Curry programs](http://www.curry-language.org)
into [Haskell programs](http://www.haskell.org/).
This distribution runs on Unix-based platforms
and has been developed and tested under Linux (Debian/Ubuntu).
However, it should also run on similar platforms like Mac OS X or SunOS.
Furthermore, the installation of the KiCS2 kernel
has been successfully tested on a Windows 7 together with
[MinGW] (see below for detailed instructions).

If you want to install and run the complete system
on your computer, you need a Haskell implementation,
in particular the [Glasgow Haskell Compiler] and [Cabal],
a Haskell package management system. They are both shipped with the
[Haskell Platform], a ready-to-use Haskell ecosystem.

For instance, if you run Ubuntu Linux, you can easily install
these packages by

    sudo apt-get install haskell-platform

Alternatively, you can easily install the [Glasgow Haskell Compiler]
and [Cabal] by

    sudo apt-get install ghc cabal-install

If you want to use profiling (see the manual for more details),
then you also need to install the packages
`haskell-platform-prof` or `ghc-prof`
which contain additional profiling libraries.

After unpacking the distribution file, go into the main directory
of the distribution (which is denoted by `kics2home` in the following):

    tar xvzf kics2-<version>.tar.gz
    cd kics2-<version>

Now you are ready to install KiCS2 with all its components by

    make

The complete installation with all tools takes some time.
If you do not want to wait so long and are interested
only in a kernel system to run some simple Curry programs,
you can install this kernel system by

    make kernel

This installs only the compiler and the interactive environment
to execute Curry programs but no additional tools (like the
Curry browser, unit tester, etc).

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


Configuring the installation
----------------------------

The normal installation as outlined above will use the
[Glasgow Haskell Compiler] and the [Cabal] versions visible in the path.
If you wish to use other versions, for example because you are
experimenting with different versions of GHC or Cabal,
you can specify one or both of them by

    make install GHC=/path/to/ghc CABAL=/path/to/cabal

If you want to install the system at a location different from
the build location, you have to specify the intended installation
location at built-time with the parameter `KICS2INSTALLDIR`.
Furthermore, the final installation location must not exist
during built time. For instance, to built KiCS2 at some local directory
and move it later to `/opt/kics2`, you can do it by

    make KICS2INSTALLDIR=/opt/kics2
    ...
    mv /path/to/kics2 /opt/kics2

Note that this cross installation is possible only with Cabal
versions 1.22 and higher (which support relocatable packages).


Windows Installation
--------------------

To install the KiCS2 kernel system (the full system currently needs some
Unix-specific mechanisms), you need the following software installed:

  - [MinGW], providing tools like `make`, `cp`, ...
  - the [Glasgow Haskell Compiler]
  - the [Cabal] system.

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

 3. If you want to use the **library for GUI programming in Curry**
    (`kics2home/lib/GUI.curry`), you need also a
    Tcl/Tk implementation (including the windowing shell `wish`)
    which can be downloaded
    [here](http://www.scriptics.com/software/tcltk/) or
    installed in a Ubuntu distribution by

        sudo apt-get install tk tcl

    In this case, the windowing shell `wish` must be
    in your path (or you can also modify the definition of the
    operation `openGuiPort` in the library `GUI.curry`).


Changing system constants
-------------------------

The distribution of KiCS2 is configured with a
**maximal tuple arity of 15**, i.e., Curry programs containing larger
tuple sizes cannot be compiled. If you want to increase this size
(usually, it is preferable to change your program), you have to change
(in a source distribution of KiCS2)
two system files and install your system as follows:

 1. Change the definition of the constant `maxTupleArity` in the file
    `kics2home/frontend/src/Generators/GenFlatCurry.hs`
    according to your required maximal arity.
 2. Delete the file `kics2home/lib/.curry/Prelude.fcy`.
 3. Re-install KiCS2 by running `make`.

-------------------------------------------------------------

Contact: [Michael Hanus](http://www.informatik.uni-kiel.de/~mh/)

[Glasgow Haskell Compiler]: http://www.haskell.org/ghc/
[Cabal]: http://www.haskell.org/cabal/
[MinGW]: http://www.mingw.org/
