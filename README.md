KiCS2: The Kiel Curry System (Version 2)
========================================

KiCS2 is an implementation of the multi-paradigm declarative language
[Curry](http://www.curry-lang.org) developed by the
[Programming Languages and Compiler Construction](http://www.informatik.uni-kiel.de/en/prog/)
group at the [University of Kiel](http://www.uni-kiel.de/).
KiCS2 compiles Curry programs into [Haskell programs](http://www.haskell.org/)
using the [Glasgow Haskell Compiler](http://www.haskell.org/ghc/)
as its backend.
Similarly to many other implementations of Curry,
KiCS2 has an interactive environment (read/eval/print loop)
to ease the development of Curry applications.

This directory contains:

`RELNOTES.txt`:
  Some information about the current release and changes w.r.t. previous
  releases of PAKCS.

`INSTALL.txt`:
  Instructions how to install the system.

`GITINSTALL.txt`:
  Instructions how to install the system from the GIT repository
  (only intended for developers).

`bin`:
  A directory containing various executables
  to execute the components of KiCS2.

`currytools`:
  This directory contains various tools for Curry
  (see the README there for a more detailed description).

`docs`:
  This directory contains some documentation, in particular,
  the KiCS2 User Manual.

`frontend`:
  This directory contains the Curry frontend, i.e., a parser for
  Curry programs. It is adapted from the parser originally developed for the
  [Muenster Curry Compiler](http://danae.uni-muenster.de/~lux/curry/).

`include`:
  This directory contains some resources which are included
  by various tools delivered with KiCS2.

`lib`:
  This directory contains a collection of standard libraries
  implemented in Curry (including the standard prelude).

`runtime`:
  This directory contains Haskell programs used by the run time
  system of KiCS2.

`src`:
  This directory contains the implementation of the KiCS2 compiler
  and interactive environment. Note that both are implemented
  in Curry so that some bootstrapping is necessary to generate
  the complete system.

`scripts`:
  This directory contains some templates for scripts used by KiCS2.

`testsuite`:
  This directory contains a collection of Curry programs
  implementing unit and property tests to check the functionality
  of the system using CurryCheck. All tests can be executed by the
  shell script `test.sh` in this directory.

`tools`:
  This directory contains various tools for the KiCS2 system.

`utils`:
  This directory contains some utilities, in particular,
  for installing KiCS2 on Windows operating systems.

-------------------------------------------------------------

Contact: [Michael Hanus](http://www.informatik.uni-kiel.de/~mh/)
