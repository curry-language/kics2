########################################################################
# Makefile for KiCS2 compiler suite
########################################################################

# Some parameters for this installation
# --------------------------------------
# (these parameters might be passed to `make`)

# If the parameter CURRYFRONTEND is set to an executable,
# this executable will be used as the front end for KiCS2.
# Otherwise, the front end will be compiled from the sources
# in subdir "frontend".
export CURRYFRONTEND =

# Is this an installation for a distribution (Debian) package (yes|no)?
# In case of "yes":
# - nothing will be stored during the installation in the home directory
# - the documentation will not be built (since this takes a lot of time)
# - the paramters CURRYLIBSDIR and CURRYTOOLSDIR must be defined and
#   refer to the directories containing the Curry system libraries and tools
export DISTPKGINSTALL = no

# In order to build the system in a place different from the place of
# the final installation (e.g., when building it as a (Debian) package),
# the variable KICS2INSTALLDIR should be set to the location where it
# will be finally installed after the build (e.g., /usr/lib/kics2).
# It is required that during the build, this directory does not exist,
# otherwise the build fails. If this variable is set and the
# installed system will be moved to this location after the build, it will be
# used as the root directory for all generated components of the system.
export KICS2INSTALLDIR =

# Should profiling be enabled (yes/no)?
PROFILING       = yes

########################################################################
# The name of the Curry system, needed for installation of currytools
export CURRYSYSTEM = kics2

# The major version number
MAJORVERSION    = 3
# The minor version number
MINORVERSION    = 0
# The revision version number
REVISIONVERSION = 0
# The build version number (if >0, then it is a pre-release)
BUILDVERSION    = 1
# Complete version
export VERSION  = $(MAJORVERSION).$(MINORVERSION).$(REVISIONVERSION)
# The version date:
ifeq ($(DISTPKGINSTALL),yes)
COMPILERDATE := $(shell date "+%Y-%m-%d")
else
COMPILERDATE := $(shell git log -1 --format="%ci" | cut -c-10)
endif
# The installation date, set to the current date
INSTALLDATE    := $(shell date)

# Windows operating system?
ifneq (,$(findstring MINGW, $(shell uname)))
export WINDOWS    = 1
export EXE_SUFFIX = .exe
else
export EXE_SUFFIX =
endif

# Paths used in this installation
# -------------------------------

# the root directory of the installation
export ROOT=$(CURDIR)

# Directories of the sources of the standard libraries and tools
ifeq ($(DISTPKGINSTALL),yes)
export CURRYLIBSDIR  = $(error "CURRYLIBSDIR is undefined!")
export CURRYTOOLSDIR = $(error "CURRYTOOLSDIR is undefined!")
else
export CURRYLIBSDIR  = $(ROOT)/lib-trunk
export CURRYTOOLSDIR = # not used
endif

# binary directory and executables
export BINDIR        = $(ROOT)/bin
# Directory where the front end is located
export FRONTENDDIR   = $(ROOT)/frontend
# Directory where the sources of the standard libraries are located
export LIBSRCDIR     = $(ROOT)/lib-trunk
# Directory where the libraries are located
export LIBDIR        = $(ROOT)/lib
# Directory where the documentation files are located
export DOCDIR        = $(ROOT)/docs
# Directory where local executables are stored
export LOCALBIN      = $(BINDIR)/.local
# installation prefix, may be overwritten
export INSTALLPREFIX = $(ROOT)
# Directory where local package installations are stored
export LOCALPKG   = $(INSTALLPREFIX)/pkg
# The path to the package database
export PKGDB         = $(LOCALPKG)/kics2.conf.d
# The local path (from the ROOT) to the package database
export LOCALPKGDB    = pkg/kics2.conf.d

# The version number of the base libraries:
BASEVERSIONFILE = $(LIBDIR)/VERSION

# Executable of CurryCheck:
CURRYCHECK := $(shell which curry-check)
# Executable of CurryDoc:
CURRYDOC := $(shell which curry-doc)
# Executable of the markdown translator (required for documentation generation):
MD2PDF := $(shell which md2pdf)

# GHC and CABAL configuration
# ---------------------------

# The path to the Glasgow Haskell Compiler and Cabal
export GHC     := $(shell which ghc)
export GHC-PKG := $(shell dirname "$(GHC)")/ghc-pkg
export CABAL    = cabal

# Libraries installed with GHC
GHC_LIBS := $(shell "$(GHC-PKG)" list --global --simple-output --names-only)
# Packages used by the compiler
GHC_PKGS  = $(foreach pkg,$(ALLDEPS),-package $(pkg))

# Standard options for compiling target programs with ghc.
# Uses our own package db and explicitly exposes the packages
# to avoid conflicts with globally installed ones.
export GHC_OPTS       = -no-user-$(GHC_PKG_OPT) -$(GHC_PKG_OPT) "$(PKGDB)" \
                        -hide-all-packages $(GHC_PKGS)
# the same for inclusion into INSTALLHS
export GHC_OPTS_INST  = -no-user-$(GHC_PKG_OPT) -$(GHC_PKG_OPT) \""++installDir++"/$(LOCALPKGDB)\" \
                        -hide-all-packages $(GHC_PKGS)

# extract CABAL version
CABAL_MAJOR := $(shell "$(CABAL)" --numeric-version | cut -d. -f1)
CABAL_MINOR := $(shell "$(CABAL)" --numeric-version | cut -d. -f2)

# Since CABAL 1.22, it is possible to create relocatable packages
# with the option "--enable-relocatable".
# With relocatable packages, we can generate a relocatable distribution
# of KiCS2 when the variable KICS2INSTALLDIR is set.
ifeq ($(shell test $(CABAL_MAJOR) -gt 1 -o \( $(CABAL_MAJOR) -eq 1 -a $(CABAL_MINOR) -ge 22 \) ; echo $$?),0)
CABAL_REL_OPT = --enable-relocatable
else
CABAL_REL_OPT =
endif
# CABAL on Windows does not support the option "--enable-relocatable":
ifdef WINDOWS
CABAL_REL_OPT =
endif

# Command to unregister a package
export GHC_UNREGISTER = "$(GHC-PKG)" unregister --$(GHC_PKG_OPT)="$(PKGDB)"
# Command to install missing packages using cabal
export CABAL_INSTALL  = "$(CABAL)" v1-install --with-compiler="$(GHC)"    \
                        --with-hc-pkg="$(GHC-PKG)" --prefix="$(LOCALPKG)" \
                        --global --package-db="$(PKGDB)" $(CABAL_REL_OPT) \
                        --ghc-options="$(GHC_OPTIMIZATIONS)"
# Cabal profiling options
ifeq ($(PROFILING),yes)
export CABAL_PROFILE = -p
else
export CABAL_PROFILE  =
endif
# Additional flags passed to the runtime
export RUNTIMEFLAGS   =

# extract GHC version
GHC_MAJOR := $(shell "$(GHC)" --numeric-version | cut -d. -f1)
GHC_MINOR := $(shell "$(GHC)" --numeric-version | cut -d. -f2)

# Because of an API change in GHC 7.6,
# we need to distinguish GHC < 7.6 and GHC >= 7.6.
# GHC 7.6 renamed the option "package-conf" to "package-db".
# package-db (>= 7.6) or package-conf (< 7.6)?
ifeq ($(shell test $(GHC_MAJOR) -gt 7 -o \( $(GHC_MAJOR) -eq 7 -a $(GHC_MINOR) -ge 6 \) ; echo $$?),0)
GHC_PKG_OPT = package-db
else
GHC_PKG_OPT = package-conf
endif

# Since the compilation of some of the compiler sources and
# libraries (e.g. FiniteMap.curry) does not terminate when
# using the GHC in version 8.0.1 with optimization option -O2,
# we explicitly deactivate the strictness analysis of the GHC
# when installing KICS2 with a GHC 8.0.1 or higher.
# With this optimization deactivated everything seems to be ok.
ifeq ($(shell test $(GHC_MAJOR) -ge 8 ; echo $$?),0)
export GHC_OPTIMIZATIONS = -O2 -fno-strictness
else
export GHC_OPTIMIZATIONS = -O2
endif

# Special files and binaries used in this installation
# ----------------------------------------------------

# The compiler binary
export COMP         = $(LOCALBIN)/kics2c$(EXE_SUFFIX)
# The REPL binary, used for building the libraries
export REPL         = $(LOCALBIN)/kics2i$(EXE_SUFFIX)
# The default options for the REPL, used for libraries and tools
export REPL_OPTS    = :set v2 :set -ghci
# The standard name of the interactive Curry system in then bin dirctory:
export BINCURRY     = $(BINDIR)/curry
# The frontend binary
export CYMAKE       = $(BINDIR)/$(CURRYSYSTEM)-frontend$(EXE_SUFFIX)
# The cleancurry binary
export CLEANCURRY   = $(BINDIR)/cleancurry$(EXE_SUFFIX)
# The Haskell installation info
export INSTALLHS    = $(ROOT)/runtime/Installation.hs
# The Curry installation info
export INSTALLCURRY = $(ROOT)/src/Installation.curry
# The version information for the manual
MANUALVERSION       = $(ROOT)/docs/src/version.tex
# Logfiles for make
MAKELOG             = make.log
# Utility programs
PWD                 = utils/pwd$(EXE_SUFFIX)
WHICH               = utils/which$(EXE_SUFFIX)

# Cabal packages on which this installation depends
# -------------------------------------------------

# Dependencies for the kics2 runtime system
export RUNTIMEDEPS = base containers ghc mtl parallel-tree-search tree-monad \
	             directory
# Dependencies for the kics2 libraries
export LIBDEPS     = base directory network old-time parallel-tree-search \
	              process time

# Depend on network-bsd package for ghc >= 7.8
# (network-bsd depends on base >= 4.7 which translates to ghc >= 7.8)
ifeq ($(shell test $(GHC_MAJOR) -gt 7 -o \( $(GHC_MAJOR) -eq 7 -a $(GHC_MINOR) -ge 8 \) ; echo $$?),0)
	LIBDEPS += network-bsd
endif

# Dependency to system library
ifdef WINDOWS
export SYSTEMDEPS  = Win32
else
export SYSTEMDEPS  = unix
endif
# All dependencies. Note that "sort" also removes duplicates.
export ALLDEPS     = $(sort $(RUNTIMEDEPS) $(LIBDEPS) $(SYSTEMDEPS))

########################################################################
# The targets
########################################################################

# main (default) target - starts installation with logging
.PHONY: all
all:
ifeq ($(DISTPKGINSTALL),yes)
	$(MAKE) build
	# if we build a package, we compile all libraries at the end
	# so that their intermediate files are up to date:
	$(REPL) $(REPL_OPTS) :load AllLibraries :eval "3*13+3" :quit
else
	@rm -f ${MAKELOG}
	@echo "Make started at `date`" > ${MAKELOG}
	$(MAKE) build 2>&1 | tee -a ${MAKELOG}
	@echo "Make finished at `date`" >> ${MAKELOG}
	@echo "Make process logged in file ${MAKELOG}"
endif

# Check whether the value of KICS2INSTALLDIR, if defined, is a non-existing
# directory
.PHONY: checkinstalldir
checkinstalldir:
	@if [ -n "$(KICS2INSTALLDIR)" -a -d "$(KICS2INSTALLDIR)" ] ; then \
	  echo "ERROR: Variable KICS2INSTALLDIR points to an existing directory!" && exit 1 ; \
	fi

# build the complete system
.PHONY: build
build:
	$(MAKE) kernel
	$(MAKE) tools
	$(MAKE) manual
	chmod -R go+rX .

# remove files from user's home directory
.PHONY: uninstall
uninstall:
ifeq ($(DISTPKGINSTALL),no)
	rm -rf $(HOME)/.kics2rc $(HOME)/.kics2rc.bak $(HOME)/.kics2i_history
endif
	@echo "Just remove this directory to finish uninstallation."

# install additional tools
.PHONY: tools
tools: $(BINCURRY)
	cd currytools && $(MAKE) # shared tools
	cd tools      && $(MAKE) # compiler specific tools

# compile analysis tool only:
.PHONY: CASS
CASS:
	cd currytools && $(MAKE) CASS

# build the kernel system (binaries and libraries)
.PHONY: kernel
kernel:
	$(MAKE) kernelbins
	$(MAKE) kernellibs
	# compile code optimization tools:
	@cd currytools/optimize && $(MAKE)

# build the kernel system binaries (compiler and REPL)
.PHONY: kernelbins
kernelbins: $(PWD) $(WHICH) $(PKGDB) frontend $(CLEANCURRY) scripts copylibs copytools
	$(MAKE) $(INSTALLHS) INSTALLPREFIX="$(shell $(PWD))" \
	                     GHC="$(shell $(WHICH) "$(GHC)")"
	cd src && $(MAKE) # build compiler
	$(MAKE) $(BINCURRY)

# install the libraries of the kernel system (i.e., compile and package them)
.PHONY: kernellibs
kernellibs: $(PKGDB)
	cd lib     && $(MAKE) unregister
	cd runtime && $(MAKE) unregister
	cd runtime && $(MAKE)
	cd lib     && $(MAKE)
	# compile all libraries:
	scripts/compile-all-libs.sh

$(BINCURRY): $(BINDIR)/$(CURRYSYSTEM)
	rm -f $@
	cd $(BINDIR) && ln -s $(CURRYSYSTEM) $(notdir $@)

# install the library sources from the trunk directory:
.PHONY: copylibs
copylibs:
	@if [ -d $(CURRYLIBSDIR) ] ; then cd $(CURRYLIBSDIR) && $(MAKE) -f Makefile_$(CURRYSYSTEM)_install ; fi

# if the directory `currytools` is not present, copy it from the sources:
# (only necessary for the installation of a (Debian) packages, otherwise
# `currytools` is a submodule of the repository)
.PHONY: copytools
copytools:
ifeq ($(DISTPKGINSTALL),yes)
	@if [ ! -f currytools/Makefile ] ; then $(MAKE) forcecopytools ; fi
endif

.PHONY: forcecopytools
forcecopytools:
	mkdir -p currytools
	# Copying currytools from $(CURRYTOOLSDIR)
	cp -pr $(CURRYTOOLSDIR)/* currytools

# create package database
$(PKGDB):
	"$(GHC-PKG)" init $@
	$(CABAL) update
	$(CABAL_INSTALL) $(CABAL_PROFILE) $(filter-out $(GHC_LIBS),$(ALLDEPS))

# install front end (from environment variable CURRYFRONTEND or sources):
.PHONY: frontend
frontend:
	mkdir -p $(BINDIR)
	rm -f $(CYMAKE)
ifeq ($(shell test -x "$(CURRYFRONTEND)" ; echo $$?),0)
	ln -s $(CURRYFRONTEND) $(CYMAKE)
else
	cd $(FRONTENDDIR) && $(MAKE)
	cd $(BINDIR) && ln -s ../pkg/bin/curry-frontend$(EXE_SUFFIX) $(notdir $(CYMAKE))
endif

.PHONY: scripts
scripts: $(PWD)
	cd scripts && $(MAKE) ROOT=$(shell $(PWD))

$(CLEANCURRY): utils/cleancurry$(EXE_SUFFIX)
	mkdir -p $(@D)
	cp $< $@

# build installation utils
utils/%: .FORCE
	cd utils && $(MAKE) $(@F)

########################################################################
# Testing: run test suites to check the installation
#
ifeq ($(DISTPKGINSTALL),yes)
# for a package installation, we run the tests in verbose mode:
export RUNTESTPARAMS=-v
else
export RUNTESTPARAMS=
endif

# run the test suites to check the installation
.PHONY: runtest
runtest:
	@if [ ! -x "$(CURRYCHECK)" ] ; then \
	  echo "Executable 'curry-check' is not installed!" && echo "To run the tests, install it by > cypm install currycheck" ; \
	else $(MAKE) runalltests ; fi

.PHONY: runalltests
runalltests:
	cd testsuite && ./test.sh $(RUNTESTPARAMS)
	cd lib && ./test.sh $(RUNTESTPARAMS)
	cd currytools && $(MAKE) runtest

# run the test suites in verbose mode so that all output is shown:
.PHONY: runtestverbose
runtestverbose:
	$(MAKE) runtest RUNTESTPARAMS=-v

########################################################################
# Cleaning:
#

.PHONY: clean
clean: $(CLEANCURRY)
	-cd benchmarks && $(MAKE) clean
	cd currytools  && $(MAKE) clean
	-cd docs/src   && $(MAKE) clean
	-cd frontend   && $(MAKE) clean
	-cd lib        && $(MAKE) clean
	cd runtime     && $(MAKE) clean
	cd src         && $(MAKE) clean
	cd tools       && $(MAKE) clean
	cd utils       && $(MAKE) clean
	rm -f $(MAKELOG) $(BINCURRY)
	rm -f $(INSTALLHS)

# clean everything (including compiler and tool binaries)
.PHONY: cleanall
cleanall: clean
	cd currytools && $(MAKE) uninstall
	-cd docs/src  && $(MAKE) cleanall
	-cd frontend  && $(MAKE) cleanall
	-cd lib       && $(MAKE) cleanall
	cd scripts    && $(MAKE) cleanall
	cd src        && $(MAKE) cleanall
	cd utils      && $(MAKE) cleanall
	rm -rf $(LOCALBIN) $(CYMAKE) $(LOCALPKG)
	rm -f  $(CLEANCURRY)

.PHONY: maintainer-clean
maintainer-clean: cleanall
	rm -rf $(BINDIR)
	rm -rf $(LIBDIR)
	cd currytools && git clean -fdX
ifeq ($(DISTPKGINSTALL),no)
	cd $(CURRYLIBSDIR)  && git clean -fdX
endif

.PHONY: .FORCE
.FORCE:

##############################################################################
# Building the compiler itself
##############################################################################

GLOBALPKGS = -package kics2-runtime -package kics2-libraries -package kics2-libraries-trace

# generate Haskell module with basic installation information.
# This information is used for building the compiler itself as well as the
# libraries, where the information is exposed by the module Distribution.
$(INSTALLHS): Makefile
ifneq ($(shell test -x "$(GHC)" ; echo $$?), 0)
	$(error "Executable 'ghc' not found. You may use 'make <target> GHC=<path>'")
endif
	echo "-- This file is automatically generated, do not change it!" > $@
	echo "module Installation where" >> $@
	echo "" >> $@
	echo "import System.Directory (doesDirectoryExist)" >> $@
	echo "import System.IO.Unsafe (unsafePerformIO)" >> $@
	echo "" >> $@
	echo 'compilerName :: String' >> $@
	echo 'compilerName = "kics2"' >> $@
	echo "" >> $@
	echo 'installDir :: String' >> $@
	echo 'installDir = if null pkgInstallDir then buildDir else if unsafePerformIO (doesDirectoryExist pkgInstallDir) then pkgInstallDir else buildDir' >> $@
	echo "" >> $@
	echo 'buildDir :: String' >> $@
	echo 'buildDir = "$(INSTALLPREFIX)"' >> $@
	echo "" >> $@
	echo 'pkgInstallDir :: String' >> $@
	echo 'pkgInstallDir = "$(KICS2INSTALLDIR)"' >> $@
	echo "" >> $@
	echo 'majorVersion :: Int' >> $@
	echo 'majorVersion = $(MAJORVERSION)' >> $@
	echo "" >> $@
	echo 'minorVersion :: Int' >> $@
	echo 'minorVersion = $(MINORVERSION)' >> $@
	echo "" >> $@
	echo 'revisionVersion :: Int' >> $@
	echo 'revisionVersion = $(REVISIONVERSION)' >> $@
	echo "" >> $@
	echo 'buildVersion :: Int' >> $@
	echo 'buildVersion = $(BUILDVERSION)' >> $@
	echo "" >> $@
	echo 'compilerDate :: String' >> $@
	echo 'compilerDate = "$(COMPILERDATE)"' >> $@
	echo "" >> $@
	echo 'installDate :: String' >> $@
	echo 'installDate = "$(INSTALLDATE)"' >> $@
	echo "" >> $@
	echo 'runtime :: String' >> $@
	echo 'runtime = "ghc"' >> $@
	echo "" >> $@
	echo 'runtimeMajor :: Int' >> $@
	echo 'runtimeMajor = $(GHC_MAJOR)' >> $@
	echo "" >> $@
	echo 'runtimeMinor :: Int' >> $@
	echo 'runtimeMinor = $(GHC_MINOR)' >> $@
	echo "" >> $@
	echo 'baseVersion :: String' >> $@
	echo 'baseVersion = "$(shell cat $(BASEVERSIONFILE))"' >> $@
	echo "" >> $@
	echo 'ghcExec :: String' >> $@
	echo 'ghcExec = "\"$(GHC)\""' >> $@
	echo "" >> $@
	echo '-- GHC options for using local libraries and not cabal packages:' >> $@
	echo 'ghcLocalOptions :: String' >> $@
	echo 'ghcLocalOptions = "$(GHC_OPTS_INST)"' >> $@
	echo "" >> $@
	echo 'ghcOptions :: String' >> $@
	echo 'ghcOptions = ghcLocalOptions ++ " $(GLOBALPKGS)"' >> $@
	echo "" >> $@
	echo 'ghcOptimizations :: String' >> $@
	echo 'ghcOptimizations = "$(GHC_OPTIMIZATIONS)"' >> $@
	echo "" >> $@
	echo 'withProfiling :: Bool' >> $@
ifeq ($(PROFILING),yes)
	echo 'withProfiling = True' >> $@
else
	echo 'withProfiling = False' >> $@
endif

##############################################################################
# Create HTML documentation for system libraries:
##############################################################################

.PHONY: libdoc
libdoc:
	@rm -f ${MAKELOG}
	@echo "Make libdoc started at `date`" > ${MAKELOG}
	@cd lib && $(MAKE) htmldoc 2>&1 | tee -a ../${MAKELOG}
	@echo "Make libdoc finished at `date`" >> ${MAKELOG}
	@echo "Make libdoc process logged in file ${MAKELOG}"

##############################################################################
# Create the KiCS2 manual
##############################################################################

MANUAL = docs/Manual.pdf

$(MANUAL):
	$(MAKE) manual

.PHONY: manual
manual:
	# generate manual, if necessary:
	@if [ -d $(DOCDIR)/src -a $(DISTPKGINSTALL) = "no" ] ; then \
	   if [ -x "$(CURRYDOC)" -a -x "$(MD2PDF)" ] ; then \
	     $(MAKE) $(MANUALVERSION) && cd $(DOCDIR)/src && $(MAKE) install ; \
	   else echo "Executable 'curry-doc' or 'md2pdf' not found!" ; \
	        echo "To generate the manual, install them by:" ; \
                echo "> cypm install currydoc && cypm install markdown" ; \
           fi \
         fi

${MANUALVERSION}: Makefile
	echo '\\newcommand{\\kicsversiondate}'         >  $@
	echo '{Version $(VERSION) of ${COMPILERDATE}}' >> $@

.PHONY: cleanmanual
cleanmanual:
	-cd docs/src && $(MAKE) clean

# SNIP FOR DISTRIBUTION - DO NOT REMOVE THIS COMMENT

##############################################################################
# Distribution targets
##############################################################################

# temporary directory to create distribution version
FULLNAME = kics2-$(VERSION)
DISTDIR  = $(FULLNAME)
TARBALL  = $(FULLNAME).tar.gz

# generate a source distribution of KiCS2
.PHONY: dist
dist:
	# remove old distribution
	rm -f $(TARBALL)
	$(MAKE) $(TARBALL)

# publish the distribution files in the local web pages
HTMLDIR = $(HOME)/public_html/kics2/download
.PHONY: publish
publish: $(TARBALL)
	cp $(TARBALL) docs/INSTALL.html $(HTMLDIR)
	chmod -R go+rX $(HTMLDIR)
	@echo "Don't forget to run 'update-kics2' to make the update visible!"

# test installation of created distribution
.PHONY: testdist
testdist: $(TARBALL)
	rm -rf $(DISTDIR)
	tar xzfv $(TARBALL)
	cd $(DISTDIR) && $(MAKE) build
	cd $(DISTDIR) && $(MAKE) runtest
	rm -rf $(DISTDIR)
	@echo "Integration test successfully completed."

# Directories containing development stuff only
DEV_DIRS = benchmarks debug docs experiments

# Clean all files that should not be included in a distribution
.PHONY: cleandist
cleandist:
	rm -rf .dist-modules .git .gitignore .gitmodules
	cd currytools              && rm -rf .git .gitignore download_tools.sh
	cd frontend/curry-base     && rm -rf .git .gitignore dist
	cd frontend/curry-frontend && rm -rf .git .gitignore dist
	rm -rf $(CURRYLIBSDIR)
	cd utils                   && $(MAKE) cleanall
	rm -rf $(BINDIR)
	rm -rf $(DEV_DIRS)
	rm -rf $(LOCALPKG)
	rm -rf docker

$(TARBALL): $(COMP) frontend $(MANUAL)
	rm -rf $(DISTDIR)
	# clone current git repository
	git clone . $(DISTDIR)
	# adopt paths for submodules
	cat .dist-modules | sed 's|ROOT|$(ROOT)|' > $(DISTDIR)/.gitmodules
	# check out submodules
	cd $(DISTDIR) && git submodule init && git submodule update
	# create local binary directory
	mkdir -p $(DISTDIR)/bin/.local
	# copy frontend binary
	cp -p $(CYMAKE) $(DISTDIR)/bin/
	# copy bootstrap compiler
	cp -p $(COMP) $(DISTDIR)/bin/.local/
	# generate compiler and REPL in order to have the bootstrapped
	# Haskell translations in the distribution
	cd $(DISTDIR) && $(MAKE) Compile       # translate compiler
	cd $(DISTDIR) && $(MAKE) REPL          # translate REPL
	cd $(DISTDIR) && $(MAKE) clean         # clean object files
	cd $(DISTDIR) && $(MAKE) cleandist     # delete unnessary files
	# copy documentation if it exists:
	mkdir -p $(DISTDIR)/docs
	@if [ -f $(MANUAL) ] ; then cp $(MANUAL) $(DISTDIR)/docs ; fi
	# update Makefile
	cat Makefile \
	  | sed -e "/^# SNIP FOR DISTRIBUTION/,\$$d" \
	  | sed 's|^PROFILING *=.*$$|PROFILING   = no|' \
	  | sed 's|^COMPILERDATE *:=.*$$|COMPILERDATE    = $(COMPILERDATE)|' \
	  > $(DISTDIR)/Makefile
	# Zip it!
	tar cfvz $(TARBALL) $(DISTDIR)
	rm -rf $(DISTDIR)
	@echo "----------------------------------"
	@echo "Distribution $(TARBALL) generated."

##############################################################################
# Development targets
##############################################################################

# bootstrap the compiler
.PHONY: bootstrap
bootstrap: | $(INSTALLHS) $(PKGDB) frontend $(CLEANCURRY) scripts copylibs
	cd src && $(MAKE) bootstrap

.PHONY: fastbootstrap
fastbootstrap: | $(INSTALLHS) $(PKGDB) frontend $(CLEANCURRY) scripts copylibs
	cd src && $(MAKE) fastbootstrap

.PHONY: Compile
Compile: $(PKGDB) $(INSTALLHS) scripts copylibs
	cd src && $(MAKE) CompileBoot

.PHONY: REPL
REPL: $(PKGDB) $(INSTALLHS) scripts copylibs
	cd src && $(MAKE) REPLBoot

# build the benchmark system
.PHONY: benchmarks
benchmarks:
	cd benchmarks && $(MAKE)

$(COMP): | $(INSTALLHS) $(PKGDB) frontend $(CLEANCURRY) scripts copylibs
	cd src && $(MAKE) bootstrap

# Peform a full bootstrap - distribution - installation - uninstallation
# lifecycle to test consistency of the whole process.
.PHONY: roundtrip
roundtrip:
	$(MAKE) maintainer-clean
	$(MAKE) bootstrap
	$(MAKE) kernel
	$(MAKE) dist
	$(MAKE) testdist
	mv $(TARBALL) $(FULLNAME)-$(shell date +%Y%m%d).tar.gz

# This is a debugging target showing you the current setting of variables.
.PHONY: config
config:
	@$(foreach V, \
          $(sort $(.VARIABLES)), \
	  $(if $(filter-out environment% default automatic, \
          $(origin $V)),$(info $V = $($V))))
	@true
