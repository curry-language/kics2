########################################################################
# Makefile for KiCS2 compiler suite
########################################################################

# Some parameters for this installation
# --------------------------------------
# (these parameters might be passed to `make`)

# The compiler to compile KiCS2 with. PAKCS by default.
# Note that this also determines which CPM to use.
export CURRYC = pakcs

# The path to GHC, its package manager, Cabal and the Curry package manager
export GHC     := $(shell which ghc)
export GHC_PKG := $(shell dirname "$(GHC)")/ghc-pkg
export CABAL   := $(shell which cabal)
export CYPM    := $(CURRYC) cypm

# KiCS2 runtime dependencies (Cabal packages)
export RUNTIMEDEPS = base containers ghc mtl parallel-tree-search tree-monad directory
# KiCS2 library dependencies (Cabal packages)
LIBDEPS            = base directory network network-bsd old-time parallel-tree-search process time
# Custom runtime dependencies (Cabal packages)
CUSTOMDEPS         = kics2-runtime kics2-libraries
# System dependencies (TODO: Windows)
SYSTEMDEPS         = unix
# All dependencies, with duplicates removed (see 'sort')
ALLDEPS            = $(sort $(RUNTIMEDEPS) $(LIBDEPS) $(SYSTEMDEPS))

# Libraries installed with GHC
GHC_LIBS         := $(shell "$(GHC_PKG)" list --global --simple-output --names-only)
# Packages used by the compiler
GHC_PKGS          = $(foreach pkg,$(ALLDEPS),-package $(pkg))
# The compilation of some libraries does not terminate with -O2
# on GHC > 8.0.1, e.g. FiniteMap, therefore we disable this stage.
GHC_OPTIMIZATIONS = -O2 -fno-strictness 
# Options for compiling target programs using our own package db
GHC_OPTS          = -no-user-package-db -package-db $(PKGDB) \
                    -hide-all-packages $(GHC_PKGS)
# GHC version
GHC_MAJOR := $(shell "$(GHC)" --numeric-version | cut -d. -f1)
GHC_MINOR := $(shell "$(GHC)" --numeric-version | cut -d. -f2)

# The Cabal install command that installs into the local package db
export CABAL_INSTALL = $(CABAL) v1-install --with-compiler="$(GHC)" \
                                           --with-hc-pkg="$(GHC_PKG)" \
                                           --package-db="$(PKGDB)" \
                                           --prefix="$(PKGDIR)"

# The KiCS2 directory (the current one)
export ROOT = $(CURDIR)
# The directory containing the built binaries
export BINDIR = $(ROOT)/bin
# The directory containing the frontend sources
FRONTENDDIR = $(ROOT)/frontend
# The directory containing the start scripts (including 'kics2')
SCRIPTSDIR = $(ROOT)/scripts
# The directory containing the runtime
RUNTIMEDIR = $(ROOT)/runtime
# The directory containing the KiCS2 sources
SRCDIR = $(ROOT)/src
# The (generated) Installation.curry module
INSTALLCURRY = $(SRCDIR)/Installation.curry
# The directory containing the built libraries
export LIBDIR = $(ROOT)/lib
# The directory containing the library sources
LIBSRCDIR = $(ROOT)/lib-trunk
# The directories for Cabal packages used at runtime by KiCS2
PKGDIR = $(ROOT)/pkg
PKGDB = $(PKGDIR)/kics2.conf.d
# The frontend binary ('kics2-frontend')
FRONTEND = $(BINDIR)/kics2-frontend
# The REPL binary ('kics2i')
REPL = $(BINDIR)/kics2i
# The compiler binary ('kics2c')
COMP = $(BINDIR)/kics2c

# The Curry system name
export CURRYSYSTEM = kics2
# The KiCS2 version, as defined in CPM's package.json
export VERSION  = $(shell cypm info | grep -oP "^\S*Version\S*\s+\K([\d\.]+)\s*")
MAJORVERSION    = $(word 1,$(subst ., ,$(VERSION)))
MINORVERSION    = $(word 2,$(subst ., ,$(VERSION)))
REVISIONVERSION = $(word 3,$(subst ., ,$(VERSION)))
# The build version number (if >0, then it is a pre-release)
BUILDVERSION    = 1
# Compiler and installation dates
COMPILERDATE := $(shell git log -1 --format="%ci" | cut -c-10)
INSTALLDATE  := $(shell date)

# Text formatting (e.g. for info/warning messages)
# See https://linux.101hacks.com/ps1-examples/prompt-color-using-tput/.
GREEN = 2
CYAN = 6
WHITE = 7
HIGHLIGHT = $(shell tput bold)$(shell tput setaf $(CYAN))
SUCCESS = $(shell tput bold)$(shell tput setaf $(GREEN))
NORMAL = $(shell tput sgr0)

########################################################################
# The targets
########################################################################

# Builds the KiCS2 compiler using CURRYC (PAKCS by default)
.PHONY: all
all: $(REPL)
	@echo "$(SUCCESS) >> Successfully built KiCS2! $(NORMAL)"
	@echo "$(SUCCESS) >> The executables are located in $(BINDIR) $(NORMAL)"

# Cleans up build files (not from the frontend, however!)
.PHONY: clean
clean:
	cd $(RUNTIMEDIR) && $(MAKE) clean
	cd $(SCRIPTSDIR) && $(MAKE) cleanall
	rm $(INSTALLCURRY)
	rm -rf $(BINDIR) \
	       $(LIBDIR) \
	       $(PKGDIR) \
	       $(ROOT)/.cpm \
	       $(ROOT)/.curry \
	       $(SRCDIR)/.curry

# Builds the REPL executable (with CURRYC and its cpm)
$(REPL): $(shell find $(SRCDIR)/KiCS2 -name "*.curry") $(INSTALLCURRY) | frontend runtime scripts $(COMP) $(BINDIR) $(LIBDIR)
	@echo "$(HIGHLIGHT) >> Building KiCS2 REPL $(NORMAL)"
	$(CURRYC) :load KiCS2.REPL :save :quit
	mv KiCS2.REPL $(REPL)

# Builds the compiler executable (with CURRYC and its cpm)
$(COMP): $(shell find $(SRCDIR)/KiCS2 -name "*.curry") | frontend runtime scripts $(BINDIR) $(LIBDIR)
	@echo "$(HIGHLIGHT) >> Building KiCS2 compiler $(NORMAL)"
	$(CURRYC) :load KiCS2.Compile :save :quit
	mv KiCS2.Compile $(COMP)

# Builds the frontend
.PHONY: frontend
frontend: | $(BINDIR)
	@echo "$(HIGHLIGHT) >> Building frontend $(NORMAL)"
	@cd $(FRONTENDDIR) && $(MAKE)
	@cd $(BINDIR) && ln -sf ../frontend/bin/curry-frontend $(FRONTEND)

# Generates start scripts for the compiler, e.g. kics2 which in turn invokes kics2i
.PHONY: scripts
scripts: | $(BINDIR)
	@echo "$(HIGHLIGHT) >> Setting up startup scripts $(NORMAL)"
	@cd $(SCRIPTSDIR) && $(MAKE)

# Compiles the runtime packages and places them in the package db
.PHONY: runtime
runtime: $(PKGDB)
	@echo "$(HIGHLIGHT) >> Building runtime $(NORMAL)"
	@cd $(RUNTIMEDIR) && $(MAKE)

# Creates the package database (for KiCS2's runtime packages)
$(PKGDB): | $(PKGDIR)
	@echo "$(HIGHLIGHT) >> Creating package database for KiCS2 runtime $(NORMAL)"
	rm -rf $(PKGDB)
	$(GHC_PKG) init $@

# Creates a directory for the package database ('pkg')
$(PKGDIR):
	mkdir -p $(PKGDIR)

# Copies the libraries from the source folder ('lib-trunk') to a new one ('lib')
$(LIBDIR):
	@echo "$(HIGHLIGHT) >> Copying KiCS2 standard libraries $(NORMAL)"
	rm -rf $(LIBDIR)
	cd $(LIBSRCDIR) && $(MAKE) -f Makefile_$(CURRYSYSTEM)_install

# Creates a directory for the target binaries ('bin')
$(BINDIR):
	mkdir -p $(BINDIR)

# Generate a source module with metadata about the KiCS2 installation
$(INSTALLCURRY):
	@echo "$(HIGHLIGHT) >> Generating Installation.curry module $(NORMAL)"
	@echo "-- This file is automatically generated, do not change it!" > $@
	@echo "module Installation where" >> $@
	@echo "" >> $@
	@echo "import System.Directory (doesDirectoryExist)" >> $@
	@echo "import System.IO.Unsafe (unsafePerformIO)" >> $@
	@echo "" >> $@
	@echo 'compilerName :: String' >> $@
	@echo 'compilerName = "kics2"' >> $@
	@echo "" >> $@
	@echo 'installDir :: String' >> $@
	@echo 'installDir = if null pkgInstallDir then buildDir else if unsafePerformIO (doesDirectoryExist pkgInstallDir) then pkgInstallDir else buildDir' >> $@
	@echo "" >> $@
	@echo 'buildDir :: String' >> $@
	@echo 'buildDir = "$(ROOT)"' >> $@
	@echo "" >> $@
	@echo 'pkgInstallDir :: String' >> $@
	@echo 'pkgInstallDir = ""' >> $@
	@echo "" >> $@
	@echo 'majorVersion :: Int' >> $@
	@echo 'majorVersion = $(MAJORVERSION)' >> $@
	@echo "" >> $@
	@echo 'minorVersion :: Int' >> $@
	@echo 'minorVersion = $(MINORVERSION)' >> $@
	@echo "" >> $@
	@echo 'revisionVersion :: Int' >> $@
	@echo 'revisionVersion = $(REVISIONVERSION)' >> $@
	@echo "" >> $@
	@echo 'buildVersion :: Int' >> $@
	@echo 'buildVersion = $(BUILDVERSION)' >> $@
	@echo "" >> $@
	@echo 'compilerDate :: String' >> $@
	@echo 'compilerDate = "$(COMPILERDATE)"' >> $@
	@echo "" >> $@
	@echo 'installDate :: String' >> $@
	@echo 'installDate = "$(INSTALLDATE)"' >> $@
	@echo "" >> $@
	@echo 'runtime :: String' >> $@
	@echo 'runtime = "ghc"' >> $@
	@echo "" >> $@
	@echo 'runtimeMajor :: Int' >> $@
	@echo 'runtimeMajor = $(GHC_MAJOR)' >> $@
	@echo "" >> $@
	@echo 'runtimeMinor :: Int' >> $@
	@echo 'runtimeMinor = $(GHC_MINOR)' >> $@
	@echo "" >> $@
	@echo 'baseVersion :: String' >> $@
	@echo 'baseVersion = "$(shell cat $(LIBDIR)/VERSION)"' >> $@
	@echo "" >> $@
	@echo 'ghcExec :: String' >> $@
	@echo 'ghcExec = "\"$(GHC)\""' >> $@
	@echo "" >> $@
	@echo '-- GHC options for using local libraries and not cabal packages:' >> $@
	@echo 'ghcLocalOptions :: String' >> $@
	@echo 'ghcLocalOptions = "$(GHC_OPTS)"' >> $@
	@echo "" >> $@
	@echo 'ghcOptions :: String' >> $@
	@echo 'ghcOptions = ghcLocalOptions ++ " $(foreach pkg,$(CUSTOMDEPS),-package $(pkg))"' >> $@
	@echo "" >> $@
	@echo 'ghcOptimizations :: String' >> $@
	@echo 'ghcOptimizations = "$(GHC_OPTIMIZATIONS)"' >> $@
	@echo "" >> $@
	@echo 'withProfiling :: Bool' >> $@
	@echo 'withProfiling = False' >> $@

