########################################################################
# Makefile for KiCS2 compiler suite
########################################################################

# Some parameters for this installation
# --------------------------------------
# (these parameters might be passed to `make`)

# The compiler to compile KiCS2 with. PAKCS by default.
# Note that this also determines which CPM to use.
export CURRY = pakcs

# The path to GHC, its package manager, Cabal and the Curry package manager
export GHC     := $(shell which ghc)
export GHC_PKG := $(shell dirname "$(GHC)")/ghc-pkg
export CABAL   := $(shell which cabal)
export CYPM    := $(CURRY) cypm

# KiCS2 runtime dependencies (Cabal packages)
export RUNTIMEDEPS = base containers ghc mtl parallel-tree-search tree-monad directory
# KiCS2 library dependencies (Cabal packages)
export LIBDEPS     = base directory network network-bsd old-time parallel-tree-search process time
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
GHC_OPTS          =
# GHC version
GHC_MAJOR := $(shell "$(GHC)" --numeric-version | cut -d. -f1)
GHC_MINOR := $(shell "$(GHC)" --numeric-version | cut -d. -f2)

# The KiCS2 directory (the current one)
export ROOT = $(CURDIR)
# The directory containing the built binaries
export BINDIR = $(ROOT)/bin
# The directory containing local binaries
export LOCALBINDIR = $(BINDIR)/.local
# The directory containing the frontend sources
export FRONTENDDIR = $(ROOT)/frontend
# The directory containing the start scripts (including 'kics2')
export SCRIPTSDIR = $(ROOT)/scripts
# The directory containing the runtime
export RUNTIMEDIR = $(ROOT)/runtime
# The directory containing utility programs
export UTILSDIR = $(ROOT)/utils
# The directory containing the built libraries
export LIBDIR = $(ROOT)/lib
# The directory containing the library sources
export LIBTRUNKDIR = $(ROOT)/lib-trunk
# The directory containing the KiCS2 sources
SRCDIR = $(ROOT)/src
# The (generated) Installation module for use by the compiler
INSTALLCURRY = $(SRCDIR)/Installation.curry
# The (generated) Installation module for use at runtime
INSTALLHS = $(RUNTIMEDIR)/Installation.hs
# The KiCS2 package manifest
PACKAGEJSON = $(ROOT)/package.json
# The Curry package manager directory
CPMDIR = $(ROOT)/.cpm

# Dummy file for tracking installation state of CPM dependencies
CPMDEPS = $(CPMDIR)/.installation-state-dummy

# The KiCS2 version, as defined in CPM's package.json
export VERSION := $(shell $(CYPM) info | perl -nle "print $$& while m{^\S*Version\S*\s+\K([\d\.]+)\s*}g")
MAJORVERSION    = $(word 1,$(subst ., ,$(VERSION)))
MINORVERSION    = $(word 2,$(subst ., ,$(VERSION)))
REVISIONVERSION = $(word 3,$(subst ., ,$(VERSION)))
# The build version number (if >0, then it is a pre-release)
BUILDVERSION    = 1
# Compiler and installation dates
COMPILERDATE := $(shell git log -1 --format="%ci" | cut -c-10)
INSTALLDATE  := $(shell date)

# The executable suffix (Windows or POSIX)
ifneq (,$(findstring MINGW, $(shell uname)))
export WINDOWS    = 1
export EXE_SUFFIX = .exe
else
export EXE_SUFFIX =
endif

# Text formatting (e.g. for info/warning messages)
# See https://linux.101hacks.com/ps1-examples/prompt-color-using-tput/.
GREEN = 2
CYAN = 6
WHITE = 7
HIGHLIGHT = $(shell tput bold)$(shell tput setaf $(CYAN))
SUCCESS = $(shell tput bold)$(shell tput setaf $(GREEN))
NORMAL = $(shell tput sgr0)
NULL  =
SPACE = $(NULL) $(NULL)
COMMA = ,

# prefix "pre" "dir/file.ext" = "dir/prefile.ext"
prefix    = $(patsubst ./%,%,$(dir $(2))$(1)$(notdir $(2)))
# a b c -> a, b, c
comma_sep = $(subst $(SPACE),$(COMMA)$(SPACE),$(1))

########################################################################
# The default target
########################################################################

.PHONY: default
.NOTPARALLEL:
default: all

########################################################################
# Included sub-makefiles
########################################################################

include mk/bin.mk
include mk/lib-install.mk
include mk/lib.mk
include mk/runtime.mk
include mk/scripts.mk
include mk/utils.mk

########################################################################
# The high-level phony targets
########################################################################

# Builds the KiCS2 compiler using CURRY (PAKCS by default)
.PHONY: all
all: $(REPL) $(COMP) $(SCRIPTS)
	@echo "$(SUCCESS)>> Successfully built KiCS2!$(NORMAL)"
	@echo "$(SUCCESS)>> The executables are located in $(BINDIR)$(NORMAL)"

# Builds the REPL and runs it.
.PHONY: run
run: $(REPL)
	$(REPL)

# Builds the REPL (kics2i) only.
.PHONY: repl
repl: $(REPL)

# Builds the compiler (kics2c) only.
.PHONY: compiler
compiler: $(COMP)

# Builds the scripts (kics2, ...) only.
.PHONY: scripts
scripts: $(SCRIPTS)

# Builds the frontend only.
.PHONY: frontend
frontend: $(FRONTEND)

# Builds the libraries only.
.PHONY: lib
lib: $(LIB)

# Installs the dependencies only.
.PHONY: deps
deps: $(CPMDEPS)

# Cleans up library-related build artifacts.
.PHONY: cleanlib
cleanlib:
	rm -rf $(LIB_ARTIFACTS) $(LIBDIR)

# Cleans up runtime-related build artifacts.
.PHONY: cleanruntime
cleanruntime:
	rm -rf $(RUNTIME_ARTIFACTS)

# Cleans up utility-related build artifacts.
.PHONY: cleanutils
cleanutils:
	rm -rf $(UTILS_ARTIFACTS)

# Cleans up binaries.
.PHONY: cleanbin
cleanbin:
	rm -rf $(BIN_ARTIFACTS)

# Cleans up frontend-related build artifacts.
.PHONY: cleanfrontend
cleanfrontend:
	cd $(FRONTENDDIR) && $(MAKE) cleanall

# Cleans up build files (not from the frontend, however!)
.PHONY: clean
clean: cleanlib cleanruntime cleanutils cleanbin
	rm -rf $(CPMDIR) \
	       $(ROOT)/.curry \
	       $(SRCDIR)/.curry \
		   $(INSTALLCURRY) \
		   $(INSTALLHS)

# Cleans up everything.
.PHONY: cleanall
cleanall: clean cleanfrontend

########################################################################
# The targets
########################################################################

$(CPMDEPS): $(PACKAGEJSON)
	@echo "$(HIGHLIGHT)>> Updating CPM index and installing dependencies$(NORMAL)"
	$(CYPM) update
	$(CYPM) install --noexec
	@touch $@

# Creates a directory for the compiled libraries
$(LIBDIR):
	mkdir -p $(LIBDIR)

# Creates a directory for the target binaries ('bin')
$(BINDIR):
	mkdir -p $(BINDIR)

# Creates a directory for local binaries to be output
$(LOCALBINDIR):
	mkdir -p $(LOCALBINDIR)

# Generate a source module with metadata about the KiCS2 installation for use at runtime
$(INSTALLHS):
	@echo "$(HIGHLIGHT)>> Generating Installation.hs module$(NORMAL)"
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
	@echo 'fullVersion :: String' >> $@
	@echo 'fullVersion = "$(VERSION)"' >> $@
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

# Generate a source module with metadata about the KiCS2 installation for use by the compiler
$(INSTALLCURRY): $(INSTALLHS)
	@echo "$(HIGHLIGHT)>> Generating Installation.curry module$(NORMAL)"
	cp $(INSTALLHS) $(INSTALLCURRY)
