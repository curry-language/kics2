########################################################################
# Makefile for KiCS2 compiler suite
########################################################################

# Essential system dependencies
PYTHON3 := $(shell which python3)
STACKBIN := $(shell which stack)

ifeq ($(PYTHON3),)
$(error Please make sure that 'python3' is on your PATH or specify it explicitly by passing 'make PYTHON3=...')
endif

ifeq ($(STACKBIN),)
$(error Please make sure that 'stack' (The Haskell Stack build tool) is on your PATH or specify it explicitly by passing 'make STACKBIN=...')
endif

# The built KiCS2
BUILT_KICS2 = $(CURDIR)/bin/kics2
BUILT_CYPM = $(CURDIR)/bin/cypm

# 1 if true, 0 otherwise
BUILT_KICS2_AVAILABLE := $(shell ! test -x "$(BUILT_KICS2)" -a -x "$(BUILT_CYPM)"; echo $$?)

# The compiler to compile KiCS2 with. By default this
# is `kics2` if a built version exists, otherwise PAKCS.
# Note that this also determines which CPM to use. You
# can also pass this variable explicitly to `make`:
#
#     make CURRY=/path/to/curry
ifeq ($(BUILT_KICS2_AVAILABLE),1)
export CURRY = $(BUILT_KICS2)
else
export CURRY = pakcs
endif

# Optionally a custom installation directory that KiCS2
# is assumed to reside in at runtime. Useful for distribution
# packages.
export KICS2INSTALLDIR =

# The name of the Curry system, needed for tool installation
export CURRYSYSTEM = kics2

# The Stack resolver to use. This also determines the GHC version used for KiCS2!
export STACKRESOLVER = lts-21.7
# GHC version as determined by the Stack resolver (see https://www.stackage.org/)
export GHC_MAJOR = 9
export GHC_MINOR = 4

# The KiCS2 home directory (the current one)
export ROOT = $(CURDIR)
export KICS2HOME = $(ROOT)
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
# The directory containing the tools
export CURRYTOOLSDIR = $(ROOT)/currytools
# The directory containing the package manager
export CPMDIR = $(CURRYTOOLSDIR)/cpm
# The directory containing modules for starting the compiler, e.g. when bootstrapping
export BOOTDIR = $(ROOT)/boot
# The directory containing a built distribution archives (e.g. tarballs).
export DISTROOTDIR = $(ROOT)/dist
# The directory containing scripts for use in the Makefiles
MKSCRIPTSDIR = $(ROOT)/mk-scripts
# The directory containing the KiCS2 sources
SRCDIR = $(ROOT)/src
# The directory containing runtime sources
RUNTIMESRCDIR = $(ROOT)/runtime-src
# The template Installation module for use by the compiler
INSTALLCURRYIN = $(SRCDIR)/Installation.curry.in
# The (generated) Installation module for use by the compiler
INSTALLCURRY = $(SRCDIR)/Installation.curry
# The (generated) Installation module for use at runtime
INSTALLHS = $(RUNTIMEDIR)/Installation.hs
# The KiCS2 package manifest
PACKAGEJSON = $(ROOT)/package.json
# The Stack manifest
STACKYAML = $(ROOT)/stack.yaml
# The Stack manifest input file
STACKYAMLIN = $(ROOT)/stack.yaml.in
# The Stack root directory
STACKROOT = $(ROOT)/.stack
# The Stack configuration file
STACKCONFIG = $(STACKROOT)/config.yaml
# The Stack configuration input file
STACKCONFIGIN = $(ROOT)/stack-config.yaml.in
# The Stack lock file
STACKYAMLLOCK = $(ROOT)/stack.yaml.lock
# The Curry package manager directory
DOTCPMDIR = $(ROOT)/.cpm
# The Stack build directory.
DOTSTACKWORKDIR = $(ROOT)/.stack-work
# A directory for auxiliary state files from the Makefile.
DOTMKDIR = $(ROOT)/.mk

# Dummy file for tracking installation state of CPM dependencies
CPMDEPS = $(DOTMKDIR)/.cpmdeps-state-dummy
# Dummy file for tracking installation state of runtime packages
STACKPKGS = $(DOTMKDIR)/.stackpkgs-state-dummy

# The REPL binary ('kics2i')
export REPL = $(LOCALBINDIR)/kics2i
# The compiler binary ('kics2c')
export COMP = $(LOCALBINDIR)/kics2c
# The standard kics2 start script (`kics2`, built by scripts.mk)
export KICS2BIN = $(BINDIR)/kics2
# The Curry binary symlinked to the `kics2` script (`curry`)
export CURRYBIN = $(BINDIR)/curry
# The frontend binary ('kics2-frontend')
export FRONTEND = $(BINDIR)/kics2-frontend
# The package manager binary (`cypm`)
export CPM = $(BINDIR)/cypm

# A utility for outputting a colored info message
export ECHOINFO = $(MKSCRIPTSDIR)/echo-highlighted 6
# A utility for outputting a colored success message
export ECHOSUCCESS = $(MKSCRIPTSDIR)/echo-highlighted 2

# The path to GHC, its package manager, Cabal and the Curry package manager
export STACK = $(STACKBIN) --stack-yaml $(STACKYAML) --stack-root $(STACKROOT)
export GHC   = $(STACK) exec -- ghc
export CYPM  = $(CURRY) cypm

# KiCS2 runtime dependencies (Cabal packages)
export RUNTIMEDEPS = base containers ghc mtl parallel-tree-search tree-monad directory
# KiCS2 library dependencies (Cabal packages)
export LIBDEPS     = base directory network network-bsd old-time parallel-tree-search time# process (seems to cause a duplicate install)
# Custom runtime dependencies (Cabal packages)
export CUSTOMDEPS  = kics2-runtime kics2-libraries
# System dependencies (TODO: Windows)
export SYSTEMDEPS  = unix
# All dependencies, with duplicates removed (see 'sort')
export ALLDEPS = $(sort $(RUNTIMEDEPS) $(LIBDEPS) $(SYSTEMDEPS) $(CUSTOMDEPS))

# Libraries installed with GHC
GHC_LIBS          = $(shell $(STACK) exec ghc-pkg -- list --global --simple-output --names-only)
# Packages used by the compiler
GHC_PKGS          = $(foreach pkg,$(ALLDEPS),-package $(pkg))
# The compilation of some libraries does not terminate with -O2
# on GHC > 8.0.1, e.g. FiniteMap, therefore we disable this stage.
export GHC_OPTIMIZATIONS = -O2 -fno-strictness -fno-liberate-case
export GHC_OPTS          =

# The KiCS2 version, as defined in CPM's package.json
export VERSION := $(shell cat $(PACKAGEJSON) | $(PYTHON3) -c "import sys,json;print(json.load(sys.stdin)['version'])")
export MAJORVERSION    = $(word 1,$(subst ., ,$(VERSION)))
export MINORVERSION    = $(word 2,$(subst ., ,$(VERSION)))
export REVISIONVERSION = $(word 3,$(subst ., ,$(VERSION)))
# The build version number (if >0, then it is a pre-release)
export BUILDVERSION    = 5

ifeq ($(VERSION),)
$(error "Could not determine VERSION. Please make sure that a 'package.json' exists and that it defines a 'version' mapping!")
endif

# Git history is unavailable in distributions, therefore we use a flag to check for it.
# 1 if true, 0 otherwise
GIT_HISTORY_AVAILABLE := $(shell ! test -d "$(ROOT)/.git"; echo $$?)

# Compiler and installation dates
ifeq ($(GIT_HISTORY_AVAILABLE),1)
export COMPILERDATE := $(shell git log -1 --format="%ci" | cut -c-10)
else
export COMPILERDATE := $(shell date "+%Y-%m-%d")
endif
export INSTALLDATE  := $(shell date)

# The executable suffix (Windows or POSIX)
ifneq (,$(findstring MINGW, $(shell uname)))
export WINDOWS    = 1
export EXE_SUFFIX = .exe
else
export EXE_SUFFIX =
endif

# Text snippets
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

# Builds the entire KiCS2 system. Uses an existing KiCS2 compiler if
# available, otherwise performs a full bootstrap.
.PHONY: default
.NOTPARALLEL:
ifeq ($(CURRY:%kics2=),)
default: all
else
default: bootstrap
endif

########################################################################
# Included sub-makefiles
########################################################################

include mk/lib-install.mk
include mk/lib.mk
include mk/runtime.mk
include mk/scripts.mk
include mk/utils.mk
include mk/bin.mk
include mk/dist.mk
include mk/manual.mk
include mk/testing.mk

########################################################################
# The high-level phony targets
########################################################################

# Builds the entire KiCS2 system using CURRY (PAKCS by default)
.PHONY: all
all:
	$(MAKE) kernel
	$(MAKE) tools
	@$(ECHOSUCCESS) "Successfully built KiCS2!"
	@$(ECHOSUCCESS) "The executables are located in $(BINDIR)"

# Bootstraps the entire KiCS2 system in 3 stages using CURRY (PAKCS by default),
# then performs a bootstrapped build of the tools
.PHONY: bootstrap
bootstrap: $(STAGE3REPL)
	$(MAKE) scripts
	$(MAKE) tools
	@$(ECHOSUCCESS) "Successfully bootstrapped KiCS2!"
	@$(ECHOSUCCESS) "The executables are located in $(BINDIR)"

# Builds the REPL, compiler and scripts.
.PHONY: kernel
kernel: $(REPL) $(SCRIPTS)

# Builds the tools.
.PHONY: tools
tools: $(CPM)

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

# Builds the runtime only.
.PHONY: runtime
runtime: $(RUNTIME)

# Builds the scripts (kics2, ...) only.
.PHONY: scripts
scripts: $(SCRIPTS)

# Builds the utils (cleancurry, ...) only.
.PHONY: utils
utils: $(UTILS)

# Builds the frontend only.
.PHONY: frontend
frontend: $(FRONTEND)

# Builds the package manager only.
.PHONY: cpm
cpm: $(CPM)

# Builds the libraries only.
.PHONY: lib
lib: $(LIB)

# Builds the runtime packages (kics2-libraries and kics2-runtime).
.PHONY: pkgs
pkgs: $(STACKPKGS)

# Installs the dependencies only.
.PHONY: deps
deps: $(CPMDEPS)

# Creates a KiCS2 distribution.
.PHONY: dist
dist: $(DIST)
	@$(ECHOSUCCESS) "Successfully built KiCS2 distribution!"
	@$(ECHOSUCCESS) "The tarball is located at $(DIST)"

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

# Cleans up distributions.
.PHONY: cleandist
cleandist:
	rm -rf $(DIST_ARTIFACTS)

# Cleans up frontend-related build artifacts.
.PHONY: cleanfrontend
cleanfrontend:
	cd $(FRONTENDDIR) && $(MAKE) cleanall

# Cleans up tool-related build artifacts.
.PHONY: cleantools
cleantools:
	cd $(CURRYTOOLSDIR) && $(MAKE) clean

# Cleans up build files (not from the frontend, however!)
.PHONY: cleankics2
cleankics2: cleanlib cleanruntime cleanutils cleanbin cleandist
	rm -rf $(DOTCPMDIR) $(CPMDEPS) \
	       $(DOTSTACKWORKDIR) $(STACKPKGS) \
	       $(STACKYAML) \
	       $(STACKYAMLLOCK) \
	       $(ROOT)/.curry \
	       $(SRCDIR)/.curry \
	       $(RUNTIMESRCDIR)/.curry \
		   $(INSTALLCURRY) \
		   $(INSTALLHS) \
		   $(BOOTDIR)/*.hi \
		   $(BOOTDIR)/*.o

# Cleans up everything.
.PHONY: clean
clean: cleankics2 cleanfrontend cleantools
	rm -rf $(STACKROOT)

########################################################################
# The targets
########################################################################

$(CPMDEPS): $(PACKAGEJSON) | $(DOTMKDIR)
	@$(ECHOINFO) "Updating CPM index and installing dependencies"
	$(CYPM) update
	$(CYPM) install --noexec
	@touch $@

$(STACKPKGS): $(STACKYAML) $(STACKCONFIG) $(LIB) $(RUNTIME) | $(DOTMKDIR)
	@$(ECHOINFO) "Rebuilding runtime and libraries"
	$(STACK) build
	chmod a+rw $(STACKROOT)/stack.sqlite3.pantry-write-lock \
	           $(STACKROOT)/pantry/pantry.sqlite3.pantry-write-lock \
	           $(DOTSTACKWORKDIR)/stack.sqlite3.pantry-write-lock
	chmod a+r $(STACKYAMLLOCK) $(STACKCONFIG)
	@touch $@

# Creates a directory for the compiled libraries
$(LIBDIR):
	mkdir -p $@

# Creates a directory for the target binaries ('bin')
$(BINDIR):
	mkdir -p $@

# Creates a directory for local binaries to be output
$(LOCALBINDIR):
	mkdir -p $@

# Creates a directory for auxiliary local state files from the Makefiles.
$(DOTMKDIR):
	mkdir -p $@

# Creates a directory for the distributions.
$(DISTROOTDIR):
	mkdir -p $@

# Creates the .stack directory.
$(STACKROOT):
	mkdir -p $@

# Generates the local stack configuration.
$(STACKYAML): $(STACKYAMLIN) | $(LIB) $(RUNTIME)
	@$(ECHOINFO) "Generating local Stack config (stack.yaml)"
	@envsubst < $< > $@

# Generates the global stack configuration.
$(STACKCONFIG): $(STACKCONFIGIN) | $(STACKROOT)
	@$(ECHOINFO) "Generating global Stack config (config.yaml)"
	@envsubst < $< > $@

# Generate a source module with metadata about the KiCS2 installation for use by the compiler
$(INSTALLCURRY): $(INSTALLCURRYIN) $(PACKAGEJSON) $(LIBDIR)/VERSION Makefile
	@$(ECHOINFO) "Generating Installation.curry module"
	$(eval export BASE_VERSION := $(shell cat $(LIBDIR)/VERSION))
	@envsubst < $< > $@

# Generates a source module with metadata about the KiCS2 installation for use at runtime
$(INSTALLHS): $(INSTALLCURRY)
	@$(ECHOINFO) "Generating Installation.hs module"
	cp $< $@
	@echo "" >> $@
	@echo 'installDir :: String' >> $@
	@echo 'installDir = if null pkgInstallDir then buildDir else if unsafePerformIO (doesDirectoryExist pkgInstallDir) then pkgInstallDir else buildDir' >> $@
	@echo "" >> $@
	@echo 'buildDir :: String' >> $@
	@echo 'buildDir = "$(ROOT)"' >> $@
	@echo "" >> $@
	@echo 'pkgInstallDir :: String' >> $@
	@echo 'pkgInstallDir = "$(KICS2INSTALLDIR)"' >> $@
