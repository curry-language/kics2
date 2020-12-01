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
GHC     := $(shell which ghc)
GHC_PKG := $(shell dirname "$(GHC)")/ghc-pkg
CABAL   := $(shell which cabal)
CYPM    := $(CURRYC) cypm

# KiCS2 runtime dependencies (Cabal packages)
export RUNTIMEDEPS = base containers ghc mtl parallel-tree-search tree-monad directory
# KiCS2 library dependencies (Cabal packages)
LIBDEPS            = base directory network old-time parallel-tree-search process time
# System dependencies (TODO: Windows)
SYSTEMDEPS         = unix
# All dependencies, with duplicates removed (see 'sort')
ALLDEPS            = $(sort $(RUNTIMEDEPS) $(LIBDEPS) $(SYSTEMDEPS))

# Libraries installed with GHC
GHC_LIBS := $(shell "$(GHC_PKG)" list --global --simple-output --names-only)
# Packages used by the compiler
GHC_PKGS  = $(foreach pkg,$(ALLDEPS),-package $(pkg))

# The directory containing the built binaries
export BINDIR = $(CURDIR)/bin
# The directory containing the frontend sources
FRONTENDDIR = $(CURDIR)/frontend
# The directory containing the start scripts (including 'kics2')
SCRIPTSDIR = $(CURDIR)/scripts
# The directories for Cabal packages used at runtime by KiCS2
PKGDIR = $(CURDIR)/pkg
PKGDB = $(PKGDIR)/kics2.conf.d
# The frontend binary ('kics2-frontend')
FRONTEND = $(BINDIR)/kics2-frontend
# The REPL binary ('kics2i')
REPL = $(BINDIR)/kics2i

########################################################################
# The targets
########################################################################

# Builds the KiCS2 compiler using CURRYC (PAKCS by default)
.PHONY: all
all: $(REPL)

# Generates start scripts for the compiler, e.g. kics2 which in turn invokes kics2i
.PHONY: scripts
scripts: | $(BINDIR)
	cd $(SCRIPTSDIR) && $(MAKE)

# Builds the frontend
.PHONY: frontend
frontend: | $(BINDIR)
	cd $(FRONTENDDIR) && $(MAKE)
	cd $(BINDIR) && ln -sf ../frontend/bin/curry-frontend $(FRONTEND)

# Builds the REPL executable (with CURRYC and its cpm)
$(REPL): frontend $(PKGDB) | $(BINDIR)
	$(CURRYC) :load KiCS2.REPL :save :quit
	mv KiCS2.REPL $(REPL)

# Creates a directory for the target binaries
$(BINDIR):
	mkdir -p $(BINDIR)

# Creates a directory for the package database
$(PKGDIR):
	mkdir -p $(PKGDIR)

# Creates the package database (for KiCS2's runtime packages)
$(PKGDB): | $(PKGDIR)
	rm -rf $(PKGDB)
	$(GHC_PKG) init $@
	$(CABAL) v1-install --with-compiler="$(GHC)" \
	                    --with-hc-pkg="$(GHC_PKG)" \
	                    --package-db="$(PKGDB)" \
						--prefix="$(PKGDIR)" \
	                    $(ALLDEPS)
