########################################################################
# Makefile for KiCS2 compiler suite
########################################################################

# Some parameters for this installation
# --------------------------------------
# (these parameters might be passed to `make`)

# The compiler to compile KiCS2 with. PAKCS by default.
# Note that this also determines which CPM to use.
export CURRYC = pakcs

# If the parameter CURRYFRONTEND is set to an executable,
# this executable will be used as the front end for KiCS2.
# Otherwise, the front end will be compiled from the sources
# in subdir "frontend".
export CURRYFRONTEND =

# The CPM executable to use
CYPM = $(CURRYC) cypm
# The directory containing the built binaries
BINDIR = $(CURDIR)/bin
# The directory containing the frontend sources
FRONTENDDIR = $(CURDIR)/frontend
# The directory containing the start scripts (including 'kics2')
SCRIPTSDIR = $(CURDIR)/scripts
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
$(REPL): frontend | $(BINDIR)
	$(CURRYC) :load KiCS2.REPL :save :quit
	mv KiCS2.REPL $(REPL)

# Creates a directory for the target binaries
$(BINDIR):
	mkdir -p $(BINDIR)
