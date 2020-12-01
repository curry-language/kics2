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

########################################################################
# The targets
########################################################################

.PHONY: all
all:
	frontend
	# TODO

.PHONY: frontend
frontend: | $(BINDIR)
	cd frontend && $(MAKE)

$(BINDIR):
	mkdir -p $(BINDIR)
