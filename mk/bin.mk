# Makefile for building and bootstrapping compiler/REPL and other binaries.

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

# GHC options for compilation
GHC_OPTS2 = $(GHC_OPTIMIZATIONS) --make -v1 -cpp -fno-liberate-case
# The call to the GHC binary
GHC_CALL = $(GHC) $(GHC_OPTS) $(GHC_OPTS2)

# The CURRYPATH used for bootstrapping
BOOTSTRAP_CURRYPATH := $(SRCDIR):$(subst $(SPACE),:,$(foreach p,$(shell ls $(DOTCPMDIR)/packages),$(DOTCPMDIR)/packages/$(p)/src))

# The bootstrap directories.
STAGE1DIR = $(LOCALBINDIR)/stage1
STAGE2DIR = $(LOCALBINDIR)/stage2
STAGE3DIR = $(LOCALBINDIR)/stage3

# The bootstrapped compiler binaries.
STAGE1COMP = $(STAGE1DIR)/kics2c
STAGE2COMP = $(STAGE2DIR)/kics2c
STAGE3COMP = $(STAGE3DIR)/kics2c

export BIN = $(REPL) $(COMP) $(FRONTEND)
export BIN_ARTIFACTS = $(BINDIR)

########################################################################
# The general targets
########################################################################

# Builds the REPL executable (with CURRY and its cpm)
$(REPL): $(shell find $(SRCDIR)/KiCS2 -name "*.curry") $(INSTALLCURRY) $(PACKAGEJSON) | $(FRONTEND) $(CPMDEPS) $(RUNTIME) $(CLEANCURRY) $(COMP) $(ENV) $(LOCALBINDIR)
	@echo "$(HIGHLIGHT)>> Building KiCS2 REPL$(NORMAL)"
	$(CURRY) :load KiCS2.REPL :save :quit
	mv KiCS2.REPL $(REPL)

# Builds the compiler executable (with CURRY and its cpm)
$(COMP): $(shell find $(SRCDIR)/KiCS2 -name "*.curry") $(INSTALLCURRY) $(PACKAGEJSON) | $(FRONTEND) $(CPMDEPS) $(RUNTIME) $(LOCALBINDIR)
	@echo "$(HIGHLIGHT)>> Building KiCS2 compiler$(NORMAL)"
	$(CURRY) :load KiCS2.Compile :save :quit
	mv KiCS2.Compile $(COMP)

# Builds the frontend
$(FRONTEND): $(shell find $(FRONTENDDIR) -name "*.hs" -o -name "*.cabal") $(FRONTENDDIR)/Makefile $(FRONTENDDIR)/stack.yaml | $(BINDIR)
	@echo "$(HIGHLIGHT)>> Building Curry frontend$(NORMAL)"
	@cd $(FRONTENDDIR) && $(MAKE)
	@cd $(BINDIR) && ln -srf $(FRONTENDDIR)/bin/curry-frontend $(FRONTEND)

# Creates the `curry` executable by linking to `kics2`
$(CURRYBIN): $(KICS2BIN) $(REPL)
	@cd $(BINDIR) && ln -srf $(KICS2BIN) $(CURRYBIN)

# Builds the package manager
$(CPM): $(shell find $(CPMDIR) -name "*.curry") $(CPMDIR)/Makefile $(CURRYBIN)
	@echo "$(HIGHLIGHT)>> Building Curry package manager$(NORMAL)"
	@cd $(CURRYTOOLSDIR) && $(MAKE)

########################################################################
# The bootstrap targets
########################################################################

# kics2c compiled with PAKCS (or another KiCS2)
$(STAGE1COMP): $(COMP)
	cp $< $@
	@echo "$(HIGHLIGHT)>> Successfully built stage 1!$(NORMAL)"

# kics2c compiled with stage1-kics2c
$(STAGE2COMP): $(STAGE1COMP)
	# TODO:
	# rm $(COMP)
	# $(MAKE) $(COMP) CURRYC=$(STAGE1COMP)
	@echo "$(HIGHLIGHT)>> Successfully built stage 2!$(NORMAL)"

# kics2c compiled with stage2-kics2c
$(STAGE3COMP): $(STAGE2COMP)
	# TODO:
	# rm $(COMP)
	# $(MAKE) $(COMP) KICS2C=$(STAGE2COMP)
	# @echo "$(HIGHLIGHT)>> Successfully built stage 3!$(NORMAL)"
