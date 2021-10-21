# Makefile for building and bootstrapping compiler/REPL and other binaries.

# The paths to the CPM package sources used for bootstrapping
BOOTSTRAP_PACKAGE_SRCS = $(foreach p,$(shell ls $(DOTCPMDIR)/packages),$(DOTCPMDIR)/packages/$(p)/src)
# The CURRYPATH used for bootstrapping
BOOTSTRAP_CURRYPATH = $(LIBDIR):$(subst $(SPACE),:,$(BOOTSTRAP_PACKAGE_SRCS))
# The directory for compiled Haskell files of the compiler
BOOTSTRAP_OUTDIR = $(SRCDIR)/.curry/kics2-$(VERSION)
# The main (Haskell) module of `kics2c` for bootstrapping
BOOTSTRAP_COMPILEBOOT = $(BOOTDIR)/CompileBoot.hs
# The main (Haskell) module of `kics2i` for bootstrapping
BOOTSTRAP_REPLBOOT = $(BOOTDIR)/REPLBoot.hs

# KiCS2 options for bootstrap compilation
BOOTSTRAP_KICS2_OPTS = -v2 --parse-options=-Wall -i$(BOOTSTRAP_CURRYPATH)
# GHC options for bootstrap compilation
BOOTSTRAP_GHC_OPTS = $(GHC_OPTIMIZATIONS) --make -v1 -cpp
# Directories for compiled Haskell modules of the packages
BOOTSTRAP_PACKAGE_OUTDIRS = $(foreach p,$(BOOTSTRAP_PACKAGE_SRCS),$(p)/.curry/kics2-$(VERSION))
# GHC includes for bootstrapping
BOOTSTRAP_GHC_INCL = $(BOOTSTRAP_OUTDIR):$(LIBDIR)/.kics2-$(VERSION):$(subst $(SPACE),:,$(BOOTSTRAP_PACKAGE_OUTDIRS))
# The call to the GHC binary for bootstrapping
BOOTSTRAP_GHC = $(GHC) $(GHC_OPTS) $(BOOTSTRAP_GHC_OPTS) -i$(BOOTSTRAP_GHC_INCL)

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
$(REPL): $(shell find $(SRCDIR)/KiCS2 -name "*.curry") $(INSTALLCURRY) $(PACKAGEJSON) | $(FRONTEND) $(CPMDEPS) $(STACKPKGS) $(CLEANCURRY) $(COMP) $(LOCALBINDIR)
	@$(ECHOINFO) "Building KiCS2 REPL"
	$(CURRY) :set v2 :load KiCS2.REPL :save :quit
	mv KiCS2.REPL $@

# Builds the compiler executable (with CURRY and its cpm)
$(COMP): $(shell find $(SRCDIR)/KiCS2 -name "*.curry") $(INSTALLCURRY) $(PACKAGEJSON) | $(FRONTEND) $(CPMDEPS) $(LOCALBINDIR)
	@$(ECHOINFO) "Building KiCS2 compiler"
	$(CURRY) :set v2 :load KiCS2.Compile :save :quit
	mv KiCS2.Compile $@

# Builds the frontend
$(FRONTEND): $(shell find $(FRONTENDDIR) -name "*.hs" -o -name "*.cabal") $(FRONTENDDIR)/Makefile $(FRONTENDDIR)/stack.yaml | $(BINDIR)
	@$(ECHOINFO) "Building Curry frontend"
	@cd $(FRONTENDDIR) && $(MAKE)
	@cd $(BINDIR) && ln -sf ../frontend/bin/curry-frontend $@

# Creates the `curry` executable by linking to `kics2`
$(CURRYBIN): $(KICS2BIN) $(REPL)
	@cd $(BINDIR) && ln -sf kics2 $@

# Builds the package manager
$(CPM): $(shell find $(CPMDIR) -name "*.curry") $(CPMDIR)/Makefile $(CURRYBIN)
	@$(ECHOINFO) "Building Curry package manager"
	@cd $(CURRYTOOLSDIR) && $(MAKE)

########################################################################
# The bootstrap targets
########################################################################

# kics2c compiled with PAKCS (or another KiCS2)
$(STAGE1COMP): $(COMP) $(CPMDEPS) | $(STAGE1DIR)
	cp $(COMP) $@
	@$(ECHOINFO) "Successfully built stage 1 compiler!"

# kics2c compiled with stage1-kics2c
$(STAGE2COMP): $(STAGE1COMP) $(BOOTSTRAP_COMPILEBOOT) | $(STACKPKGS) $(STAGE2DIR)
	@$(ECHOINFO) "Building stage 2 compiler"
	rm -f $(COMP)
	# Compile in multiple steps to avoid memory issues with PAKCS
	cd $(SRCDIR) \
		&& $(STAGE1COMP) $(BOOTSTRAP_KICS2_OPTS) KiCS2.ModuleDeps \
		&& $(STAGE1COMP) $(BOOTSTRAP_KICS2_OPTS) KiCS2.TransTypes \
		&& $(STAGE1COMP) $(BOOTSTRAP_KICS2_OPTS) KiCS2.TransFunctions \
		&& $(STAGE1COMP) $(BOOTSTRAP_KICS2_OPTS) KiCS2.Compile
	$(BOOTSTRAP_GHC) -o $(COMP) $(BOOTSTRAP_COMPILEBOOT)
	cp $(COMP) $@
	@$(ECHOINFO) "Successfully built stage 2 compiler!"

# kics2c compiled with stage2-kics2c
$(STAGE3COMP): $(STAGE2COMP) $(BOOTSTRAP_COMPILEBOOT) | $(STACKPKGS) $(STAGE3DIR)
	@$(ECHOINFO) "Building stage 3 compiler"
	rm -f $(COMP)
	cd $(SRCDIR) && $(STAGE2COMP) $(BOOTSTRAP_KICS2_OPTS) KiCS2.Compile
	$(BOOTSTRAP_GHC) -o $(COMP) $(BOOTSTRAP_COMPILEBOOT)
	cp $(COMP) $@
	@$(ECHOINFO) "Successfully built stage 3 compiler!"

# kics2i compiled with stage3-kics2c
$(STAGE3REPL): $(STAGE3COMP) $(BOOTSTRAP_REPLBOOT) | $(STACKPKGS) $(STAGE3DIR)
	@$(ECHOINFO) "Building stage 3 REPL"
	rm -f $(COMP)
	cd $(SRCDIR) && $(STAGE3COMP) $(BOOTSTRAP_KICS2_OPTS) KiCS2.REPL
	$(BOOTSTRAP_GHC) -o $(REPL) $(BOOTSTRAP_REPLBOOT)
	cp $(REPL) $@
	@$(ECHOINFO) "Successfully built stage 3 REPL!"

# Creates the directory for the first bootstrap stage's binaries
$(STAGE1DIR):
	mkdir -p $@

# Creates the directory for the second bootstrap stage's binaries
$(STAGE2DIR):
	mkdir -p $@

# Creates the directory for the third bootstrap stage's binaries
$(STAGE3DIR):
	mkdir -p $@
