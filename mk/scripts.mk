##############################################################################
# Installation of KICS2 shell scripts
##############################################################################

ifdef WINDOWS
BINS =
BATS = $(patsubst $(SCRIPTSDIR)/%, $(BINDIR)/%, $(wildcard $(SCRIPTSDIR)/*.bat))
else
# Unixes
BINS = $(patsubst $(SCRIPTSDIR)/%.sh, $(BINDIR)/%, $(wildcard $(SCRIPTSDIR)/*.sh))
BATS =
endif

export SCRIPTS = $(BINS) $(BATS)

$(BINS): $(BINDIR)/%: $(SCRIPTSDIR)/%.sh
	@echo "Copying script '$*'..."
	@mkdir -p $(@D)
	@cat $< | sed "s|^KICS2BUILDDIR=.*$$|KICS2BUILDDIR=$(ROOT)|" | \
	 sed "s|^KICS2INSTALLDIR=.*$$|KICS2INSTALLDIR=$(KICS2INSTALLDIR)|" > $@
	@chmod 755 $@

$(BATS): $(BINDIR)/%.bat: $(SCRIPTSDIR)/%.bat
	@echo "Copying script '$*'..."
	@mkdir -p $(@D)
	@sed "s|^set KICS2HOME=.*$$|set KICS2HOME=$(ROOT)|" < $< > $@
	@chmod 755 $@
