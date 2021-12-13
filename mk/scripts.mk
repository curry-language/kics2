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

$(BINS): $(BINDIR)/%: $(SCRIPTSDIR)/%.sh | $(BINDIR)
	@echo "Copying script '$*'..."
	@mkdir -p $(@D)
	@sed "s|^KICS2INSTALLDIR=.*$$|KICS2INSTALLDIR=$(KICS2INSTALLDIR)|" < $< > $@
	@chmod 755 $@

# TODO: Hardcoding $(ROOT) here prevents the script from being relocated
$(BATS): $(BINDIR)/%.bat: $(SCRIPTSDIR)/%.bat | $(BINDIR)
	@echo "Copying script '$*'..."
	@mkdir -p $(@D)
	@sed "s|^set KICS2HOME=.*$$|set KICS2HOME=$(ROOT)|" < $< > $@
	@chmod 755 $@
