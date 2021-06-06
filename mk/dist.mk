# Makefile for creating a KiCS2 distribution

DISTNAME = kics2-$(VERSION)
TARBALLNAME = $(DISTNAME).tar.gz

DISTDIR = $(DISTROOTDIR)/$(DISTNAME)
DISTBINDIR = $(DISTDIR)/bin
DISTLOCALBINDIR = $(DISTBINDIR)/.local
export TARBALL = $(DISTROOTDIR)/$(TARBALLNAME)

##############################################################################
# The distribution targets
##############################################################################

# TODO: Deal with paths, especially stack
# TODO: Copy (built?) docs
# TODO: Remove Git history?

# The distribution includes the kics2c compiler built
# using the current Curry compiler (i.e. if you want to
# bootstrap, you have to run `make bootstrap` prior to
# `make dist`). The user of the distribution can then
# take advantage the fully bootstrapped kics2c
# to quickly `make` the complete KiCS2 system.

$(TARBALL): $(DISTLOCALBINDIR)/kics2c $(DISTDIR)
	cd $(DISTROOTDIR) && tar -cvzf $(TARBALLNAME) $(DISTNAME)

$(DISTLOCALBINDIR)/kics2c: $(LOCALBINDIR)/kics2c | $(DISTLOCALBINDIR)
	cp $< $@

$(DISTBINDIR): | $(DISTDIR)
	mkdir -p $@

$(DISTLOCALBINDIR): | $(DISTDIR)
	mkdir -p $@

$(DISTDIR):
	rm -rf $(DISTDIR)
	git clone $(ROOT) $(DISTDIR) --recurse-submodules
