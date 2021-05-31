# Makefile for creating a KiCS2 distribution

DISTNAME = kics2-$(VERSION)
TARBALLNAME = $(DISTNAME).tar.gz

DISTDIR = $(DISTSDIR)/$(DISTNAME)
DISTBINDIR = $(DISTDIR)/bin
DISTLOCALBINDIR = $(DISTBINDIR)/.local
export TARBALL = $(DISTSDIR)/$(TARBALLNAME)

##############################################################################
# The distribution targets
##############################################################################

# TODO: Deal with paths, especially stack
# TODO: Include binaries?
# TODO: Copy docs
# TODO: Remove Git history?

$(TARBALL): $(DISTBINDIR)/kics2 $(DISTLOCALBINDIR)/kics2c $(DISTLOCALBINDIR)/kics2i $(DISTDIR)
	cd $(DISTSDIR) && tar cfvz $(TARBALL) $(DISTDIR)

$(DISTBINDIR)/%: $(BINDIR)/% | $(DISTBINDIR)
	cp $< $@

$(DISTLOCALBINDIR)/%: $(LOCALBINDIR)/% | $(DISTLOCALBINDIR)
	cp $< $@

$(DISTBINDIR): | $(DISTDIR)
	mkdir -p $@

$(DISTLOCALBINDIR): | $(DISTDIR)
	mkdir -p $@

$(DISTDIR):
	rm -rf $(DISTDIR)
	git clone $(ROOT) $(DISTDIR) --recurse-submodules
