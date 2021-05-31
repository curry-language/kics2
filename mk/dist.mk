# Makefile for creating a KiCS2 distribution

DISTNAME = kics2-$(VERSION)
TARBALLNAME = $(DISTNAME).tar.gz

DISTDIR = $(DISTSDIR)/$(DISTNAME)
export TARBALL = $(DISTSDIR)/$(TARBALLNAME)

##############################################################################
# The distribution targets
##############################################################################

# TODO: Include built compiler & more
$(TARBALL): $(DISTDIR)
	cd $(DISTSDIR) && tar cfvz $(TARBALL) $(DISTDIR)

$(DISTDIR):
	rm -rf $(DISTDIR)
	git clone $(ROOT) $(DISTDIR) --recurse-submodules
