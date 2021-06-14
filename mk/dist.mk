# Makefile for creating a KiCS2 distribution

DISTNAME = kics2-$(VERSION)
TARBALLNAME = $(DISTNAME).tar.gz

DISTDIR = $(DISTROOTDIR)/$(DISTNAME)
DISTSRCDIR = $(DISTDIR)/src
DISTBINDIR = $(DISTDIR)/bin
DISTLIBDIR = $(DISTDIR)/lib
DISTDOTMKDIR = $(DISTDIR)/.mk
DISTCPMDEPSDUMMY = $(DISTDOTMKDIR)/.cpmdeps-state-dummy
DISTDOTCPMDIR = $(DISTDIR)/.cpm
DISTCPMPACKAGESDIR = $(DISTDOTCPMDIR)/packages
DISTLOCALBINDIR = $(DISTBINDIR)/.local
export TARBALL = $(DISTROOTDIR)/$(TARBALLNAME)

export DIST = $(TARBALL)
export DIST_ARTIFACTS = $(DISTROOTDIR)

##############################################################################
# The distribution targets
##############################################################################

# TODO: Deal with paths, especially stack
# TODO: Copy (built?) docs

# The distribution includes the `kics2c` compiler built
# using the current Curry compiler (i.e. if you want to
# bootstrap, you have to run `make bootstrap` prior to
# `make dist`). The user of the distribution can then
# take advantage the fully bootstrapped `kics2c`
# to quickly `make` the complete KiCS2 system.

$(TARBALL): $(DISTDIR) $(DISTBINDIR)/kics2-frontend $(DISTLOCALBINDIR)/kics2c $(DISTLOCALBINDIR)/kics2i $(DISTSRCDIR)/Installation.curry $(DISTLIBDIR)/VERSION $(DISTCPMPACKAGESDIR) $(DISTCPMDEPSDUMMY)
	# Make sure that `make` doesn't have to rebuild `kics2c`, `kics2i` and CPM dependencies
	touch $(DISTLIBDIR)/VERSION
	touch $(DISTSRCDIR)/Installation.curry
	touch $(DISTSRCDIR)/package.json
	touch $(DISTLOCALBINDIR)/kics2c
	touch $(DISTLOCALBINDIR)/kics2i
	touch $(DISTCPMDEPSDUMMY)

	# Create the tarball
	cd $(DISTROOTDIR) && tar -cvzf $(TARBALLNAME) $(DISTNAME)

$(DISTDIR): | $(DISTROOTDIR)
	rm -rf $(DISTDIR)
	git clone $(ROOT) $(DISTDIR)
	cat $(ROOT)/.dist-modules | sed 's|ROOT|$(ROOT)|' > $(DISTDIR)/.gitmodules
	cd $(DISTDIR) && git submodule update --init
	rm -rf $(DISTDIR)/.git $(DISTDIR)/**/.git $(DISTDIR)/.gitmodules $(DISTDIR)/.dist-modules

$(DISTSRCDIR)/%: $(SRCDIR)/%
	cp $< $@

$(DISTBINDIR)/%: $(BINDIR)/% | $(DISTBINDIR)
	cp $< $@

$(DISTLOCALBINDIR)/%: $(LOCALBINDIR)/% | $(DISTLOCALBINDIR)
	cp $< $@

$(DISTLIBDIR)/%: $(LIBDIR)/% | $(DISTLIBDIR)
	cp $< $@

$(DISTCPMPACKAGESDIR): $(DOTCPMDIR)/packages | $(DISTDOTCPMDIR)
	cp -r $< $@

$(DISTCPMDEPSDUMMY): $(CPMDEPS) | $(DISTDOTMKDIR)
	cp $< $@

$(DISTBINDIR): | $(DISTDIR)
	mkdir -p $@

$(DISTLOCALBINDIR): | $(DISTDIR)
	mkdir -p $@

$(DISTLIBDIR): | $(LIBDIR)
	mkdir -p $@

$(DISTDOTCPMDIR): | $(DOTCPMDIR)
	mkdir -p $@

$(DISTDOTMKDIR): | $(DISTDIR)
	mkdir -p $@
