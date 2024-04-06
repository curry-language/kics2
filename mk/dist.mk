# Makefile for creating a KiCS2 distribution

DISTNAME := kics2-$(VERSION)-$(shell uname -m)-$(shell uname -s | tr '[:upper:]' '[:lower:]')
TARBALLNAME = $(DISTNAME).tar.gz

DISTDIR = $(DISTROOTDIR)/$(DISTNAME)
DISTSRCDIR = $(DISTDIR)/src
DISTBINDIR = $(DISTDIR)/bin
DISTLIBDIR = $(DISTDIR)/lib
DISTDOTMKDIR = $(DISTDIR)/.mk
DISTDOTCPMDIR = $(DISTDIR)/.cpm
DISTCPMPACKAGESDIR = $(DISTDOTCPMDIR)/packages
DISTLOCALBINDIR = $(DISTBINDIR)/.local

DISTFRONTEND = $(DISTBINDIR)/kics2-frontend
DISTCOMP = $(DISTLOCALBINDIR)/kics2c
DISTREPL = $(DISTLOCALBINDIR)/kics2i
DISTINSTALLCURRY = $(DISTSRCDIR)/Installation.curry
DISTLIBVERSION = $(DISTLIBDIR)/VERSION
DISTPACKAGEJSON = $(DISTDIR)/package.json
DISTCPMDEPSDUMMY = $(DISTDOTMKDIR)/.cpmdeps-state-dummy

DISTSTAGE1DIR = $(DISTLOCALBINDIR)/stage1
DISTSTAGE2DIR = $(DISTLOCALBINDIR)/stage2
DISTSTAGE3DIR = $(DISTLOCALBINDIR)/stage3
DISTSTAGE1COMP = $(DISTSTAGE1DIR)/kics2c
DISTSTAGE2COMP = $(DISTSTAGE2DIR)/kics2c
DISTSTAGE3COMP = $(DISTSTAGE3DIR)/kics2c

DISTBOOTSTRAPCOMPS = $(DISTSTAGE1COMP) $(DISTSTAGE2COMP) $(DISTSTAGE3COMP)

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

$(TARBALL): $(DISTDIR) $(DISTFRONTEND) $(DISTCOMP) $(DISTBOOTSTRAPCOMPS) $(DISTSTAGE3COMP) $(DISTREPL) $(DISTINSTALLCURRY) $(DISTLIBVERSION) $(DISTCPMPACKAGESDIR) $(DISTCPMDEPSDUMMY)
	# Make sure that `make` doesn't have to rebuild `kics2c`, `kics2i` and CPM dependencies
	touch $(DISTLIBVERSION)
	touch $(DISTINSTALLCURRY)
	touch $(DISTPACKAGEJSON)
	touch $(DISTCPMDEPSDUMMY)
	touch $(DISTCOMP)
	touch $(DISTREPL)
	touch $(DISTSTAGE1COMP)
	touch $(DISTSTAGE2COMP)
	touch $(DISTSTAGE3COMP)

	# Create the tarball
	cd $(DISTROOTDIR) && tar -cvzf $(TARBALLNAME) $(DISTNAME)

$(DISTDIR): $(ROOT) | $(DISTROOTDIR)
	rm -rf $(DISTDIR)
	git clone $(ROOT) $(DISTDIR)
	cat $(ROOT)/.dist-modules | sed 's|ROOT|$(ROOT)|' > $(DISTDIR)/.gitmodules
	cd $(DISTDIR) && git -c protocol.file.allow=always submodule update --init
	rm -rf $(DISTDIR)/.git $(DISTDIR)/**/.git $(DISTDIR)/.gitmodules $(DISTDIR)/.dist-modules

$(DISTSRCDIR)/%: $(SRCDIR)/%
	mkdir -p $$(dirname $@)
	cp $< $@

$(DISTBINDIR)/%: $(BINDIR)/%
	mkdir -p $$(dirname $@)
	cp $< $@

$(DISTLOCALBINDIR)/%: $(LOCALBINDIR)/%
	mkdir -p $$(dirname $@)
	cp $< $@

$(DISTLIBDIR)/%: $(LIBDIR)/%
	mkdir -p $$(dirname $@)
	cp $< $@

$(DISTCPMPACKAGESDIR): $(DOTCPMDIR)/packages
	mkdir -p $$(dirname $@)
	cp -r $< $@

$(DISTCPMDEPSDUMMY): $(CPMDEPS)
	mkdir -p $$(dirname $@)
	cp $< $@

$(DISTSTAGE1COMP): $(DISTCOMP)
	mkdir -p $$(dirname $@)
	ln -sf ../kics2c $@

$(DISTSTAGE2COMP): $(DISTCOMP)
	mkdir -p $$(dirname $@)
	ln -sf ../kics2c $@

$(DISTSTAGE3COMP): $(DISTCOMP)
	mkdir -p $$(dirname $@)
	ln -sf ../kics2c $@
