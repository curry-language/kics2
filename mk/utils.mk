# Makefile for building installation utils

# The cleancurry binary
export CLEANCURRY = $(BINDIR)/cleancurry

LOCALUTILS = $(foreach u, pwd cleancurry which, $(LOCALBINDIR)/$(u)$(EXE_SUFFIX))

export UTILS = $(LOCALUTILS) $(CLEANCURRY)
export UTILS_ARTIFACTS = $(UTILSDIR)/*.hi \
                         $(UTILSDIR)/*.o

$(LOCALUTILS): $(LOCALBINDIR)/%$(EXE_SUFFIX): $(UTILSDIR)/%.hs | $(LOCALBINDIR) $(STACKYAML)
	$(GHC) --make -Wall -O2 -o $@ $<

$(CLEANCURRY): $(LOCALBINDIR)/cleancurry
	ln -sf .local/cleancurry $@
