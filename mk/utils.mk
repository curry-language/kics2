# Makefile for building installation utils

# The cleancurry binary
export CLEANCURRY = $(LOCALBINDIR)/cleancurry

export UTILS = $(foreach u, pwd cleancurry which, $(LOCALBINDIR)/$(u)$(EXE_SUFFIX))
export UTILS_ARTIFACTS = $(UTILSDIR)/*.hi \
                         $(UTILSDIR)/*.o

.PHONY: all
all: $(UTILS)

$(UTILS): $(LOCALBINDIR)/%$(EXE_SUFFIX): $(UTILSDIR)/%.hs | $(LOCALBINDIR) $(STACKYAML)
	$(GHC) --make -Wall -O2 -o $@ $<
