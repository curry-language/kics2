# Makefile for building installation utils

# The cleancurry binary
export CLEANCURRY = $(BINDIR)/cleancurry

export UTILS = $(foreach u, pwd cleancurry which, $(BINDIR)/$(u)$(EXE_SUFFIX))
export UTILS_ARTIFACTS = $(UTILSDIR)/*.hi \
                         $(UTILSDIR)/*.o

.PHONY: all
all: $(UTILS)

$(UTILS): $(BINDIR)/%$(EXE_SUFFIX): $(UTILSDIR)/%.hs
	"$(GHC)" --make -Wall -O2 -o $@ $<
