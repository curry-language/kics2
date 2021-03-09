# Makefile for a GHC environment referencing the compiled
# runtime and library packages, together with its dependencies.

ENV_CABAL_PROJECT = $(ENVDIR)/cabal.project

export ENV = $(ENVFILE)
export ENV_ARTIFACTS = $(ENVDIR)

########################################################################
# The general targets
########################################################################

$(ENV_CABAL_PROJECT): | $(ENVDIR)
	@echo "packages: $(LIBDIR), $(RUNTIMEDIR)" > $@

$(ENVFILE): $(ENV_CABAL_PROJECT) $(LIB) $(RUNTIME) | $(ENVDIR)
	@echo "$(HIGHLIGHT)>> Installing libraries and runtime into GHC environment$(NORMAL)"
	rm -f $(ENVFILE)
	cd $(ENVDIR) && $(CABAL) v2-install --lib --package-env $@ $(ALLDEPS)
