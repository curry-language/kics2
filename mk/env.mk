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

$(ENVFILE): $(ENV_CABAL_PROJECT)
	@echo "$(HIGHLIGHT)>> Installing libraries and runtime into GHC environment$(NORMAL)"
	cd $(ENVDIR) && $(CABAL) v2-install --lib --env $@ $(ALLDEPS)
