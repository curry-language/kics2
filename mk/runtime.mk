# Generate the kics2-runtime.cabal and install the package

# Name of the package
RUNTIME_PACKAGE = kics2-runtime
# Name of the cabal file
RUNTIME_CABAL = $(RUNTIMEDIR)/$(RUNTIME_PACKAGE).cabal
# Name of the cabal template
RUNTIME_CABAL_IN = $(RUNTIME_CABAL).in
# IDSupply implementation to use (see kics2-runtime.cabal for a list)
export RUNTIME_IDSUPPLY = idsupplyinteger
# Additional flags for compilation of the runtime
RUNTIME_FLAGS =

# the runtime package configuration file
RUNTIME_PKG_CONF = $(LOCALPKG)/runtime.conf

# runtime dependencies as comma separated list
RUNTIME_CABAL_DEPS  = $(call comma_sep,$(RUNTIMEDEPS))
# GHC options passed to the cabal file
RUNTIME_GHC_OPTS = $(RUNTIME_FLAGS:%=-D%)

export RUNTIME = $(RUNTIME_CABAL)
export RUNTIME_ARTIFACTS = $(RUNTIMEDIR)/dist \
                           $(RUNTIMEDIR)/dist-newstyle \
                           $(RUNTIMEDIR)/.stack-work \
                           $(RUNTIMEDIR)/**/*.hi \
                           $(RUNTIMEDIR)/**/*.hi-boot \
                           $(RUNTIMEDIR)/**/*.o \
                           $(RUNTIMEDIR)/**/*.o-boot

$(RUNTIME_CABAL): $(RUNTIME_CABAL_IN) $(INSTALLHS) $(PACKAGEJSON)
	@envsubst < $< > $@
