# Generate the kics2-runtime.cabal and install the package

# Name of the package
RUNTIME_PACKAGE = kics2-runtime
# Name of the cabal file
RUNTIME_CABAL = $(RUNTIMEDIR)/$(RUNTIME_PACKAGE).cabal
# idsupply directory to use
RUNTIME_IDSUPPLYDIR  = $(RUNTIMEDIR)/idsupplyioref
# Additional flags for compilation of the runtime
RUNTIME_FLAGS =

# the runtime package configuration file
RUNTIME_PKG_CONF = $(LOCALPKG)/runtime.conf

# runtime dependencies as comma separated list
RUNTIME_CABAL_DEPS  = $(call comma_sep,$(RUNTIMEDEPS))
# GHC options passed to the cabal file
RUNTIME_GHC_OPTS = $(RUNTIME_FLAGS:%=-D%)

export RUNTIME = $(RUNTIME_CABAL)
export RUNTIME_ARTIFACTS = $(RUNTIME_CABAL) \
                           $(RUNTIMEDIR)/dist \
                           $(RUNTIMEDIR)/dist-newstyle \
                           $(RUNTIMEDIR)/**/*.hi \
                           $(RUNTIMEDIR)/**/*.hi-boot \
                           $(RUNTIMEDIR)/**/*.o \
                           $(RUNTIMEDIR)/**/*.o-boot

$(RUNTIME_CABAL):
ifndef VERSION
	$(error VERSION is not defined. Please use 'make' on top-level)
endif
	@echo "name:           $(RUNTIME_PACKAGE)"                >  $@
	@echo "version:        $(VERSION)"                        >> $@
	@echo "description:    The runtime environment for KiCS2" >> $@
	@echo "license:        OtherLicense"                      >> $@
	@echo "author:         The KiCS2 Team"                    >> $@
	@echo "maintainer:     kics2@curry-lang.org"              >> $@
	@echo "build-type:     Simple"                            >> $@
	@echo "cabal-version:  >= 1.9.2"                          >> $@
	@echo ""                                                  >> $@
	@echo "library"                                           >> $@
	@echo "  build-depends: $(RUNTIME_CABAL_DEPS)"            >> $@
	@echo "  exposed-modules:"                                >> $@
	@echo "      Basics, CurryException, KiCS2Debug, FailInfo">> $@
	@echo "    , FailTrace, IDSupply, Installation"           >> $@
	@echo "    , MonadList, MonadSearch, PrimTypes, SafeExec" >> $@
	@echo "  other-modules:"                                  >> $@
	@echo "      ConstStore, ID, Search, Solver"              >> $@
	@echo "    , Strategies, Types"                           >> $@
	@echo "  hs-source-dirs: ., $(RUNTIME_IDSUPPLYDIR)"       >> $@
	@echo "  ghc-options: $(RUNTIME_GHC_OPTS)"                >> $@
