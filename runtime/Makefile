# Generate the kics2-runtime.cabal and install the package

# Name of the package
PACKAGE      = kics2-runtime
# Name of the cabal file
CABAL_FILE   = $(PACKAGE).cabal
# idsupply directory to use
IDSUPPLYDIR  = idsupplyioref
# Additional flags for compilation of the runtime
RUNTIMEFLAGS =

# the runtime package configuration file
RUNTIME_PKG_CONF = $(LOCALPKG)/runtime.conf

# replacement stuff
comma     := ,
empty     :=
space     := $(empty) $(empty)
# a b c -> a, b, c
comma_sep  = $(subst $(space),$(comma)$(space),$(1))
# runtime dependencies as comma separated list
CABAL_RUNTIMEDEPS  = $(call comma_sep,$(RUNTIMEDEPS))
# GHC options passed to the cabal file
CABAL_GHC_OPTS = $(RUNTIMEFLAGS:%=-D%)

.PHONY: install
install: $(CABAL_FILE)

.PHONY: clean
clean:
	rm -rf dist/
	rm -rf dist-newstyle/
	rm -f *.hi *.hi-boot */*.hi *.o *.o-boot */*.o
	rm -f $(CABAL_FILE)

$(CABAL_FILE): ../Makefile Makefile
ifndef VERSION
	$(error VERSION is not defined. Please use 'make' on top-level)
endif
	@echo "name:           $(PACKAGE)"                        >  $@
	@echo "version:        $(VERSION)"                        >> $@
	@echo "description:    The runtime environment for KiCS2" >> $@
	@echo "license:        OtherLicense"                      >> $@
	@echo "author:         The KiCS2 Team"                    >> $@
	@echo "maintainer:     kics2@curry-lang.org"              >> $@
	@echo "build-type:     Simple"                            >> $@
	@echo "cabal-version:  >= 1.9.2"                          >> $@
	@echo ""                                                  >> $@
	@echo "library"                                           >> $@
	@echo "  build-depends: $(CABAL_RUNTIMEDEPS)"             >> $@
	@echo "  exposed-modules:"                                >> $@
	@echo "      Basics, CurryException, KiCS2Debug, FailInfo">> $@
	@echo "    , FailTrace, IDSupply"                         >> $@
	@echo "    , MonadList, MonadSearch, PrimTypes, SafeExec" >> $@
	@echo "  other-modules:"                                  >> $@
	@echo "      ConstStore, ID, Search, Solver"              >> $@
	@echo "    , Strategies, Types"                           >> $@
	@echo "  hs-source-dirs: ., $(IDSUPPLYDIR)"               >> $@
	@echo "  ghc-options: $(CABAL_GHC_OPTS)"                  >> $@
