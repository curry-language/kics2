# Makefile for various compilations of the system libraries,
# in particular, to generate the documentation

# Set the __KICS2__ flag for the Prelude (to some dummy value)
LIB_FRONTENDPARAMS = --extended -Wnone -i. -o .curry/kics2-$(VERSION) -D "__KICS2__=0"

# directory for LaTeX documentation files
LIB_TEXDOCDIR := $(ROOT)/docs/src/lib

# Curry library files
LIB_CURRY     = $(filter-out $(EXCLUDES), $(LIB_CURRY_FILES))
# lib names without directory prefix
LIB_NAMES     = $(subst /,., $(LIB_CURRY:$(LIBDIR)/%.curry=%))
# lib names included in library documentation page (without directory prefix)
LIB_DOCNAMES = $(filter-out $(DOCEXCLUDES), $(LIB_NAMES))
# Generated files
LIB_AFCY     = $(foreach lib, $(LIB_CURRY:$(LIBDIR)/%.curry=$(LIBDIR)/.curry/kics2-$(VERSION)/%.afcy), $(lib))
LIB_ACY      = $(foreach lib, $(LIB_CURRY:$(LIBDIR)/%.curry=$(LIBDIR)/.curry/kics2-$(VERSION)/%.acy), $(lib))
LIB_HS       = $(foreach lib, $(LIB_CURRY:$(LIBDIR)/%.curry=$(LIBDIR)/.curry/kics2-$(VERSION)/%.hs), $(call prefix,Curry_,$(lib)))
LIB_HS_TRACE = $(foreach lib, $(LIB_CURRY:$(LIBDIR)/%.curry=$(LIBDIR)/.curry/kics2-$(VERSION)/%.hs), $(call prefix,Curry_Trace_,$(lib)))
LIB_TEX      = $(foreach lib, $(LIB_NAMES),  $(LIB_TEXDOCDIR)/$(lib).tex)
LIB_HS_NAMES       = $(call comma_sep,$(foreach lib,$(LIB_NAMES),$(if $(findstring .,$(lib)),$(basename $(lib)).Curry_$(subst .,,$(suffix $(lib))),Curry_$(lib))))
LIB_TRACE_HS_NAMES = $(call comma_sep,$(foreach lib,$(LIB_NAMES),$(if $(findstring .,$(lib)),$(basename $(lib)).Curry_Trace_$(subst .,,$(suffix $(lib))),Curry_Trace_$(lib))))

ALLLIBS       = AllLibraries
MAINGOAL      = $(LIBDIR)/Curry_Main_Goal.curry
# Modules not included as regular libraries:
EXCLUDES      = $(LIBDIR)/$(ALLLIBS).curry $(MAINGOAL)
# Modules not included in library documentation index page:
DOCEXCLUDES  = CPNS ValueSequence

PACKAGE         = kics2-libraries
PACKAGE_TRACE   = kics2-libraries-trace
LIB_CABAL       = $(LIBDIR)/$(PACKAGE).cabal
LIB_TRACE_CABAL = $(LIBDIR)/$(PACKAGE_TRACE).cabal
LIB_CABAL_DEPS  = $(call comma_sep,$(LIBDEPS))

# Executable of CurryDoc:
CURRYDOC := $(shell which curry-doc)

export LIB = $(LIB_CABAL) $(LIB_HS) $(LIB_AFCY) $(LIB_ACY) $(LIBDIR)/$(ALLLIBS).curry # hstrace
export LIB_ARTIFACTS = $(LIBDIR)/.curry \
                       $(LIBDIR)/*.hi \
                       $(LIBDIR)/*.o \
                       $(LIBDIR)/dist \
                       $(LIBDIR)/dist-newstyle \
                       $(LIBDIR)/.stack-work \
                       $(LIB_CABAL) \
                       $(LIB_TRACE_CABAL)

########################################################################
# support for installation
########################################################################

# create a program importing all libraries in order to re-compile them
# so that all auxiliary files (.nda, .hs, ...) are up-to-date
$(LIBDIR)/$(ALLLIBS).curry: $(LIB_CURRY) | $(LIBDIR)
	rm -f $@
	for i in $(filter-out Prelude, $(LIB_NAMES)) ; do echo "import $$i" >> $@ ; done

$(LIB_CABAL): | $(LIBDIR)
	@echo "name:           $(PACKAGE)"                             > $@
	@echo "version:        $(VERSION)"                            >> $@
	@echo "description:    The standard libraries for KiCS2"      >> $@
	@echo "license:        OtherLicense"                          >> $@
	@echo "author:         The KiCS2 Team"                        >> $@
	@echo "maintainer:     kics2@curry-lang.org"                  >> $@
	@echo "build-type:     Simple"                                >> $@
	@echo "cabal-version:  >= 1.9.2"                              >> $@
	@echo ""                                                      >> $@
	@echo "library"                                               >> $@
	@echo "  build-depends:"                                      >> $@
	@echo "      kics2-runtime == $(VERSION)"                     >> $@
	@echo "    , $(LIB_CABAL_DEPS)"                               >> $@
	@echo "  if os(windows)"                                      >> $@
	@echo "    build-depends: Win32"                              >> $@
	@echo "  else"                                                >> $@
	@echo "    build-depends: unix"                               >> $@
	@echo "  exposed-modules: $(LIB_HS_NAMES)"                    >> $@
	@echo "  hs-source-dirs: ./.curry/kics2-$(VERSION)"           >> $@

$(LIB_TRACE_CABAL): | $(LIBDIR)
	@echo "name:           $(PACKAGE_TRACE)"                          > $@
	@echo "version:        $(VERSION)"                               >> $@
	@echo "description:    The tracing standard libraries for KiCS2" >> $@
	@echo "license:        OtherLicense"                             >> $@
	@echo "author:         The KiCS2 Team"                           >> $@
	@echo "maintainer:     kics2@curry-lang.org"                     >> $@
	@echo "build-type:     Simple"                                   >> $@
	@echo "cabal-version:  >= 1.9.2"                                 >> $@
	@echo ""                                                         >> $@
	@echo "library"                                                  >> $@
	@echo "  build-depends:"                                         >> $@
	@echo "      kics2-runtime == $(VERSION)"                        >> $@
	@echo "    , $(LIB_CABAL_DEPS)"                                  >> $@
	@echo "  if os(windows)"                                         >> $@
	@echo "    build-depends: Win32"                                 >> $@
	@echo "  else"                                                   >> $@
	@echo "    build-depends: unix"                                  >> $@
	@echo "  exposed-modules: $(LIB_TRACE_HS_NAMES)"                 >> $@
	@echo "  hs-source-dirs: ./.curry/kics2-$(VERSION)"              >> $@

define LIB_RULE
$(dir $(LIBDIR)/.curry/kics2-$(VERSION)/$1)Curry_$(notdir $1).hs: $(LIB_CURRY_FILES) $(LIB_GHC_FILES) | $(COMP)
	rm -f $$@
	cd $$(LIBDIR) && $$(COMP) -v0 -i. $$(subst /,.,$1)

$(dir $(LIBDIR)/.curry/kics2-$(VERSION)/$1)Curry_Trace_$(notdir $1).hs: $(LIB_CURRY_FILES) $(LIB_GHC_FILES) | $(COMP)
	rm -f $$@
	cd $$(LIBDIR) && $$(COMP) -v0 -i. --trace-failure $$(subst /,.,$1)
endef

$(foreach module, $(LIB_CURRY:$(LIBDIR)/%.curry=%),$(eval $(call LIB_RULE,$(module))))

# generate FlatCurry file in subdirectory .curry:
$(LIBDIR)/.curry/kics2-$(VERSION)/%.afcy: $(LIBDIR)/%.curry | $(LIBDIR)
	cd $(LIBDIR) && "$(FRONTEND)" --type-annotated-flat $(LIB_FRONTENDPARAMS) $(subst /,.,$*)

# generate AbstractCurry file in subdirectory .curry:
$(LIBDIR)/.curry/kics2-$(VERSION)/%.acy: $(LIBDIR)/%.curry | $(LIBDIR)
	cd $(LIBDIR) && "$(FRONTEND)" --acy $(LIB_FRONTENDPARAMS) $(subst /,.,$*)

# compile all libraries:
.PHONY: compile-all-libs
compile-all-libs:
	scripts/compile-all-libs.sh

##############################################################################
# create LaTeX documentation files for system libraries
##############################################################################

# Check whether CurryDoc is installed
.PHONY: checkcurrydoc
checkcurrydoc:
	@if [ ! -x "$(CURRYDOC)" ] ; then \
	  echo "ERROR: Executable 'curry-doc' is not installed!" && echo "Install it by > cpm install currydoc" && exit 1 ; \
	fi

.PHONY: texdoc
texdoc: checkcurrydoc #$(LIB_CURRY)
	@mkdir -p "$(LIB_TEXDOCDIR)"
	$(MAKE) $(LIB_TEX)

# Generate individual LaTeX documentations for libraries.
define LIB_TEXRULE
$(LIB_TEXDOCDIR)/$1.tex: lib/$(subst .,/,$1).curry
	$$(CURRYDOC) --tex "$(LIB_TEXDOCDIR)" $1
endef

$(foreach module, $(LIB_NAMES),$(eval $(call LIB_TEXRULE,$(module))))
