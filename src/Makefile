########################################################################
# Makefile for KiCS2 compiler and REPL
########################################################################

# The kics2 subdirectory for compiled files
SUBDIR     = .curry/kics2
# ghc options
# see https://ghc.haskell.org/trac/ghc/ticket/7068
GHC_OPTS2  = $(GHC_OPTIMIZATIONS) --make -v1 -cpp -fno-liberate-case
# ghc language extensions for compiling translated Curry programs
GHC_EXTS   = -XMultiParamTypeClasses -XFlexibleInstances -XRelaxedPolyRec -XPolyKinds
# ghc includes for compiling translated Curry programs
GHC_INCL   = -i$(ROOT)/runtime:$(ROOT)/runtime/idsupplyghc:$(ROOT)/src/$(SUBDIR):$(LIBDIR)/$(SUBDIR)
# Call to ghc binary
GHC_CALL   = "$(GHC)" $(GHC_OPTS) $(GHC_OPTS2) $(GHC_EXTS) $(GHC_INCL)
# The kics2c call
KICS2C     = $(COMP) -v2 --parse-options=-Wall -i$(LIBDIR)
# options for the bootstrapping compiler
CURRY_OPTS =

# Compile and REPL sources
KICS2_SOURCES = $(shell find KiCS2 -name "*.curry")

.PHONY: all
all: CompileBoot updateInstallInfo REPLBoot

.PHONY: bootstrap-stage1
bootstrap-stage1: clean
	# Create kics2 via PAKCS or other KiCS2 (stage 1)
	@echo "Compiling stage 1"
	$(MAKE) CompileInitial
	cp $(COMP) $(LOCALBIN)/stage1
	$(CLEANCURRY) -r

.PHONY: bootstrap-stage2
bootstrap-stage2: bootstrap-stage1
	# Create kics2 via kics2 (stage 2)
	@echo "Compiling stage 2"
	$(MAKE) CompileBoot
	cp $(COMP) $(LOCALBIN)/stage2

.PHONY: bootstrap-stage3
bootstrap-stage3: bootstrap-stage2
	# Create kics2 via kics2 (stage 3)
	@echo "Compiling stage 3"
	$(MAKE) CompileBoot
	cp $(COMP) $(LOCALBIN)/stage3

bootstrap: | bootstrap-stage3 cleancurry REPLBoot

# faster bootstrap where stage 3 is omitted
.PHONY: fastbootstrap
fastbootstrap: | bootstrap-stage2 cleancurry REPLBoot

.PHONY: cleancurry
cleancurry:
	$(CLEANCURRY) -r

# clean all intermediate files of the compiler bootstrapping
.PHONY: clean
clean:
	rm -f $(LOCALBIN)/stage[1,2,3] Compile
	find . -name "*.o" -type f -delete
	find . -name "*.hi" -type f -delete

.PHONY: cleanall
cleanall: clean
	rm -rf .curry

########################################################################
# Compile
########################################################################

$(INSTALLCURRY): $(INSTALLHS)
	cat $< | \
	  sed "s|^import System.Directory|import System.Directory|" | \
	  sed "s|^import System.IO.Unsafe|import System.IO.Unsafe|" > $@

# generate executable for Curry->Haskell compiler via PAKCS or other KiCS2
.PHONY: CompileInitial
CompileInitial: KiCS2/Compile.curry $(INSTALLCURRY)
	mkdir -p $(LOCALBIN)
ifeq ($(shell test -x "$(KICS2)" ; echo $$?),0)
	$(KICS2) $(CURRY_OPTS) :load KiCS2.Compile :save :quit
else
	pakcs    $(CURRY_OPTS) :load KiCS2.Compile :save :quit || (exit 1)
endif
	mv KiCS2.Compile $(COMP)

# generate executable for Curry -> Haskell compiler kics2c
.PHONY: CompileBoot
CompileBoot: CompileBoot.hs | $(INSTALLCURRY) $(SUBDIR)/KiCS2/Curry_Compile.hs
	mkdir -p $(LOCALBIN)
	$(GHC_CALL) -o $(COMP) $<

# Although $(INSTALLCURRY) is used by the compiler it must not be included
# in the dependencies because for the distribution installation it would
# require a working KICS2 binary *during* installation.
$(SUBDIR)/KiCS2/Curry_Compile.hs: $(KICS2_SOURCES)

# recompile installation information and the compiler
.PHONY: updateInstallInfo
updateInstallInfo: $(SUBDIR)/Curry_Installation.hs
	$(MAKE) CompileBoot

########################################################################
# REPL
########################################################################

# generate executable for Curry -> Haskell REPL kics2i
.PHONY: REPLBoot
REPLBoot: REPLBoot.hs $(SUBDIR)/KiCS2/Curry_REPL.hs
	@echo "Compiling REPL"
	mkdir -p $(LOCALBIN)
	$(GHC_CALL) -o $(REPL) $<

$(SUBDIR)/KiCS2/Curry_REPL.hs: $(KICS2_SOURCES) $(INSTALLCURRY)

$(SUBDIR)/KiCS2/Curry_%.hs: KiCS2/%.curry
	$(KICS2C) $<
