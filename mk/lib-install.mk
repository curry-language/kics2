# directory containing the repository library files:
LIBSRCDIR = $(LIBTRUNKDIR)/src

MODULE_FOLDERS  := $(foreach f, $(shell cd $(LIBSRCDIR) && find * -type d), $(LIBDIR)/$(f))
CURRY_FILES     := $(foreach f, $(shell cd $(LIBSRCDIR) && find * -name "*.curry"), $(LIBDIR)/$(f))
GHC_FILES       := $(foreach f, $(shell cd $(LIBSRCDIR) && find * -name "*.kics2"), $(LIBDIR)/$(f))
GHC_CURRY_FILES := $(addsuffix .curry, $(basename $(GHC_FILES)))
CURRYONLY_FILES  = $(filter-out $(GHC_CURRY_FILES), $(CURRY_FILES))

##########################################################################
# Install the library sources into the Curry system library directory:
##########################################################################

$(MODULE_FOLDERS): $(LIBDIR)/%: $(LIBSRCDIR)/% | $(LIBDIR)
	mkdir -p $@

$(CURRYONLY_FILES): $(LIBDIR)/%.curry: $(LIBSRCDIR)/%.curry | $(LIBDIR)
	cp $< $@

$(GHC_FILES): $(LIBDIR)/%.kics2: $(LIBSRCDIR)/%.kics2 | $(LIBDIR)
	cp $< $@

$(GHC_CURRY_FILES): $(LIBDIR)/%.curry: $(LIBSRCDIR)/%.curry %.kics2 | $(LIBDIR)
	cp $< $@

$(LIBDIR)/VERSION: $(LIBTRUNKDIR)/VERSION | $(LIBDIR)
	cp $< $@
