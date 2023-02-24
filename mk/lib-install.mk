# directory containing the repository library files:
LIBSRCDIR = $(LIBTRUNKDIR)/src

LIB_MODULE_FOLDERS  := $(foreach f, $(shell cd $(LIBSRCDIR) && find * -type d), $(LIBDIR)/$(f))
LIB_CURRY_FILES     := $(foreach f, $(shell cd $(LIBSRCDIR) && find * -name "*.curry"), $(LIBDIR)/$(f))
LIB_GHC_FILES       := $(foreach f, $(shell cd $(LIBSRCDIR) && find * -name "*.kics2.hs"), $(LIBDIR)/$(f))
LIB_GHC_CURRY_FILES := $(addsuffix .curry, $(basename $(LIB_GHC_FILES)))
LIB_CURRYONLY_FILES  = $(filter-out $(LIB_GHC_CURRY_FILES), $(LIB_CURRY_FILES))

##########################################################################
# Install the library sources into the Curry system library directory:
##########################################################################

$(LIB_MODULE_FOLDERS): $(LIBDIR)/%: $(LIBSRCDIR)/% | $(LIBDIR)
	mkdir -p $@

$(LIB_CURRYONLY_FILES): $(LIBDIR)/%.curry: $(LIBSRCDIR)/%.curry | $(LIB_MODULE_FOLDERS)
	cp $< $@

$(LIB_GHC_FILES): $(LIBDIR)/%.kics2.hs: $(LIBSRCDIR)/%.kics2.hs | $(LIB_MODULE_FOLDERS)
	cp $< $@

$(LIB_GHC_CURRY_FILES): $(LIBDIR)/%.curry: $(LIBSRCDIR)/%.curry $(LIBSRCDIR)/%.kics2.hs | $(LIB_MODULE_FOLDERS)
	cp $< $@

$(LIBDIR)/VERSION: $(LIBTRUNKDIR)/VERSION | $(LIBDIR)
	cp $< $@
