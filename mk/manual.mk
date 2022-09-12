# Makefile to create the KiCS2 manual

# Directory where the documentation files are located
export DOCDIR        = $(ROOT)/docs
# Executable of CurryDoc:
CURRYDOC := $(shell which curry-doc)
# The version information for the manual
MANUALVERSION       = $(ROOT)/docs/src/version.tex

##############################################################################
# Create the KiCS2 manual
##############################################################################

MANUAL = docs/Manual.pdf

$(MANUAL):
	$(MAKE) manual

.PHONY: manual
manual:
	# generate manual, if necessary:
	@if [ -d $(DOCDIR)/src ] ; then \
	   if [ -x "$(CURRYDOC)" ] ; then \
	     $(MAKE) $(MANUALVERSION) && cd $(DOCDIR)/src && $(MAKE) install ; \
	   else echo "Executable 'curry-doc' not found!" ; \
	        echo "To generate the manual, install them by:" ; \
                echo "> cypm install currydoc" ; \
           fi \
         fi

${MANUALVERSION}: Makefile
	echo '\\newcommand{\\kicsversiondate}'         >  $@
	echo '{Version $(VERSION) of ${COMPILERDATE}}' >> $@

.PHONY: cleanmanual
cleanmanual:
	-cd docs/src && $(MAKE) clean

