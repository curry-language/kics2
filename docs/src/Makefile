# To format the KiCS2 User Manual and other documents

MANUAL = $(DOCDIR)/Manual.pdf

# Executable of CurryDoc:
CURRYDOC := $(shell which curry-doc)

.PHONY: pdf
pdf : manual.pdf

.PHONY: install
install:
	@if [ -x "$(CURRYDOC)" ] ; then \
	  $(MAKE) $(MANUAL) ; else \
	  echo "'curry-doc' not installed, skipping manual generation." ; \
	fi

.PHONY: clean
clean:
	rm -f *.aux *.idx *.ilg *.ind *.log *.out *.toc *.tpt

.PHONY: cleanall
cleanall: clean
	rm -f manual.pdf $(MANUAL) $(HTMLFILES)
	rm -f lib version.tex

$(MANUAL): manual.pdf
	rm -f $@
	cp $< $@

manual.pdf: $(wildcard *.tex *.bbl *.jpg) version.tex libdoc
	pdflatex manual
	bibtex manual
	pdflatex manual
	makeindex manual.idx
	pdflatex manual

# generate documentation for all libraries
.PHONY: libdoc
libdoc:
	#cd ../../lib && $(MAKE) texdoc
	$(MAKE) -C $(ROOT) texdoc

syntax.tex: syntax.sty
