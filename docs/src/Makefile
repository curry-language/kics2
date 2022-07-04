# To format the KiCS2 User Manual and other documents

HTMLFILES = $(DOCDIR)/markdown_syntax.html $(DOCDIR)/INSTALL.html \
            $(DOCDIR)/GITINSTALL.html $(DOCDIR)/RELNOTES.html \
            $(DOCDIR)/CurryStyleGuide.html

MANUAL = $(DOCDIR)/Manual.pdf

# Executable of CurryDoc:
CURRYDOC := $(shell command -v curry-doc 2>/dev/null)

# Executable of the markdown translator:
MD2PDF := $(shell command -v md2pdf 2>/dev/null)

.PHONY: pdf
pdf : manual.pdf

.PHONY: install
install: $(HTMLFILES)
	@if [ -x "$(CURRYDOC)" ] ; then \
	  $(MAKE) $(MANUAL) ; else \
	  echo "'curry-doc' not installed, skipping manual generation." ; \
	fi

.PHONY: clean
clean:
	rm -f markdown_syntax.tex
	rm -f *.aux *.idx *.ilg *.ind *.log *.out *.toc *.tpt

.PHONY: cleanall
cleanall: clean
	rm -f manual.pdf $(MANUAL) $(HTMLFILES)
	rm -f lib version.tex

$(MANUAL): manual.pdf
	rm -f $@
	cp $< $@

manual.pdf: $(wildcard *.tex *.bbl *.jpg) version.tex markdown_syntax.tex libdoc
	pdflatex manual
	bibtex manual
	pdflatex manual
	makeindex manual.idx
	pdflatex manual

# generate documentation for all libraries
.PHONY: libdoc
libdoc:
	cd $(LIBDIR) && $(MAKE) texdoc

syntax.tex: syntax.sty

# format markdown syntax description as LaTeX
markdown_syntax.tex: markdown_syntax.txt
	$(MD2PDF) --include --tex -o "$@" "$<"

# format markdown syntax description as HTML page
$(DOCDIR)/markdown_syntax.html: markdown_syntax.txt
	$(MD2PDF) -t "Markdown Syntax" --html -o "$@" "$<"

# format installation description as HTML page
$(DOCDIR)/INSTALL.html: $(ROOT)/INSTALL.txt
	$(MD2PDF) -t "KiCS2 Installation" --html -o "$@" "$<"

# format repository installation description as HTML page
$(DOCDIR)/GITINSTALL.html: $(ROOT)/GITINSTALL.txt
	$(MD2PDF) -t "KiCS2 Repository" --html -o "$@" "$<"

# format release notes as HTML page
$(DOCDIR)/RELNOTES.html: $(ROOT)/RELNOTES.txt
	$(MD2PDF) -t "KiCS2 Release Notes" --html -o "$@" "$<"

# format Curry style guide as HTML page
$(DOCDIR)/CurryStyleGuide.html: tooldocs/casc/curry-style-guide.md
	cd tooldocs/casc && $(MAKE)
	@if [ -f "tooldocs/casc/curry-style-guide.html" ] ; then cp tooldocs/casc/curry-style-guide.html $@ ; fi

