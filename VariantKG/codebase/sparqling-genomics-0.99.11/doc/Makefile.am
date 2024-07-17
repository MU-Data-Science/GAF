# Copying and distribution of this file, with or without modification,
# are permitted in any medium without royalty provided the copyright
# notice and this notice are preserved.  This file is offered as-is,
# without any warranty.

AUTOMAKE_OPTIONS 	= subdir-objects
SUBDIRS                 = . 

docdir                  = ${pkgdatadir}
doc_NAME                = sparqling-genomics
doc_PDF                 = $(doc_NAME).pdf
doc_HTML                = $(doc_NAME).html
doc_TEX                 = $(doc_NAME).tex
doc_STYLE		= sparqling-genomics.sty
dist_doc_DATA =

if HAVE_PDFLATEX
dist_doc_DATA           += $(doc_PDF)

# Run pdflatex twice, so that the table of contents and references are
# properly set up.
$(doc_PDF):
	@$(PDFLATEX) $(doc_TEX) $(doc_STYLE) > /dev/null
	@$(BIBTEX) $(doc_NAME) > /dev/null
	@$(PDFLATEX) $(doc_TEX) $(doc_STYLE) > /dev/null
	@$(PDFLATEX) $(doc_TEX) $(doc_STYLE) > /dev/null
	@printf "Generated $(doc_PDF).\n"
endif

EXTRA_DIST              = $(doc_TEX) $(doc_STYLE)              \
                          command-line-tools.tex               \
                          figures/logo.pdf                     \
                          figures/logo.png                     \
                          figures/sg-auth-manager-scaleout.pdf \
                          figures/sg-web-dashboard.pdf         \
                          figures/sg-web-exploratory.pdf       \
                          figures/sg-web-import-rdf.pdf        \
                          figures/sg-web-project-details.pdf   \
                          figures/sg-web-query.pdf             \
                          implemented-ontologies.tex           \
                          information-management.tex           \
                          information-retrieval.tex            \
                          introduction.tex                     \
                          knowledge-graph.tex                  \
                          programming-with-sparql.tex          \
                          web-interface.tex

docs-clean:
	$(RM) -rf $(doc_NAME).aux			\
		  $(doc_NAME).bbl			\
		  $(doc_NAME).blg			\
		  $(doc_NAME).log			\
		  $(doc_NAME).out			\
		  $(doc_NAME).toc			\
		  $(doc_NAME).xref			\
		  $(doc_NAME).4ct			\
		  $(doc_NAME).4tc			\
		  $(doc_NAME).dvi			\
		  $(doc_NAME).idv			\
		  $(doc_NAME).tmp			\
		  $(doc_NAME).lg			\
		  $(doc_NAME).lof			\
		  $(doc_NAME).lot			\
		  $(doc_NAME).pdf                       \
		  $(doc_NAME).html                      \
		  texput.log

clean-local: docs-clean

# Make sure there is something to run.
all: $(doc_PDF)
