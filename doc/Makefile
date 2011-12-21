
# the source .tex
SRC = paper
SRCTEX = $(SRC).tex
SRCBIB = $(SRC)*.bib
# additional dependencies
GEN = clam.gitlog.tex

# commands
BIBTEX = bibtex
DVILATEX = latex
DVIPDF = dvipdfm
DVIPS = dvips
PDFLATEX = pdflatex
PDF2PS = pdf2ps
PS2EPS = ps2eps

RM = rm -f

WHICHPS = $(DVIPS)
WHICHLATEX = $(PDFLATEX)
PSOPTS = -P cmz -o $@ -t letter
LATEXOPTS = -synctex=1 -interaction=nonstopmode
DVIOPTS =

SRCARG=
ifeq ($(BOOK)$(PROPOSAL)$(REFMAN),)
  $(warning Defaulting to Final Report format [BOOK=1])
  BOOK=1
endif

ifeq ($(BOOK),1)
  SRCARG := $(SRCARG)\let\ifbook\iftrue
else
  SRCARG := $(SRCARG)\let\ifbook\iffalse
endif
ifeq ($(PROPOSAL),1)
  SRCARG := $(SRCARG)\let\ifproposal\iftrue
  NOBIB=1
else
  SRCARG := $(SRCARG)\let\ifproposal\iffalse
endif
ifeq ($(REFMAN),1)
  SRCARG := $(SRCARG)\let\ifrefman\iftrue
else
  SRCARG := $(SRCARG)\let\ifrefman\iffalse
endif

#
# Do not modify below this line!
#

# compile the final SRCARG value
SRCARG := $(SRCARG)\input{$(SRC).tex}

# force a dependency on all data files.
DEP := $(wildcard *.tex)
RESULTS := $(wildcard graphs/*.pdf)
FIGURES := $(wildcard figures/*.pdf)

PDFDEPEND = $(DEP) $(RESULTS) $(FIGURES)
DVIDEPEND = $(DEP) $(RESULTS:%.pdf=%.eps) $(FIGURES:%.pdf=%.eps)

FORCE = .silly_file_to_force_rebuild

.PHONY: all check_tools clean distclean force do_force $(GEN) pdf

all: check_tools $(FORCE) $(SRC).pdf

$(FORCE): check_tools
	@if [ ! -e $(FORCE) ]; then echo "" > $(FORCE); fi

check_tools: TOOLS = $(BIBTEX) $(DVILATEX) $(DVIPDF) $(PDF2PS) $(PS2EPS) $(WHICHPS) $(WHICHLATEX)
check_tools:
	@echo "Checking build tools..."
	$(foreach tool,$(TOOLS),$(if $(shell which $(tool)),,$(error You seem to be missing $(tool)!)))

do_force: check_tools
	@echo "Forcing re-build"
	@touch Makefile $(DEPEND) $(SRC).bib
	@date > $(FORCE)

force: do_force all
	@echo "Re build complete."

%.eps: %.pdf
	@echo "[PDF->EPS] $< -> $@"
	@$(PDF2PS) -dLanguageLevel=3 $< $*.ps
	@$(PS2EPS) -F -a $*.ps >> $(SRC).build.log
	@$(RM) $*.ps

$(SRC).pdf: $(FORCE) $(PDFDEPEND) $(SRC).bbl Makefile
	@$(WHICHLATEX) $(LATEXOPTS) "$(SRCARG)"

$(SRC).dvi: $(FORCE) $(DVIDEPEND) $(SRC).bbl Makefile
	@$(DVILATEX) $(DVIOPTS) "$(SRCARG)"

$(SRC).ps: $(FORCE) $(SRC).dvi $(SRC).bbl Makefile
	@$(WHICHPS) $(PSOPTS) "$(SRCARG)"

clam.gitlog.tex:
	@echo "[Generating Git log]"
	@./gen_gitshortlog.pl > $@

ifeq ($(NOBIB),1)
$(SRC).bbl: $(GEN)
	@echo "[no bib]"
else
$(SRC).bbl: $(FORCE) $(SRC).bib Makefile $(GEN)
	@echo "[$(WHICHLATEX)]: first pass to generate *.aux files"
	@$(WHICHLATEX) $(LATEXOPTS) "$(SRCARG)" 2>&1 >> $(SRC).build.log
	@echo "[$(BIBTEX)] $(SRC)"
	@$(BIBTEX) $(SRC) 2>&1 >> $(SRC).build.log
	@echo "[$(WHICHLATEX)]: second pass to generate *.bib files"
	@$(WHICHLATEX) $(LATEXOPTS) "$(SRCARG)" 2>&1 >> $(SRC).build.log
endif

pdf: $(SRC).pdf
dvi: $(SRC).dvi
ps: $(SRC).ps
bib: $(SRC).bbl

# clean up everything except .pdf
clean:
	$(RM) *.aux *.dvi *.log *.bbl *.blg *.build.log *~ *.out *.toc
	$(RM) $(SRC).pdfsync $(SRC).synctex.gz

# clean up everything
distclean: clean
	$(RM) graphs/*.ps graphs/*.eps figures/*.ps figures/*.eps
	$(RM) $(SRC).pdf $(SRC).ps
