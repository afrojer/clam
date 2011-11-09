TGT = clam

# Binaries created by clam compiler (for 'make clean' to remove)
OUT_BIN = *.clam

LEX_SRC = scanner.mll
YACC_SRC = parser.mly
# NOTE: keep clam.ml at the back of the list!
ML_SRC = clamtypes.ml clamsys.ml backend.ml clam.ml
ML_INC = ast.mli

# env config
OBJDIR = obj
OCC = $(shell which ocamlc)
OCLEX = $(shell which ocamllex)
OYACC = $(shell which ocamlyacc)
OCAMLDEP = $(shell which ocamldep)
OCAML_COPTS =
OCAML_LOPTS = unix.cma
OCLEX_OPTS  = -q
OYACC_OPTS  =

### no more editing ###

OCAML_COPTS += -I $(OBJDIR) -I $(OBJDIR)/lex -I $(OBJDIR)/yacc

LEX_ML   := $(LEX_SRC:%.mll=$(OBJDIR)/lex/%.ml)
YACC_ML  := $(YACC_SRC:%.mly=$(OBJDIR)/yacc/%.ml)
YACC_MLI := $(YACC_SRC:%.mly=$(OBJDIR)/yacc/%.mli)

ML_OINC  := $(ML_INC:%.mli=$(OBJDIR)/%.cmi)
ML_OINC  += $(YACC_MLI:%.mli=%.cmi)
ML_OBJ   += $(YACC_ML:%.ml=%.cmo)
ML_OBJ   += $(LEX_ML:%.ml=%.cmo)
ML_OBJ   += $(ML_SRC:%.ml=$(OBJDIR)/%.cmo)

ML_DEP   := $(ML_INC:%.mli=$(OBJDIR)/%.mli.dep)
#ML_DEP   += $(YACC_ML:%.ml=%.zml.dep)
#ML_DEP   += $(LEX_ML:%.ml=%.zml.dep)
ML_DEP   += $(ML_SRC:%.ml=$(OBJDIR)/%.ml.dep)

prepdir = \
  $(shell if [ ! -d "$(shell dirname $(1))" ]; then \
          mkdir -p "$(shell dirname $(1))" >/dev/null 2>&1; fi)

all: $(TGT)
	@echo "done."

$(OBJDIR)/lex/%.ml: %.mll
	@echo "[OCAML  LEX] $*.mll"
	$(call prepdir,$@)
	@$(OCLEX) $(OCLEX_OPTS) -o $@ $<

$(OBJDIR)/yacc/%.ml $(OBJDIR)/yacc/%.mli: %.mly
	@echo "[OCAML YACC] $*.mly"
	$(call prepdir,$@)
	@$(OYACC) $(OYACC_OPTS) -b $(OBJDIR)/yacc/$* $<

$(OBJDIR)/%.cmi %.cmi: %.mli
	@echo "[OCAML    I] $*.mli"
	@$(OCC) $(OCAML_COPTS) -c -o $@ $<

$(OBJDIR)/%.cmo %.cmo: %.ml
	@echo "[OCAML    C] $<"
	@$(OCC) $(OCAML_COPTS) -c -o $@ $<

$(TGT): $(ML_DEP) $(ML_OINC) $(ML_OBJ)
	@echo "[OCAML    L] $(TGT)"
	$(OCC) -o $(TGT) $(OCAML_LOPTS) $(ML_OBJ)

clean:
	@rm -fv $(OUT_BIN)
	@echo "  CLEAN"
	@rm -f $(ML_OBJ)
	@rm -f $(ML_OINC)
	@rm -f $(ML_DEP)
	@rm -f $(LEX_ML)
	@rm -f $(YACC_ML) $(YACC_MLI)
	@rm -f $(ML_OBJ:%.cmo=%.cmi)
	@rm -f $(ML_OBJ:%.cmo=%.cmx)
	@rm -f $(ML_OBJ:%.cmo=%.o)

distclean: clean
	@echo "  RM    $(TGT)"
	@rm -rf "$(OBJDIR)"
	@rm -f "$(TGT)"

$(OBJDIR)/%.mli.dep: %.mli
	$(call prepdir,$@)
	$(shell $(OCAMLDEP) -intf $< |sed 's#\([^. ]\+\)\.\(cm[iox]\)#$(OBJDIR)/\1.\2#g;' > $@)

$(OBJDIR)/%.ml.dep: %.ml
	$(call prepdir,$@)
	$(shell $(OCAMLDEP) -impl $< |sed 's#\([^. ]\+\)\.\(cm[iox]\)#$(OBJDIR)/\1.\2#g;' > $@)

test: $(TGT)
	@./tester.sh

-include $(ML_DEP)

.PHONY: all clean distclean

