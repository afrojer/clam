TGT = clam

LEX_SRC = scanner.mll
YACC_SRC = parser.mly
ML_SRC = clam.ml
ML_INC = ast.mli

# env config
OBJDIR = obj
OCC = $(shell which ocamlc)
OCLEX = $(shell which ocamllex)
OYACC = $(shell which ocamlyacc)
OCAML_OPTS =
OCLEX_OPTS = -q
OYACC_OPTS =

### no more editing ###

OCAML_OPTS += -I $(OBJDIR) -I $(OBJDIR)/lex -I $(OBJDIR)/yacc

LEX_ML   := $(LEX_SRC:%.mll=$(OBJDIR)/lex/%.ml)
YACC_ML  := $(YACC_SRC:%.mly=$(OBJDIR)/yacc/%.ml)
YACC_MLI := $(YACC_SRC:%.mly=$(OBJDIR)/yacc/%.mli)

ML_OINC  := $(ML_INC:%.mli=$(OBJDIR)/%.cmi)
ML_OINC  += $(YACC_MLI:%.mli=%.cmi)
ML_OBJ   += $(YACC_ML:%.ml=%.cmo)
ML_OBJ   += $(LEX_ML:%.ml=%.cmo)
ML_OBJ   += $(ML_SRC:%.ml=$(OBJDIR)/%.cmo)

ML_DEP   := $(ML_INC:%.mli=$(OBJDIR)/%.mli.dep)
ML_DEP   += $(YACC_MLI:%.mli=%.mli.dep)
ML_DEP   += $(YACC_ML:%.ml=%.ml.dep)
ML_DEP   += $(LEX_ML:%.ml=%.ml.dep)
ML_DEP   += $(ML_SRC:%.ml=$(OBJDIR)/%.ml.dep)

all: $(TGT)
	@echo "done."

$(OBJDIR)/lex/%.ml: %.mll
	@echo "[OCAML LEX] $*.mll"
	@$(OCLEX) $(OCLEX_OPTS) -o $@ $<

$(OBJDIR)/yacc/%.ml $(OBJDIR)/yacc/%.mli: %.mly
	@echo "[OCAML YACC] $*.mly"
	@$(OYACC) $(OYACC_OPTS) -b $(OBJDIR)/yacc/$* $<

$(OBJDIR)/%.cmi %.cmi: %.mli
	@echo "[OCAML I] $*.mli"
	@$(OCC) $(OCAML_OPTS) -c -o $@ $<

$(OBJDIR)/%.cmo %.cmo: %.ml
	@echo "[OCAML C] $<"
	@$(OCC) $(OCAML_OPTS) -c -o $@ $<

$(TGT): $(ML_DEP) $(ML_OINC) $(ML_OBJ)
	@echo "[OCAML L] $(TGT)"
	@$(OCC) -o $(TGT) $(ML_OBJ)

clean:
	@echo "[CLEAN]"
	@rm -f $(ML_OBJ)
	@rm -f $(ML_DEP)
	@rm -f $(LEX_ML)
	@rm -f $(YACC_ML) $(YACC_MLI)
	@rm -f $(ML_OBJ:%.cmo=%.cmi)
	@rm -f $(ML_OBJ:%.cmo=%.cmx)
	@rm -f $(ML_OBJ:%.cmo=%.o)

distclean: clean
	@echo "[RM] $(TGT)"
	@rm -rf "$(OBJDIR)"
	@rm -f "$(TGT)"

%.dep:
	@if [ ! -d "$(shell dirname $@)" ]; then \
		mkdir -p "$(shell dirname $@)" >/dev/null 2>&1; fi
	@touch "$@"

.PHONY: all clean distclean

