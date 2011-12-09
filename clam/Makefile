TGT = clam

# Binaries created by clam compiler (for 'make clean' to remove)
OUT_BIN = *.out

# The top-level OCaml source file
ML_TOP   = clam.ml

# The rest of the source files
ML_SRC   = clamtypes.ml environ.ml clamsys.ml backend.ml parse_util.ml verifier.ml printer.ml
ML_INC   = ast.mli
LEX_SRC  = scanner.mll
YACC_SRC = parser.mly

# extlib from: http://code.google.com/p/ocaml-extlib/
ML_SRC += extlib/enum.ml extlib/std.ml extlib/extString.ml
ML_INC += extlib/enum.mli extlib/std.mli extlib/extString.mli

STBLIB_SRC = libstb/stb_image_write.c libstb/stb_image_read.c

# env config
OBJDIR = obj
OCC = $(shell which ocamlc)
OCLEX = $(shell which ocamllex)
OYACC = $(shell which ocamlyacc)
OCAMLDEP = $(shell which ocamldep)
OCAML_CINC  = -I extlib
OCAML_COPTS = -g
OCAML_DEPOPTS = $(OCAML_CINC)
OCAML_LOPTS = -g unix.cma
OCLEX_OPTS  = -q
OYACC_OPTS  =

CC = $(shell which gcc)
CFLAGS = -O3 -fomit-frame-pointer -fPIC -DSTB_IMAGE_WRITE_IMPLEMENTATION

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ###
###              STOP: DO NOT EDIT BELOW THIS LINE              ###
### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ###

all: $(TGT)
	@echo "done."

OCAML_CINC  += -I $(OBJDIR) -I $(OBJDIR)/lex -I $(OBJDIR)/yacc -I $(OBJDIR)/extlib
OCAML_COPTS += $(OCAML_CINC)

LEX_ML   := $(LEX_SRC:%.mll=$(OBJDIR)/lex/%.ml)
YACC_ML  := $(YACC_SRC:%.mly=$(OBJDIR)/yacc/%.ml)
YACC_MLI := $(YACC_SRC:%.mly=$(OBJDIR)/yacc/%.mli)

ML_OINC  := $(ML_INC:%.mli=$(OBJDIR)/%.cmi)
ML_OINC  += $(YACC_MLI:%.mli=%.cmi)

STBLIB_OBJ := $(STBLIB_SRC:%.c=$(OBJDIR)/%.o)

LORDER_FILE = $(OBJDIR)/link_order

# dirty trick: let Make do my link-order for me :-)
add_dep = \
  $(shell E=`which echo`; \
	  if [ ! -f `dirname $(LORDER_FILE)` ]; then \
	    mkdir -p `dirname $(LORDER_FILE)` >/dev/null 2>&1; touch $(LORDER_FILE); fi; \
	  CURR=`cat $(LORDER_FILE)`; \
	  if [ "$(2)" = "clear" ]; then $$E "" > $(LORDER_FILE); \
	  elif [ ! -z "$(1)" ]; then \
	    EXISTS=`echo $$CURR | sed -E 's:.*(^| )$(1)($$| ).*:YES:'`; \
	    if [ ! "$$EXISTS" = "YES" ]; then \
	      $$E -n "$(1:%.ml.dep=%.cmo) " >> $(LORDER_FILE); \
	    fi; \
	  fi; \
   )

get_link_order = $(shell cat $(LORDER_FILE))

prepdir = \
  $(shell mkdir -p $(OCAML_CINC:-I%=%) >/dev/null 2>&1; \
	  if [ ! -d `dirname $(1)` ]; then \
	    mkdir -p `dirname $(1)` >/dev/null 2>&1; fi \
   )

ML_OBJ := $(ML_SRC:%.ml=$(OBJDIR)/%.cmo)
ML_OBJ += $(filter-out $(ML_OBJ),$(YACC_ML:%.ml=%.cmo))
ML_OBJ += $(filter-out $(ML_OBJ),$(LEX_ML:%.ml=%.cmo))
ML_OBJ += $(filter-out $(ML_OBJ),$(ML_TOP:%.ml=$(OBJDIR)/%.cmo))

ML_DEP     = $(ML_OINC:%.cmi=%.mli.dep)
ML_DEP    += $(ML_OBJ:%.cmo=%.ml.dep)

$(OBJDIR)/lex/%.ml: %.mll
	$(call add_dep,"","clear")
	@echo "[OCAML  LEX] $*.mll"
	@$(OCLEX) $(OCLEX_OPTS) -o $@ $<

$(OBJDIR)/yacc/%.ml $(OBJDIR)/yacc/%.mli: %.mly
	@echo "[OCAML YACC] $*.mly"
	@$(OYACC) $(OYACC_OPTS) -b $(OBJDIR)/yacc/$* $<

$(OBJDIR)/%.cmi %.cmi: %.mli
	@echo "[OCAML    I] $*.mli"
	@$(call add_dep,$(findstring $(@:%.cmi=%.cmo),$(ML_OBJ)))
	@$(OCC) $(OCAML_COPTS) -c -o $@ $<

$(OBJDIR)/%.cmo %.cmo: %.ml
	@echo "[OCAML    C] $<"
	@$(call add_dep,$@)
	@$(OCC) $(OCAML_COPTS) -c -o $@ $<

$(OBJDIR)/%.o: %.c
	$(call prepdir,$@)
	@echo "[CC        ] $<"
	@$(CC) $(CFLAGS) -c -o $@ $<

$(TGT).a: $(STBLIB_OBJ)
	@echo "[AR        ] $@"
	@$(AR) -rs $@ $(STBLIB_OBJ)

$(TGT): $(TGT).a $(ML_DEP) $(ML_OINC) $(ML_OBJ)
	@echo "[OCAML    L] $(TGT)"
	@$(OCC) -o $(TGT) $(OCAML_LOPTS) $(call get_link_order)

clean:
	@echo "  CLEAN"
	@rm -fv $(OUT_BIN)
	@rm -f $(ML_OBJ)
	@rm -f $(ML_OINC)
	@rm -f $(ML_DEP)
	@rm -f $(LEX_ML)
	@rm -f $(YACC_ML) $(YACC_MLI)
	@rm -f $(STBLIB_OBJ)
	@rm -f $(ML_OBJ:%.cmo=%.cmi)
	@rm -f $(ML_OBJ:%.cmo=%.cmx)
	@rm -f $(ML_OBJ:%.cmo=%.o)

distclean: clean
	@echo "  RM    $(TGT)"
	@rm -rf "$(OBJDIR)"
	@rm -rf "$(TGT).a"
	@rm -f "$(TGT)"

$(OBJDIR)/%.mli.dep: %.mli
	$(call prepdir,$@)
	$(shell $(OCAMLDEP) $(OCAML_DEPOPTS) -intf $< |sed 's/$(OBJDIR)\///g; s#\([^. ]*\)\.\(cm[iox]\)#$(OBJDIR)/\1.\2#g;' > $@)

$(OBJDIR)/%.mli.dep: $(OBJDIR)/%.mli
	$(call prepdir,$@)
	$(shell $(OCAMLDEP) $(OCAML_DEPOPTS) -intf $< |sed 's/$(OBJDIR)\///g; s#\([^. ]*\)\.\(cm[iox]\)#\1.\2#g;' > $@)

$(OBJDIR)/%.ml.dep: %.ml
	$(call prepdir,$@)
	$(shell $(OCAMLDEP) $(OCAML_DEPOPTS) -impl $< |sed 's/$(OBJDIR)\///g; s#\([^. ]*\)\.\(cm[iox]\)#$(OBJDIR)/\1.\2#g;' > $@)

$(OBJDIR)/%.ml.dep: $(OBJDIR)/%.ml
	$(call prepdir,$@)
	$(shell $(OCAMLDEP) $(OCAML_DEPOPTS) -impl $< |sed 's/$(OBJDIR)\///g; s#\([^. ]*\)\.\(cm[iox]\)#\1.\2#g;' > $@)

test: $(TGT)
	@./tests/all.test

ifneq ($(MAKECMDGOALS),clean)
  ifneq ($(MAKECMDGOALS),distclean)
    -include $(ML_DEP)
  endif
endif

# a little heavy-handed, but this prevents the
# automatic delection of parser/scanner ML files
.SECONDARY: $(ML_OBJ:%.cmo=%.ml)

.PHONY: all clean distclean

