.PHONY: all clean

CAMLC      := ocamlfind ocamlc -g
CAMLOPT    := ocamlfind ocamlopt
CAMLLEX    := ocamllex
CAMLDEP    := ocamlfind ocamldep
GENERATED  := chop.ml
MODULES    := gSet gMap patricia signatures chop print indexMap atom
OBJECTS    := $(MODULES:=.cmo)
XBJECTS    := $(MODULES:=.cmx)
FILES      := alphaLib.cmi alphaLib.cmo alphaLib.cmx alphaLib.o

# Include this option if ocamlopt supports it (3.09 and later).
PACK       := -for-pack AlphaLib
PACK       := $(shell if $(CAMLOPT) $(PACK) 2>&1 | grep "unknown option" >/dev/null ; \
                      then echo ; else echo $(PACK) ; fi)

all: $(FILES)

alphaLib.cmi alphaLib.cmo: $(OBJECTS)
	$(CAMLC) -pack -o alphaLib.cmo $(OBJECTS)

alphaLib.cmx alphaLib.o: $(XBJECTS)
	$(CAMLOPT) -pack -o alphaLib.cmx $(XBJECTS)

install: all
	ocamlfind install alphaLib META signatures.ml $(FILES)

uninstall:
	ocamlfind remove alphaLib

.depend: $(wildcard *.ml *.mli) $(GENERATED)
	@/bin/rm -f $@
	$(CAMLDEP) *.ml *.mli > $@

ifneq ($(MAKECMDGOALS),clean)
-include .depend
endif

clean:
	/bin/rm -f alphaLib.*
	/bin/rm -f *.cmi *.cmx *.cmo *.o *~ .*~
	/bin/rm -f .depend

%.cmi: %.mli
	$(CAMLC) -c $<

%.cmo: %.ml
	$(CAMLC) -c $<

%.cmx %.o: %.ml
	$(CAMLOPT) $(PACK) -c $<

%.ml: %.mll
	@/bin/rm -f $@
	$(CAMLLEX) $<
	@/bin/chmod -w $@
