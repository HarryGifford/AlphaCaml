.PHONY: clean

CAMLC      := ocamlfind ocamlc -g
CAMLDEP    := ocamlfind ocamldep
CAMLLEX    := ocamllex
CAMLYACC   := ocamlyacc
GENERATED  := parser.ml parser.mli lexer.ml
MODULES    := error syntax strings parser lexer version front kind generator
OBJECTS    := $(MODULES:=.cmo)
EXECUTABLE := alphaCaml

$(EXECUTABLE): $(OBJECTS)
	$(CAMLC) -o $(EXECUTABLE) $(OBJECTS)

.depend: $(wildcard *.ml *.mli) $(GENERATED)
	@/bin/rm -f $@
	$(CAMLDEP) *.ml *.mli > $@

ifneq ($(MAKECMDGOALS),clean)
-include .depend
endif

ifeq ($(strip $(PREFIX)),)
install uninstall:
	@echo "Cannot (un)install $(EXECUTABLE): the PREFIX variable is undefined." && false
else
install: $(EXECUTABLE)
	mkdir -p $(PREFIX)/bin/
	install $(EXECUTABLE) $(PREFIX)/bin/$(EXECUTABLE)
uninstall:
	/bin/rm -f $(PREFIX)/bin/$(EXECUTABLE)
endif


clean:
	/bin/rm -f $(EXECUTABLE) $(GENERATED)
	/bin/rm -f *.cmi *.cmx *.cmo *.ppo *.o *.obj *~ .*~
	/bin/rm -f .depend

%.cmi: %.mli
	$(CAMLC) -c $<

%.cmo: %.ml
	$(CAMLC) -c $<

%.ml %.mli: %.mly
	@/bin/rm -f $*.ml $*.mli
	$(CAMLYACC) $<
	@/bin/chmod -w $*.ml $*.mli

%.ml: %.mll
	@/bin/rm -f $@
	$(CAMLLEX) $<
	@/bin/chmod -w $@
