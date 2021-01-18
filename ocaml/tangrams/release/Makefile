default: all
all:     tangrams

CMOS     = numbers.cmo region.cmo geometry.cmo game.cmo
OCAMLC   = ocamlc
OCAMLOPT = ocamlopt
GLFLAGS  = -I $(shell ocamlfind query lablgl 2> /dev/null               \
                  || (ocamlfind query lablGL | sed 's/lablgl/lablGL/g') \
               ) lablglut.cma lablgl.cma

clean:
	rm -f *.cmo *.cmi tangrams gltest

top: $(CMOS)
	rlwrap ocaml nums.cma $^

tangrams: $(CMOS) ui.cmo main.ml
	ocamlc -o $@ $(GLFLAGS) nums.cma $^

gltest: gltest.ml
	ocamlc -o gltest $(GLFLAGS) $^

.depend: $(wildcard *.ml*)
	ocamldep $(FLAGS) $^ > .depend
include .depend

%.cmo: %.ml
	$(OCAMLC) $(GLFLAGS) -c $< -o $@

%.cmi: %.mli
	$(OCAMLC) $(GLFLAGS) -c $< -o $@

%.cmx: %.ml
	$(OCAMLOPT) $(GLFLAGS:.cma=.cmxa) -c $< -o $@

.phony: default all clean top

