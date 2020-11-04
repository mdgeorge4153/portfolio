default: all
all:     tangrams

CMOS    = numbers.cmo region.cmo geometry.cmo game.cmo ui.cmo
GLFLAGS = -I $(shell ocamlfind query lablgl 2> /dev/null               \
                 || (ocamlfind query lablGL | sed 's/lablgl/lablGL/g') \
              ) lablglut.cma lablgl.cma

clean:
	rm -f *.cmo *.cmi tangrams gltest geotest

top: $(CMOS)
	rlwrap ocaml $(GLFLAGS) nums.cma $^

tangrams: $(CMOS) ui.cmo main.ml
	ocamlc -o $@ $(GLFLAGS) nums.cma $^

gltest: gltest.ml
	ocamlc -o gltest $(GLFLAGS) $^

geotest: numbers.cmo region.cmo geometry.cmo geometry_sol.cmo test_geometry.ml
	ocamlc -o geotest nums.cma $^

numbers.cmo:  numbers.ml
region.cmo:   numbers.cmo region.mli region.ml
geometry.cmo: numbers.cmo region.cmo geometry.mli geometry.ml
geometry_sol.cmo: numbers.cmo region.cmo geometry_sol.mli geometry_sol.ml
game.cmo:     numbers.cmo region.cmo geometry.cmo game.mli game.ml
ui.cmo:       numbers.cmo region.cmo geometry.cmo game.cmo ui.ml

%.cmo:
	ocamlc -c -o $@ $(GLFLAGS) nums.cma $^

.phony: default all clean top

