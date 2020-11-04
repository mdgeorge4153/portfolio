default: nums
all:     tangrams

CMOS    = numbers.cmo region.cmo releaseTypes.cmo numberUtils.cmo
GLFLAGS =

clean:
	rm -f *.cmo *.cmi tangrams gltest

top: $(CMOS)
	rlwrap ocaml $(GLFLAGS) nums.cma $^

nums: test_nums
	OCAMLRUNPARAM=b ./test_nums

test_nums: numbers.cmo releaseTypes.cmo numberUtils.cmo property_tests.cmo
	ocamlc -g -o $@ nums.cma $^

tangrams: $(CMOS) ui.cmo main.ml
	ocamlc -g -o $@ $(GLFLAGS) nums.cma $^

gltest: gltest.ml
	ocamlc -g -o gltest $(GLFLAGS) $^

releaseTypes.cmo: releaseTypes.ml
numberUtils.cmo:  numberUtils.ml

numbers.cmo:  numbers.ml
region.cmo:   numbers.cmo region.mli region.ml
geometry.cmo: numbers.cmo region.cmo geometry.mli geometry.ml
game.cmo:     numbers.cmo region.cmo geometry.cmo game.mli game.ml
ui.cmo:       numbers.cmo region.cmo geometry.cmo game.cmo ui.ml

releaseTypes.cmo: region.cmo releaseTypes.ml
property_tests.cmo: releaseTypes.cmo numberUtils.cmo property_tests.ml

smoke_test: $(CMOS)
	ocamlc -g -o $@ nums.cma $(CMOS) smoke_test.ml

%.cmo:
	ocamlc -g -c -o $@ $(GLFLAGS) nums.cma $^

.phony: default all clean top

