## This is the Makefile for CS 3110 Problem Set 3 2014sp.
##
## targets are:
##   all   -- builds the solution, release, writeup and documentation
##   sol   -- builds the bkc solution
##   zip   -- zips the release directory
##   doc   -- generates the documentation html
##   clean -- remove all compiled release zip file, documentation directory,
##            and cm* files
##

default: zip check
all: sol zip check
zip: ps3.zip

sol:
	cd solution/mdg39 && \
	  cs3110 compile compress.ml

clean:
	git clean -fdX

.PHONY: default all zip sol clean check

RELEASE_FILES = \
	writeup/ps3.pdf \
	writeup/ps3.tex \
	writeup/images \
	release/ \
	data/Conrad.txt \
	data/Joyce.txt \
	data/writeup.pdf.huff \
	data/exam_solution_scan.jpg.huff \
	data/Boorman.txt.huff \
	doc/

ps3/%: %
	mkdir -p $(dir $@)
	cp -rT $^ $@

check: $(RELEASE_FILES:%=ps3/%)
	echo "checking release"
	cd ps3/release \
	  && cs3110 clean \
	  && for i in *.ml; do cs3110 compile $$i || break; done \
	  && rm -rf _build \
	  && cd ../ \
	  && grep "TODO" -r

ps3.zip: clean ps3/doc $(RELEASE_FILES:%=ps3/%)
	rm -rf $@
	zip -r $@ $^

writeup/ps3.pdf: force
	$(MAKE) -C writeup ps3.pdf

data/writeup.pdf: writeup/ps3.pdf
	cp $^ $@

data/%.huff: data/% sol
	cd solution/mdg39 && \
	  cs3110 run compress ../../$< > ../../$@

doc: $(wildcard release/*.mli)
	# TODO: this is terrible
	mkdir -p $@
	for i in $^; do cd release; cs3110 compile `basename $$i` || break; cd ..; done
	ocamldoc -I release/_build -d $@ $^ \
	         -warn-error -html -colorize-code
	rm -rf release/_build
	touch $@

force:

#
# vim: noet ts=8 sw=8 ai
#
