all: ps5a.pdf ps5b.pdf karma.pdf

%.pdf: %.tex
	pdflatex $<
	pdflatex $<

view: ps5.pdf
	evince $<
          
clean:
	git clean -fdX

.PHONY: view clean

#
# vim: noet
#
