all: ps3.pdf

clean:
	-rm ps3.aux
	-rm ps3.log
	-rm ps3.out
	-rm ps3.pdf

%.pdf: %.tex
	pdflatex $*.tex
	pdflatex $*.tex

view: ps3.pdf
	evince ps3.pdf

.PHONY: all clean
