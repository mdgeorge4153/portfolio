all: ps3.pdf

clean:
	git clean -fdX

ps4.pdf: *.tex

%.pdf: %.tex
	pdflatex $^
	pdflatex $^

.PHONY: all clean

# vim: noet
