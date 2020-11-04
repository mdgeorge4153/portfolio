all: ps4.pdf

ps4.pdf: sshot.png

%.pdf: %.tex
	TEXINPUTS=../../../support/tex:$TEXINPUTS: pdflatex $<
	TEXINPUTS=../../../support/tex:$TEXINPUTS: pdflatex $<
