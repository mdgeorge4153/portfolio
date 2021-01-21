default: info
all:     zip writeup

info:
	@echo "This Makefile is for building the documentation and release files"
	@echo ""
	@echo "See README.md for information on building and running the solution"
	@echo ""
	@echo "Additional make tagets:"
	@echo "   - make zip:     release zip files, including generated documentation"
	@echo "   - make doc:     just the generated documentation"
	@echo "   - make writeup: the assignment writeup"

zip: ps5.zip

clean:
	git clean -fdX

RDIRS = $(shell find release -type d | grep -v _build)

doc: doc/async.mli doc/docs.odocl $(shell find release -name "*.mli" | grep -v _build)
	
	ocamlbuild -use-ocamlfind -package core -cflag -thread \
	           -I doc $(RDIRS:%=-I %) \
	           -docflag -hide -docflag Async.Std,Pervasives \
		   -no-links \
	           doc/docs.docdir/index.html
	mkdir -p ps5
	rm -rf ps5/doc
	mv _build/doc/docs.docdir ps5/doc
	cp doc/style.css ps5/doc
	rm -rf _build tmp.docdir tmp.odocl


writeup:
	$(MAKE) -C writeup ps5b.pdf
	cp writeup/ps5b.pdf .

ps5.zip: clean writeup doc
	cp -r release ps5
	
	mkdir ps5/writeup
	cp writeup/ps5b.tex writeup/ps5b.pdf ps5/writeup
	
	rm -f ps5.zip
	zip ps5.zip -r ps5

check: ps5.zip
	rm -rf tmp
	mkdir tmp
	cd tmp                                 \
	&& unzip ../ps5.zip                    \
	&& cd ps5/release                      \
	&& cs3110 compile controllerMain.ml    \
	&& cs3110 compile workerMain.ml        \
	&& cs3110 compile async/examples.ml    \
	&& cs3110 run controllerMain.ml -local wc ../writeup/ps5b.tex | tail \
	&& cd ../writeup                       \
	&& pdflatex ps5b.tex                   \
	&& cd ..                               \
	&& ! grep -R TODO                      \
	&& cd ..                               \
	&& rm -rf tmp

.PHONY: default doc all clean zip check writeup info

#
# vim: noet ts=8 sw=8
#
