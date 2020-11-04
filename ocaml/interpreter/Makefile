

RELEASE_FILES =             \
  $(wildcard release/*.ml)  \
  $(wildcard release/*.mli) \
  release/.ocamlinit        \
  release/parser.ml         \
  writeup/ps3.pdf

ps3.zip: $(RELEASE_FILES)
	rm -f $@
	zip $@ $^

writeup/ps3.pdf: writeup/ps3.tex
	$(MAKE) -C writeup

release/parser.ml: parser/head.ml parser/gen.ml parser/lexer.ml parser/tail.ml
	cat $^ > $@

parser/gen.ml: parser/gen.mly
	ocamlyacc -v $<

parser/lexer.ml: parser/lexer.mll
	ocamllex $<

clean:
	git clean -fdX
