all: ps4-release.zip

FILES = \
	release/CHANGELOG.txt \
	release/game.ml \
	release/game.mli \
	release/geometry.ml \
	release/geometry.mli \
	release/gltest.ml \
	release/main.ml \
	release/Makefile \
	release/numbers.ml \
	release/README.txt \
	release/region.ml \
	release/region.mli \
	release/test_regions.ml \
	release/ui.ml \
	writeup/ps4.pdf

writeup/ps4.pdf: writeup/ps4.tex writeup/sshot.png
	$(MAKE) -C writeup ps4.pdf

ps4-release.zip: $(FILES)
	rm -f $@
	zip $@ $(FILES)

clean:
	git clean -fdX

.phony: all clean

