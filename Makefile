default: all
all: mdgeorge-portfolio.zip

SUBMODULES = $(shell git submodule -q foreach 'echo $${sm_path}' | grep -v fabric)
DIRECTORY  = mdgeorge-portfolio

$(DIRECTORY): .git/refs/heads/main
	rm -rf $(DIRECTORY)
	git clone . $(DIRECTORY)
	echo "SUBMODULES: " $(SUBMODULES)
	git -C $(DIRECTORY) submodule update --init $(SUBMODULES)

$(DIRECTORY).zip: $(DIRECTORY)
	rm -f $@
	zip --symlinks -r $@ $^

$(DIRECTORY).tar.gz: $(DIRECTORY)
	tar -xzf $@ $^

.PHONY: all default check
