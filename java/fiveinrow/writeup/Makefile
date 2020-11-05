default: all
all: javadoc a5release.zip writeup.html

clean:
	git clean -fdX

SOURCE=$(wildcard a5/src/**/*.java)
RELEASE=$(wildcard a5/**)

javadoc: $(SOURCE)
	javadoc $^ -d $@ \
	    -sourcepath a5/src \
	    -classpath a5/org.eclipse.jdt.annotation_2.1.0.v20160418-1457.jar \
	    -doctitle "Five in a row javadoc" \
	    -public -noindex -use \
	    -source 1.8
	touch $@

a5release.zip: clean $(RELEASE)
	zip -r a5release.zip a5

writeup.html: writeup.md
	pandoc -s -c style.css writeup.md > writeup.html

.PHONY: default all clean

