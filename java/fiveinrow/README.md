This is a programming project developed for CS 2110.  Students learn some
basics of designing nontrivial programs, develop a GUI, and different AI player
heuristics.

The students were provided with the files in the `release/` directory (as well
as generated javadocs for those files, and a generated HTML version of the
writeup in `writeup/writeup.md`).

Students in the class work within the eclipse development environment; the
release directory contains eclipse project files that they can load directly
into eclipse.

My solution is contained in the directory `solution/mdg39`.  There are eclipse
project files in that directory as well, so the easiest way to run the project
is by importing it into eclipse and running it there.  Alternatively, you can
compile and run the GUI at the command line:

    cd solution/mdg39
    mkdir bin
    javac -sourcepath src -d bin src/gui/Main.java -classpath org.eclipse.jdt.annotation_2.1.0.v20160418-1457.jar 
    java -classpath org.eclipse.jdt.annotation_2.1.0.v20160418-1457.jar:bin gui.Main

