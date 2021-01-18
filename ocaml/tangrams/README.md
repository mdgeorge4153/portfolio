Tangrams
========

This is a programming project for CS 3110 focusing on modular software design.
Students implement a large hierarchy of numeric types, and use them to implement
some interesting geometric algorithms.  We've provided a GUI that uses their
algorithms to play a tangrams game with a novel and satisfying drag-and-drop
system.

Files of note:
 * `ps4.pdf`   contains the complete assignment description
 * `release`   contains the starter code that was provided to the students
 * `sol/mdg39` contains my complete working solution

Building and running
--------------------

This project depends on lablgl, which you can install using opam:

    opam install lablgl

This may involve installing the freeglut library.  For example, on Ubuntu:

    sudo apt install freeglut3-dev

You can then build my solution by running `make` in the top level directory;
this will compile the files in `sol/mdg39` and create an
executable symlink called `tangrams` in the top-level directory.


