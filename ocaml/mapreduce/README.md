MapReduce
=========

This is a programming project for CS 3110 (functional programming) that focuses
on event-driven concurrent programming using Jane Street's Async library.  This
project was the first async-based project in the history of CS 3110; previous
iterations of the course had the students implement MapReduce using threads.

For this project, students were required to implement a controller that
coordinates several remote workers to complete a MapReduce task.  In addition,
the students were required to implement portions of the infrastructure for a
distributed implementation of the MapReduce design.  In addition, they were
required to implement several applications that made use of their infrastructure
to perform distributed computations.

In addition to give students experience with concurrent programming,
implementing MapReduce gives students additional appreciation for programming
in a functional style - by expressing their computations as folds, they are
able to build infrastructure to distribute the application while keeping
the application logic simple.

Files of note:
 * `ps5b.pdf`  contains the complete assignment description
 * `release`   contains the starter code provided to the students
 * `solution/mdg39` contains my reference implementation

We also generated specialized documentation of the Async library tailored to
CS 3110 and this project.  You can view the generated documentation for the project
[on the CS3110 website](https://www.cs.cornell.edu/courses/cs3110/2014sp/hw/5/doc/);
in particular, see the generated
[Async documentation](https://www.cs.cornell.edu/courses/cs3110/2014sp/hw/5/doc/Async.Std.html).

Building and running
--------------------

TODO: instructions

opam install dune core async ocamlnet cohttp-async


