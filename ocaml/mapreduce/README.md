MapReduce
=========

This is a programming project for CS 3110 (functional programming, spring 2014)
that focuses on event-driven concurrent programming using Jane Street's Async
library.  This project was the first async-based project in the history of CS
3110; previous iterations of the course had the students implement MapReduce
using threads.

Files of note:
 * `ps5b.pdf`   contains the complete assignment description
 * `release`    contains the starter code that was provided to the students
 * `sol/mdg39`  contains my complete working solution (removed from public repo;
   ask me for a copy if interested)

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
This documentation is somewhat out of date, since OCaml and Async have changed
a fair amount since 2014.

Building and running
--------------------

The instructions for building and running things in `ps5b.pdf` depend on a
special-purpose build tool provided to the students.  I have added dune build
scripts to the release code and my solutions.

This project depends on several opam packages.  After installing opam, you can
install the additional dependencies by running:

    opam install dune core async ocamlnet cohttp-async

You can then build my solution using dune:

    cd solution/mdg39
    dune build

The project has a client server architecture.  To run the servers (also called
workers), execute the `workerMain` file:

    dune exec ./workerMain.exe  <port>

This listens on the provided `port`.  The client (also called the controller)
tries to connect to workers at the addresses listed in `addresses.txt`.

For a quick test, run two workers on ports 31100 and 31101.  Then you can run
the controller by providing an app to run:

    # list available apps and command line options
    dune exec ./controllerMain.exe -- --help

    # run the ocoogle toy search engine
    dune exec ./controllerMain.exe -- ocoogle 100 http://www.cs.cornell.edu/


