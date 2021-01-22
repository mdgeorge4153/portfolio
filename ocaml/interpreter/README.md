OCalf interpreter
=================

This is a programming project for CS 3110 (functional programming, 2015sp).
The students were required to implement an interpreter for a typed subset of
OCaml.  I completely redesigned the interpreter project that I inherited from
previous semesters.

Interpreter projects are a staple of functional programming courses.  There are
a few details of this implementation give it an extra bit of polish, which I
think improved the students' experience and learning outcomes:

* Previous iterations of the interpreter in CS 3110 interpreted a scheme-like
  language.  In contrast, this project used a subset of OCaml for the object
  language.  This saved the students the cognitive overhead of learning about
  and working with S-exprs.  I think it also helped reinforce their
  understanding of constructs like pattern matching and variants.

* The object language has a Hindley-Milner style type system, giving students an
  opportunity to build up their understanding of the OCaml type system.

* We provided tools to improve the students' development and debugging
  experience.  In particular, we provided a fully functional parser and
  pretty-printer.  The pretty-printer in particular has several unusual
  features that improved the students' lives significantly.  It outputs
  expressions with the fewest possible parentheses , and provides automatic
  indentation and syntax highlighting.  It also integrates nicely with the
  OCaml toplevel, allowing automatic printing during debugging and
  experimentation.  I think this probably saved the students a lot of time and
  effort that they would otherwise have spent poring through complicated
  output.  Perhaps a few of the students also learned the lesson that taking
  the time to make good debugging tools is worthwhile.

* There's also something fun that I added for my own personal amusement: the
  release code contains a lambda calculus interpreter written in the object
  language (see `meta.ml`).

Files in this directory
-----------------------

Here are the files of interest:

* `ps3.pdf` contains the project description.

* `release` contains the starter code provided to the students

* `solution/mdg39` contains my reference solution

* `testing/tests` contains the code we used to evaluate the students'
  implementations during grading.

and some of the other files included in this directory:

 * `parser` contains the ocamlyacc implementation of the parser.  For
   simplicity, we pregenerated the parser instead of shipping the yacc and lex
   files to the students.

* `resources` contains the reference material I used while writing the pretty
  printer


Building and running
--------------------

The instructions in `ps3.pdf` refer to the obsolete internal build script that
we provided the students.  I have replaced that infrastructure with dune
scripts to make it easier for you to build and run the code.

    cd solution/mdg39
    dune build
    dune runtest # run the provided examples and grading test suite

The project is fun to play with in the toplevel.  The file `top.ml` contains a
small test to play with, but the parser and printer make it easy to evaluate
your own small programs.

    utop
    > #use "top.ml";;
    > let x = Parser.parse_expr "if true then Hello 2 else World 2";;
    > print_value (eval x)

