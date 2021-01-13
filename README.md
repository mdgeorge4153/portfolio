Michael George's Programming Portfolio
======================================

This repository contains several samples of my work, demonstrating a variety of
programming projects that I've worked on in various languages and settings.

This document gives a brief overview of each of the projects contained herein.
The projects that are open source are referenced as submodules so that you can
view the version history and other related public information.

Here is a brief outline of the projects; further details are listed below.

Large research projects:

 * `java/fabric`: an extension of Java with language-level support for
   distributed computing, mobile code, and strong information flow control.

Projects developed for courses:

 * `ocaml/tangrams`: a project that brings together modular software design,
   arbitrary-precision arithmetic, and computational geometry to implement a
   novel drag-and-drop algorithm

 * `ocaml/interpreter`: an interpreter implementation project for a subset of
   OCaml, including type inference

 * `ocaml/mapreduce`: an implementation of a distributed map/reduce platform
   using async.

Fabric
======

Most of my graduate research was conducted in the context of the Fabric
project.  I was one of four lead developrs of the Fabric language and system.
Fabric is an extension of Java with language-level support for distributed
computing, mobile code, and strong information flow control.

The public fabric repository is contained in the `java/fabric` directory.  As
one of the lead developers, I've made significant contributions to all portions
of the language, system, and example application implementations (296kloc
according to github).

My Fabric related publications are contained in the `pubs` directory:

`pubs/dissert.pdf`
: Trust, Authority, and Information Flow in Secure Distributed Systems.
: Doctoral dissertation. Cornell University 2020.

`pubs/fabric-jcs.pdf`
: Fabric: Building Open Distributed Systems Securely by Construction.
: J. Computer Security, 2017.
  Jed Liu, Owen Arden, Michael D. George, and Andrew C. Myers.

`pubs/mobile-oakland12.pdf`
: Sharing Mobile Code Securely with Information Flow Control.
: IEEE Symp. on Security and Privacy, May 2012.
  Owen Arden, Michael D. George, Jed Liu, K. Vikram, Aslan Askarov, and Andrew C. Myers.

`pubs/fabric-sosp09.pdf`
: Fabric: A Platform for Secure Distributed Computation and Storage.
: 22nd ACM Symp. on Operating System Principles (SOSP), October 2009.
  Jed Liu, Michael D. George, K. Vikram, Xin Qi, Lucas Waye, and Andrew C.  Myers.

You can also find more information about the Fabric project at the
[project website](https://research.cs.cornell.edu/fabric/).


Projects developed for courses
==============================

As a lecturer at Cornell, one of my responsibilities was creating new
programming projects for my students.  Although these projects are fairly
small, I always make an effort to demonstrate best practices to my students.
I've also taken great care to help the students focus on what's important
for a given project, while still providing them with interesting code to
explore on their own if they want to.

My projects tended to be large and ambitious (perhaps slightly too ambitious).
I believe that students benefit from working on large projects that require
teamwork. I also think it's important for students to be involved in enough of
each project that they can appreciate how the parts fit together, and the
advantages of good software design.


Tangrams (CS 3110 Functional Programming)
-----------------------------------------

This is a large project focusing on modular software design.  The students had
to provide several implementations of a hierarchy of numeric interfaces, and
provide functors that added general functionality to those implementations.
This project also introduced the use of higher-order functions to represent
data: students implemented arbitrary-precision real numbers encoded as functions
returning rational approximations.

The students also built some geometric algorithms on top of the number
interface.  We provided a GUI that used their geometric algorithms to implement
a tangrams game.  The tangrams game features a collision-avoiding drag and drop
interface (using an interface design that I invented) that makes use of the
arbitrary precision arithmetic implemented by the students.


OCalf interpreter (CS 3110 Functional programming)
--------------------------------------------------

An interpreter is a staple of a functional programming course.  I redesigned
the CS 3110 interpreter project, changing it from an untyped scheme-like
language to a typed OCaml-like language.  We also added a type system, and had
the students implement a small part of a type inference algorithm.

In addition to changing the source language, I cleaned up the structure of the
code significantly, and also put a fair amount of polish into the tools we
provided to the students.  For example, we provided them with a complete
parser, and a pretty printer that integrated into the REPL loop and included
automatic indentation and syntax highlighting.  Although these small details
seem ancillary, I think that providing good tooling to the students really
streamlined their experience and let them focus on the interesting problems.


MapReduce (CS 3110 Functional programming)
------------------------------------------




Five in a row (CS 2110 Object oriented programming)
---------------------------------------------------

Log-structured Filesystem (CS 4410 Operating Systems)
-----------------------------------------------------

Cornell's OS course has traditionally had a userspace log-structured filesystem
implementation project, but the source code is written in python, and there's a
lot of code and effort that goes into managing structs and their sizes, to
serializing python objects, and so on.

This seemed like one of the few places where C is the best choice.  This is my
exploration into a simple C LFS implementation.  It uses memory-mapped IO to
avoid all of the file conversion issues; the disk data structures are stored
directly in memory.

I did this in the middle of a summer session where we didn't run the full LFS
project, so I didn't bother turning it into an actual project or give it much
of a user interface, but I think it's a nice little piece of code.  I gave it
to the students as an example of using memory mapped I/O and memory
segmentation.

Personal open-source projects
-----------------------------

java/jalgebra

js/stars

js/jamcircle

ocaml/sorts

Publications
------------

