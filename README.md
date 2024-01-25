Michael George's Public Programming Portfolio
=============================================

* [LinkedIn](https://www.linkedin.com/in/mdgeorge/)

This repository contains several samples of my work, highlighting a variety of
programming projects that I've worked on in various languages and settings.

This document gives a brief overview of each of the projects contained herein.
The projects that are open source are referenced as git submodules so that you
can view the version history and other related public information.

Each project has a README file in its top-level directory containing
information about the file organization and how to build and run the project.
I've tested everything on Ubuntu 20.04 (focal); some of the projects may
require a bit of work to build or run on other platforms.

Here is a brief outline of the projects; the projects are described in further
detail below.

### Large research projects:

 * [`java/fabric`](#fabric):
   an extension of Java with language-level support for distributed computing,
   mobile code, and strong information flow control.

### Projects developed for students:

 * [`ocaml/tangrams`](#tangrams):
   a project that brings together modular software design, arbitrary-precision
   arithmetic, and computational geometry to implement a novel drag-and-drop
   algorithm.

 * [`ocaml/interpreter`](#interpreter):
   an interpreter implementation project for a subset of OCaml, including type
   inference.

 * [`ocaml/mapreduce`](#mapreduce):
   an implementation of a distributed map/reduce platform using Async.

 * [`java/fiveinrow`](#fiveinrow):
   a tic-tac-toe like game, intended to be students' first "large" project with
   multiple high-level components.

 * [`c/lfs`](#lfs):
   a project to demonstrate the layout and operation of a log-structured
   filesystem.  Also serves as a demo for memory-mapped I/O and segmentation.

### Techincal writing

 * [`docs/certora`](docs/certora):
   Certora's user-facing documentation; I was the primary author and editor.
   Contains documentation I have written in [user guide][multicontract-docs] and
   [reference manual][methods-docs] genres.

[multicontract-docs]: https://docs.certora.com/en/latest/docs/user-guide/multicontract/index.html
[methods-docs]: https://docs.certora.com/en/latest/docs/cvl/methods.html

 * [`pubs`](pubs/README.md):
   peer-reviewed publications that I have authored.

### Side projects:

 * [`coq/math-classes`](#math-classes):
   proof automation contributions to the open-source math-classes Coq library.

 * [`js/jamcircle`](#jamcircle):
   a prototype videoconferencing application designed for playing music
   together.

 * [`java/jalgebra`](#jalgebra):
   an exploration of an unusual design pattern for object-oriented number
   classes, including a custom annotation processor to support quickcheck-like
   testing.

 * [`ocaml/sorts`](#sorts):
   a monadic framework for animating in-place sorting algorithms.


Projects developed for courses
==============================

As a lecturer at Cornell, one of my responsibilities was creating new
programming projects for my students.  Although these projects are fairly
small, I always make an effort to demonstrate best practices to my students.
I've also taken great care to help the students focus on what's important
for a given project, while still providing them with interesting code to
explore on their own if they want to.

In particular, I've put a lot of time into making the interfaces and
documentation for these projects clear and tailored to the students' level of
expertise.

My projects tended to be large and ambitious (perhaps a little too ambitious).
I believe that students benefit from working on large projects that require
teamwork. I also think it's important for students to be involved in enough of
each project that they can appreciate how the parts fit together, and the
advantages of good software design.


<a id="tangrams"></a>
[Tangrams (CS 3110 Functional Programming)](ocaml/tangrams/README.md)
-----------------------------------------------------------

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


<a id="interpreter"></a>
[OCalf interpreter (CS 3110 Functional programming)](ocaml/interpreter/README.md)
-----------------------------------------------------------------------

An interpreter is a staple of a functional programming course.  I redesigned
the CS 3110 interpreter project, changing it from an untyped scheme-like
language to a subset of OCaml.  We also added a type system, and had the
students implement a small part of a type inference algorithm.

In addition to changing the source language, I cleaned up the structure of the
code significantly, and also put a fair amount of polish into the tools we
provided to the students.  For example, we provided them with a complete
parser, and a pretty printer that integrated into the REPL loop and included
automatic indentation and syntax highlighting.  Although these small details
seem ancillary, I think that providing good tooling to the students really
streamlined their experience and let them focus on the interesting problems.


<a id="mapreduce"></a>
[MapReduce (CS 3110 Functional programming)](ocaml/mapreduce/README.md)
-------------------------------------------------------------

For historical reasons, Cornell's functional programming course (CS 3110) used
to cover concurrent programming with threads and monitors.  This was never a
good fit for a variety of reasons, so my co-instructors and I redesigned the
concurrency module to cover promise-based concurrency using Jane Street's Async
library.

I redesigned the existing MapReduce project to make use of Async and added
exercises to help them learn how to use the Async library.  Our primary goal
for the project was to introduce students to promise-based asynchronous
programming.

The Async library is an industrial-strength library designed for
high-performance computing.  As a result, the official Async documentation was
much more complex than was appropriate for our students.  As part of this
project, I developed a more focused set of OCamldoc documentation for the parts
of the library that the students needed to interact with (see `doc/async.mli`).


<a id="fiveinrow"></a>
[Five in a row (CS 2110 Object oriented programming)](java/fiveinrow/README.md)
---------------------------------------------------------------------

This project was given midway through the object-oriented programming course,
to help the students move from focusing on the semantics of objects and the
mechanics of Java to focusing on the large-scale structure of programs.  This
project introduces OO design concepts like the Observer pattern and
Model/View/Controller.  It also shows students the value of programming against
an interface by having them implement the same interface in multiple ways.


<a id="lfs"></a>
[Log-structured Filesystem (CS 4410 Operating Systems)](c/lfs/README.md)
--------------------------------------------------------------

Cornell's OS course has traditionally had a userspace log-structured filesystem
implementation project, but the source code is written in python, and there's a
lot of code and effort that goes into managing structs and their sizes, to
serializing python objects, and so on.

This seemed like one of the few places where C is the best choice.  This is my
exploration into a simple C LFS implementation.  It uses memory-mapped I/O to
avoid all of the file conversion issues; the disk data structures are stored
directly in memory.

I did this in the middle of a summer session where we didn't run the full LFS
project, so I didn't bother turning it into an actual project or give it much
of a user interface, but I think it's a nice little piece of code.  I released
the code to the students as a study guide and an example of using memory mapped
I/O and memory segmentation.


<a id="fabric"></a>
[Graduate Research Project (Fabric)](java/fabric/README.md)
=================================================

**Note:** the Fabric repository is very large, so I haven't included it in this
bundle.  You can fetch it using git by running `git submodule update --init`,
or you can browse the source [online](https://github.com/apl-cornell/fabric).

Most of my graduate research was conducted in the context of the Fabric
project.  I was one of four lead developers of the Fabric language and system.
Fabric is an extension of Java with language-level support for distributed
computing, mobile code, and strong information flow control.

The public fabric repository is contained in the `java/fabric` directory.  As
one of the lead developers, I've made significant contributions to all portions
of the language, system, and example application implementations (296kloc
according to github).

My Fabric related publications are contained in the `pubs` directory:

`pubs/dissert.pdf`
: Trust, Authority, and Information Flow in Secure Distributed Systems.
  Doctoral dissertation. Cornell University 2020.

`pubs/fabric-jcs.pdf`
: Fabric: Building Open Distributed Systems Securely by Construction.
  J. Computer Security, 2017.
  Jed Liu, Owen Arden, Michael D. George, and Andrew C. Myers.

`pubs/mobile-oakland12.pdf`
: Sharing Mobile Code Securely with Information Flow Control.
  IEEE Symp. on Security and Privacy, May 2012.
  Owen Arden, Michael D. George, Jed Liu, K. Vikram, Aslan Askarov, and Andrew C. Myers.

`pubs/fabric-sosp09.pdf`
: Fabric: A Platform for Secure Distributed Computation and Storage.
  22nd ACM Symp. on Operating System Principles (SOSP), October 2009.
  Jed Liu, Michael D. George, K. Vikram, Xin Qi, Lucas Waye, and Andrew C.  Myers.

You can also find more information about the Fabric project at the
[project website](https://research.cs.cornell.edu/fabric/).


Personal open-source experiments
================================

These projects are ideas that I've explored out of curiousity in my free time.
They are less complete and less polished than the professional projects above,
but they give some idea of the things I'm interested in and my approach to
exploration.

<a id="math-classes"></a>
[Math-classes](coq/math-classes):
---------------------------------

One of my favorite "hello world" applications for learning new languages is to
implement some concepts from abstract algebra and use them to implement
computational geometry algorithms.  This repo contains the beginnings of a coq
implementation.  I have built on the `math-classes` library, adding definitions
of ordered rings and fields based on positive cones.

As part of the process, I built some proof automation for simplifying group
expressions; these contributions have been merged upstream.

<a id="jamcircle"></a>
[JamCircle](js/jamcircle)
-------------------------

One of the things I miss from before the pandemic is being able to play my
fiddle with friends.  Standard videoconferencing software is unacceptable for
playing music, because music requires tighter synchronization than internet
latency typically allows.  Special-purpose jamming software tries to lower the
latency as much as possible, but it still requires a low-latency connection and
specialized hardware and software.

I realized that by removing two-directional communication, I could create the
illusion of zero latency.  JamCircle orders the performers in time, and each
performer can only observe the performers that come before them.  They then play
along with that music as they hear it, and they can locally synchronize their
own stream with that of the past performers.  They then forward this combined
stream to the next player in time.  Even though the players are offset in time,
they observe each other as being perfectly synchronized.

This repository contains my current prototype for an application that
implements this design.  I want the program to be as widely available as
possible, so I've decided to build it as a web application using WebRTC.  I
don't have much experience with the modern JavaScript ecosystem, and WebRTC is
a new technology with spotty documentation, so this project is just an
experimental prototype.  It has been quickly growing and changing as I'm
learning, so the project organization isn't quite up to my normal standards.
But it's a neat project, and it's what I'm working on right now, so I decided
to include it.

<a id="sorts"></a>
[Sorts](ocaml/sorts)
--------------------

When I took my first computer science class in middle school and learned about
sorting algorithms, the instructor showed us a demo that animated several
in-place sorting algorithms side by side.  I remember spending a good chunk of
time that summer figuring out how to implement that program in C++.  In
retrospect, I was manually implementing coroutines by storing the stack in an
auxiliary data structure, but I didn't know that.

When I learned about monads, this old problem came to mind as a potential
application.  This little program defines a sorting monad in OCaml that defines
`compare` and `swap` methods.  It allows you to write in-place sorting
algorithms in an imperative style, and allows running them in an animated
fashion.

<a id="jalgebra"></a>
[JAlgebra](java/jalgebra)
-------------------------

The drag-and-drop algorithm contained in the tangrams project described above
is interesting because any rounding error will quickly become noticable -- it is
easy to create a hole that something should fit in but doesn't because of
rounding, no matter what level of precision is used.  The best number format
to use depends on the shapes and operations supported by the application.  For
example, the tangrams game uses polygons and allows 15° rotations, which
requires numbers of the form `(a + b√2 + c√3 + d√6)/e` (where `a`..`e` are
integers).

This led me to think about the best way to design an interface for numbers in
various languages, and to provide clear specifications and automated testing.
I've found that it's an interesting way to explore the generic programming
facilities of different languages.

Over the years, I have toyed with this idea in various languages (C++, Python,
Java, OCaml, JavaScript, Coq).  The `jalgebra` project is my Java prototype.  I
found that Java's annotation system provided an interesting hook for writing
executable specifications. JAlgebra contains several mathematical interfaces
and implementations of those interfaces.  It also contains a custom annotation
processor that automatically generates quickcheck style tests.


