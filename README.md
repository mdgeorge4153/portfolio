Michael George's Programming Portfolio
======================================

This repository contains several samples of my work, demonstrating a variety of
programming projects that I've worked on in various languages and settings.

This document gives a brief overview of each of the projects contained herein.
The projects that are open source are referenced as submodules so that you can
view the version history and other related public information.

Fabric
------

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
------------------------------

As a lecturer at Cornell, one of my responsibilities was creating new
programming projects for my students.  Although these projects are fairly
small, I always make an effort to demonstrate best practices to my students,
and to use well-designed and well-documented abstraction barriers to help my
students focus on the important lessons while still being able to write
programs that work with real technology.

For example, the puzzle solver project (`ocaml/puzzle`) was designed to fall
fairly early in the functional programming course, and was intended to teach
the students how to implement an interface, and how to dynamically explore a
search space defined by a functional interface (instead of by traversing a data
structure).  However, the provided scaffolding contained OpenGL GUIs that
animated the results of their algorithms.  Good use of the module system allowed
us to clearly differentiate the code that it was important for the students to
understand from the code that they could safely ignore.

java/fiveinrow

ocaml/interpreter

ocaml/mapreduce

ocaml/puzzle

ocaml/tangrams

c/lfs

Log-structured filesystem (c/lfs)
---------------------------------

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

