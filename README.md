# portfolio

A collection of coding assignments and other materials that Michael George has
created.

Major projects
==============


Class assignments
=================

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

Fun projects
============


