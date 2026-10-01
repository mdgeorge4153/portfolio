Publications
============

[OCaml Programming: Correct + Efficient + Beautiful][ocaml-book].
Michael R. Clarkson et al.  Michael D. George is a contributing author.

[ocaml-book]: https://cs3110.github.io/textbook/cover.html

[Fabric: Building Open Distributed Systems Securely by Construction](fabric-jcs.pdf).
J. Computer Security, 2017. Jed Liu, Owen Arden, Michael D. George, and Andrew C. Myers.

[Warranties for Faster Strong Consistency](warranties-nsdi14.pdf).
11th USENIX Symp. on Networked Systems Design and Implementation (NSDI), April 2014.
Jed Liu, Tom Magrino, Owen Arden, Michael D. George, and Andrew C. Myers.

[Sharing Mobile Code Securely with Information Flow Control](mobile-oakland12.pdf).
IEEE Symp. on Security and Privacy, May 2012.
Owen Arden, Michael D. George, Jed Liu, K. Vikram, Aslan Askarov, and Andrew C. Myers.

[Fabric: A Platform for Secure Distributed Computation and Storage](fabric-sosp09.pdf)
22nd ACM Symp. on Operating System Principles (SOSP), October 2009.
Jed Liu, Michael D. George, K. Vikram, Xin Qi, Lucas Waye, and Andrew C.  Myers.

[Trust, Authority, and Information Flow in Secure Distributed Systems](dissert.pdf).
Doctoral dissertation.  Cornell University, 2020.  Michael D. George.


Acknowledged contributions
--------------------------

[Tracking Borrows with Regular Expressions](https://ilyasergey.net/assets/pdf/papers/regex-borrows-oopsla26.pdf).
OOPSLA 2026. Todd Nowacki, Sam Blackshear, John Mitchell, Shaz Qadeer, and Ilya Sergey.

I am not an author of this paper, but I came up with its key ideas while at
Mysten Labs: tracking the aliasing relationships between references with
regular expressions, using Brzozowski derivatives to express the effects of
borrowing operations, using Kleene star to summarize borrow chains from loops
and function calls, and reducing the aliasing check to decidable regular
expression emptiness. The authors mechanized and implemented the design, which
now replaces the original borrow checker in Sui's bytecode verifier, and the
paper acknowledges my contributions.

