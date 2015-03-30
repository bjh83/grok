Grok
====
Simplicity should not come at the cost of expressiveness.

About
-----
Grok attempts to uniquely combine functional style type systems with imperative
style syntax. In particular, Grok has a fully algebraic type system that 
achieves polymorphism purely through ad-hoc polymorphism; while allowing mutable
variables and static evaluation.

Installing
----------
Installing grok requires [sbt](http://www.scala-sbt.org/) 0.13 or higher and
git:
  - See [download](http://www.scala-sbt.org/download.html) sbt
  - See [download](http://git-scm.com/downloads) git

Installing and building Grok is simple:

    $ git clone https://github.com/bjh83/grok.git
    $ cd grok
    $ sbt compile

The first time you run sbt, it will download all needed dependencies; this may
take a little while.

Running
-------
Since grok is still very much under development, it is easiest to execute it
from the sbt terminal:

    $ sbt "run [flags] [path_to_source]"

Where flags are optionally one of the following:

    --compiler_type=default
    --compiler_type=interpreter

Ultimately, the default option will compile the input files into output files
which will be executable by a virtual machine; however, at the moment, this only
verifies that the input file is grammatically and semantically valid and then
prints out the AST. The interpreter actually executes a subset of the language;
however, its error messages are unhelpful at best.

Language Specification
----------------------
A BNF format of the grammar can be found at: [grammar.bnf](https://github.com/bjh83/grok/blob/master/src/main/grammar/com/grok/grammar.bnf);
however, this is currently out of date. The antlr4 [grammar file](https://github.com/bjh83/grok/blob/master/src/main/antlr4/com/grok/Grok.g4)
is considered definitive as it is the actual file used to generate Grok's lexer
and parser.

To read more about Grok's specification please refer to the [Progress Report](http://art.case.edu/395.S15/15%20progress%20report%201/EECS395.S2015.Higgins.progress_report.pdf).

Errata
------
Grok is currently under development and as such is in a constant state of flux.
Expect regular breaking changes.

In particular, there are a number of things which do not work:
  - Type Checking: Currently under development.
  - Code Generation: Not yet started.
  - Interpreter: Only supports a subset of the language.

Please check back regularly, as this list will get smaller and more detailed.
