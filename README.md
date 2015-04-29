Grok
====
Simplicity should not come at the cost of expressiveness.

About
-----
Grok attempts to uniquely combine functional style type systems with imperative
style syntax. In particular, Grok has a fully algebraic type system that 
achieves polymorphism purely through ad-hoc polymorphism; while allowing mutable
variables and static evaluation.

Building
----------
Building grok requires [sbt](http://www.scala-sbt.org/) 0.13.6 or higher and
git:
  - See [download](http://www.scala-sbt.org/download.html) sbt
  - See [download](http://git-scm.com/downloads) git

Building Grok is simple:

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

    --version
    --compiler_type=default
    --compiler_type=interpreter
    --compiler_type=tac
    --compiler_type=vm_compiler

Version prints out the current version of the compiler, while compiler\_type
tells the compiler what configuration to use:
  - default: is the same as not specifying anything; it is also the same as the
    tac option
  - interpreter: interprets the AST directly (NOTE: this option is only
    partially supported at the moment).
  - tac: prints out the generated Three Address Code (TAC).
  - vm\_compiler: interprets the generated TAC

Installing
----------
If you are so inclined to install this on you system, you must first have java
1.7 or higher installed on your machine. Next you must execute the following
command.

    $ sbt assembly

Now create a symlink to grok.sh in a desired bin folder:

    $ ln -s /path/to/grok.sh /usr/bin/grok

You may now execute grok with the following command:

    $ grok [flags] [path_to_source]

Where flags and path\_to\_source are the same as described under the section
[Running](#running).

Note: you probably do not want to install grok on your system. Grok is currently
under development and is **SUPER PRE-ALPHA** and so it is expected to change a
very great deal in the near future. You have been warned.

Language Specification
----------------------
Grok now has a complete 
[language specification](http://art.case.edu/395.S15/15%20final%20reports/9.EECS395.S2015.Higgins.final_report.pdf) 
for its current version (lambdas are not currently in the spec, but are mostly 
supported).

Errata
------
Grok is currently under development and as such is in a constant state of flux.
Expect regular breaking changes.

In particular, there are a number of things which do not work:
  - Methods: are not currently supported.
  - Function Closures: are not currently supported.
  - Implicit Union Conversion: does not currently work.
