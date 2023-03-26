## Demo

A sample documentation for [chibi parse][chibi parse] is available: https://files.8pit.net/scmdoc-demo/

## Motivation

Tooling for inline source code documentation of Scheme code is rare.
Existing tools like [chibi-doc][chibi-doc source] actually interpret/load the code and thus do not work if, for example, some utilized libraries are not available for `chibi-scheme`.
To overcome this limitation `scmdoc` takes a different approach were the code is not interpreted and only parsed.
This allows `scmdoc` to be applicable to different R⁷RS implementations ([CHICKEN][chicken web], [Guile][guile web], [Gambit][gambit web], [Racket][racket r7rs], …) as long as the input is syntactically valid R⁷RS.
Pattern matching is then performed on parsed S-expressions to format them nicely in the generated documentation (e.g. matching for procedure definitions, library definitions, …).
In its essence, `scmdoc` is therefore a glorified pattern matcher for S-expressions.

## Status

This is currently a proof-of-concept.
The R7RS parser should support the majority of the standard but is not well tested.
A HTML generation backend is available and several R7RS expressions are formatted at the moment, including more complex ones like record type definitions.

## Installation

This software is known to compile with GHC 9.4.4 and can be installed by running the following commands inside the repository:

    $ cabal install

## Tests

Unit tests are available and can be invoked using the following command:

    $ cabal test

Furthermore, integration tests are available.
These tests require the `scmdoc` binary to be available in your `$PATH`.
Furthermore, they require [Tidy][tidy web] to be installed.
If these prerequisites are satisfied run the following command to execute these tests:

    $ ./golden/run.sh

## Approach

The `scmdoc` tool is structured around R7RS Scheme libraries.
For a given Scheme source file, it extracts all library declaration which are prefixed by a doc comment.
Contrary to a normal Scheme comment, a doc comment has the form `;;>`.
As such, `scmdoc` would for example recognize the following library declaration:

    ;;> My awesome scheme library
    (define-library (sample-library)
        …)

For each recognized library, `scmdoc` expands all includes in the declaration and then extracts all exported and documented expressions from the library.
The recognized expressions are then further processed by high-level formatters which recognize S-expressions constituting Scheme procedure definitions et cetera.
These formatters are available in `src/SchemeDoc/Format` and essentially perform pattern matching on arbitrary S-expressions.
For example, the formatter for Scheme procedure definitions roughly looks as follows:

    mkProcedure :: Sexp -> Maybe Procedure
    mkProcedure (List ((Id "define"):(List ((Id defid):arglst)):bodylst)) =
        -- Convert the S-expression into an algebraic data structure
        -- which represents Scheme Procedure definitions and can be
        -- formated.
    mkProcedure _ = Nothing

In the future, it should be possible to also supply custom formatters to, for example, to generate documentation for definitions defined using custom macros.
Apart from formatters, `scmdoc` also has a WiP concept of "expanders" which perform primitive comment-preserving macro expansion.
This feature is presently used to expand record type definitions (`define-record-type`) to primitive procedure definitions which are then formatted using the procedure formatter.

## Usage

The `scmdoc` tool generates HTML from a Scheme input defining a R7RS library.
As described above, only documented S-expressions for which a documentation formatter is available are included in the generated documentation.
In order to demonstrate generation of HTML documentation consider the following input file called `library.scm`:

    ;;> This is my documented library.
    (define-library (math arithmetic)
      (export my-proc my-magic-constant)

      ;; This is a normal comment which is not considered by scmdoc.
      (begin
        ;;> this is a hidden helper function which is not exported.
        (define my-mult *)

        ;;> my-proc multiplies the given value with two.
        (define (my-proc x)
          (my-mult x 2))

        ;;> A magic constant.
        (define my-magic-constant 42)))

In order to generate HTML documentation, using `scmdoc`, for this library run the following command:

    $ scmdoc library.scm

Within documentation comments, it is possible to use Markdown markup.

### Section Comments

Due to the lack of type information, it is difficult to meaningfully group Scheme program components.
For this reason, `scmdoc` relies on manually inserted *section comments* for grouping.
A section comment is a documentation comment which starts with a `|` character and is followed by an empty line and a normal documentation comment.
The section comment corresponds to the section title, the documentation comment following it functions as the section description.
For example:

    ;;>| Multiplication Procedures
    ;;>
    ;;> Procedures for performing multiplication.

    ;;> Multiply two signed values.
    (define (multS x y) …)

    ;;> Multiply two unsigned values.
    (define (multU x y) …)

This will group the procedure `multS` and `multU` into the section *“Multiplication Procedures”*.
Every section comment consists of a section title (as indicated by `;;>|`) and a mandatory section description.
Each documented Scheme library *should* contain at least one section comment.
If the Scheme library doesn't contain any section comments, then everything is grouped under an arbitrary “Declarations” section.

### Look and Feel

The HTML generated by `scmdoc` should be compatible with any [classless CSS framework][cssbed web].
By default, the [water.css][water.css web] dark theme is used.
A custom CSS stylesheet can be specified using the `--stylesheet` command-line.
For more information on classless CSS frameworks refer to [this blog post][greenfield classless].

## Related Work

A loose, incomplete collection of similar work on the documentation of Scheme code:

* [Scribble][scribble web]: The documentation tool used by the Racket programming language
* [chibi-doc][chibi-doc source]: The documentation tool used by chibi-scheme (inspired by Scribble)
* [SchemeDoc][schemedoc web]: Extract documentation from Scheme comments using [LAML][laml web]
* MOLE: [Scheme Program Source Code as a Semistructured Data][mole paper]
* [`l2t`][l2t web] (aka. LiSP2TeX): A literate programming utility for inserting Scheme into TeX files
* [schemeweb][schemeweb ctan]: Simple literate programming for Scheme with LaTeX

## Development

Please format all code with [fourmolu](https://github.com/fourmolu/fourmolu).
For convince, a pre-commit Git hook for checking if files are properly formated is provided in `.githooks`.
It can be activated using:

    $ git config --local core.hooksPath .githooks

## License

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation.
Furthermore, this code presently includes the Scheme number parser from [Husk Scheme][husk-scheme github] which is licensed under MIT.

[chicken web]: https://call-cc.org
[guile web]: https://www.gnu.org/software/guile/
[gambit web]: https://www.gambitscheme.org/
[racket r7rs]: https://pkgs.racket-lang.org/package/r7rs
[chibi-doc source]: https://github.com/ashinn/chibi-scheme/blob/master/tools/chibi-doc
[husk-scheme github]: https://github.com/justinethier/husk-scheme
[schemedoc web]: https://people.cs.aau.dk/~normark/schemedoc/
[schemeweb ctan]: https://ctan.org/pkg/schemeweb
[mole paper]: http://www.schemeworkshop.org/2001/lisovsky.pdf
[laml web]: http://people.cs.aau.dk/~normark/laml-distributions/laml/
[l2t web]: https://christian.queinnec.org/WWW/l2t.html
[scribble web]: https://docs.racket-lang.org/scribble/index.html
[cssbed web]: https://www.cssbed.com/
[greenfield classless]: https://ubershmekel.medium.com/the-next-css-frontier-classless-5e66f3f25fdd
[water.css web]: https://watercss.kognise.dev/
[chibi parse]: https://synthcode.com/scheme/chibi/lib/chibi/parse.html
[tidy web]: https://www.html-tidy.org
