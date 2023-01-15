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
Furthermore, only a very limited subset of R7RS expressions is matched at the moment (procedure definitions, constant definitions, library declarations).
Lastly, a preliminary HTML generation backend is available.

## Installation

This software can be installed using the following commands:

    $ git clone git://git.8pit.net/scmdoc.git
    $ cd scmdoc
    $ cabal install

Afterwards, the binary should be available via `~/.cabal/bin/scmdoc`.

## Tests

Unit tests are available and can be invoked using the following command:

    $ cabal test

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

## Usage Example

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
Since it is difficult to meaningfully group Scheme program components due to the lack of type information, `scmdoc` relies on section comments for grouping.
A section comment is a documentation comment followed by another documentation comment.
For example:

    ;;> ## Multiplication Procedures

    ;;> Procedures for performing multiplication

    ;;> Multiply two signed values.
    (define (multS x y) …)

This will group the procedure `multS` into the section *“Multiplication Procedures”*.
Section comments should **always** use a Markdown `H2`, deeper nesting is not supported.
Each documented Scheme library should contain at least one section comment.

## Related Work

A loose, incomplete collection of similar work on the documentation of Scheme code:

* [Scribble][scribble web]: The documentation tool used by the Racket programming language
* [chibi-doc][chibi-doc source]: The documentation tool used by chibi-scheme (inspired by Scribble)
* [SchemeDoc][schemedoc web]: Extract documentation from Scheme comments using [LAML][laml web]
* MOLE: [Scheme Program Source Code as a Semistructured Data][mole paper]
* [`l2t`][l2t web] (aka. LiSP2TeX): A literate programming utility for inserting Scheme into TeX files
* [schemeweb][schemeweb ctan]: Simple literate programming for Scheme with LaTeX

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
