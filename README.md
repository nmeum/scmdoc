## Motivation

Tooling for inline source code documentation of Scheme code is rare.
Existing tools like [chibi-doc][chibi-doc source] actually interpret/load the code and thus do not work if, for example, some utilized libraries are not available for `chibi-scheme`.
To overcome this limitation `scmdoc` takes a different approach were the code is not interpreted and only parsed.
This allows `scmdoc` to be applicable to different R⁷RS implementations ([CHICKEN][chicken web], [Guile][guile web], [Gambit][gambit web], [Racket][racket r7rs], …) as long as the input is syntactically valid R⁷RS.
Pattern matching is then performed on parsed S-expressions to format them nicely in the generated documentation (e.g. matching for procedure definitions, library definitions, …).
In its essence, `scmdoc` is therefore a glorified pattern matcher for S-expressions.

## Status

This is currently a proof-of-concept.
The R7RS parser is incomplete and only parses small programs successfully.
Furthermore, only a very limited subset of R7RS expressions is matched at the moment (procedure definitions, constant definitions, library declarations).
Lastly, a preliminary Markdown generator is available but is lacking many features.

## Installation

This software can be installed using the following commands:

    $ git clone git://git.8pit.net/scmdoc.git
    $ cd scmdoc
    $ cabal install

Afterwards, the binary should be available via `~/.cabal/bin/scmdoc`.

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

The `scmdoc` tool generates Markdown from a Scheme input defining a R7RS library.
As described above, only documented S-expressions for which a documentation formatter is available are included in the generated documentation.
In order to demonstrate generation of Markdown documentation consider the following input file called `library.scm`:

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

In order to generate Markdown documentation, using `scmdoc`, for this library run the following command:

    $ scmdoc library.scm

This will generate the following Markdown document (extra whitespaces added for clarity):

    # math arithmetic

    This is my documented library.

    ## Procedure my-proc

    my-proc multiplies the given value with two.

    	(my-proc x)

    ## Constant my-magic-constant

    A magic constant.

    	my-magic-constant

This file can then be processed further with a suitable Markdown implementation to generate an HTML or PDF document (e.g. [pandoc][pandoc web] or [discount][discount web]).

## Related Work

* SchemeDoc / …
* https://ctan.org/pkg/schemeweb
* STOL: https://doi.org/10.1145/382126.382663
* Literate Programming from Scheme To TeX
    * lisp2tex progam
    * https://www.cs.cmu.edu/Groups/AI/lang/scheme/util/lisp2tex/
    * Further Development: https://christian.queinnec.org/WWW/l2t.html
* MOLE: http://www.schemeworkshop.org/2001/lisovsky.pdf
* https://orgmode.org/manual/Extracting-Source-Code.html
* chibi-doc
* racket scribble
* CHICKEN Hahn
* …

## License

This program is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation.

[chicken web]: https://call-cc.org
[guile web]: https://www.gnu.org/software/guile/
[gambit web]: https://www.gambitscheme.org/
[racket r7rs]: https://pkgs.racket-lang.org/package/r7rs
[chibi-doc source]: https://github.com/ashinn/chibi-scheme/blob/master/tools/chibi-doc
[pandoc web]: https://pandoc.org/
[discount web]: http://www.pell.portland.or.us/~orc/Code/discount/
