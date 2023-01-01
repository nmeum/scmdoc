# scmdoc

An inline source code documentation tool for R⁷RS Scheme.

## Motivation

Tooling for inline source code documentation of Scheme code is rare.
Existing tools like [chibi-doc][chibi-doc source] actually interpret/load the documented code and thus do not work if, for example, some utilized libraries are not available for `chibi-scheme`.
To overcome this limitation `scmdoc` takes a different approach were the code is not interpreted and only parsed.
This allows `scmdoc` to be applicable to different R⁷RS implementations (CHICKEN, Guile, Gambit, Racket, …) as long as the input is syntactically valid R⁷RS.
In its essence, `scmdoc` is a glorified pattern matcher for S-expressions.
It extracts (custom?) S-expressions preceded by a special doc-comment and formats them according to (user-supplied?) rules.

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

[chibi-doc source]: https://github.com/ashinn/chibi-scheme/blob/master/tools/chibi-doc
