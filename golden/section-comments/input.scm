;;> This is a library which contains an unexported, but documented, identifier.
(define-library (scmdoc test simple)
  (export magic-number1 magic-number2 id foo bar)

  (begin
    ;;>| Constants
    ;;
    ;;> Various useful constants.

    ;;> A magic number.
    (define magic-number1 23)

    ;;> Another magic number.
    (define magic-number2 42)

    ;;>| Some section
    ;;>
    ;;> Yet another section.

    ;;> Foo constant.
    (define foo 42)

    ;;>| Additional section.
    ;;> One more way to write a section comment.

    ;;> Bar constant.
    (define bar 23)

    ;;>| Procedures

    ;;> Various useful procedures.

    ;;> The identity procedure.

    (define (id x) x)))
