;;> This is a library which contains an unexported, but documented, identifier.
(define-library (scmdoc test simple)
  (export magic-number1 magic-number2 id)

  (begin
    ;;>| Constants
    ;;
    ;;> Various useful constants.

    ;;> A magic number.
    (define magic-number1 23)

    ;;> Another magic number.
    (define magic-number2 42)

    ;;>| Procedures

    ;;> Various useful procedures.

    ;;> The identity procedure.

    (define (id x) x)))
