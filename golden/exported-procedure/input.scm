;;> This is a library which contains an exported procedure definition.

(define-library (scmdoc test simple)
  (export my-procedure)

  (begin
    ;;> This identifier is a documented and exported procedure.
    (define (my-procedure x)
      (if (> x 23)
        23
        42))))
