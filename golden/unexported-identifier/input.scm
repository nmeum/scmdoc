;;> This is a library which contains an unexported, but documented, identifier.
(define-library (scmdoc test simple)
  (export exported-and-documented)

  (begin
    ;;> This identifier is properly documented and exported.
    (define exported-and-documented 1)

    ;;> This identifier is documented but not exported.
    (define documented-but-not-exported 0)))
