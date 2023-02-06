;;> This is a library which renames an identifier.
(define-library (scmdoc test simple)
  (export (rename foo bar))

  (begin
    ;;> This is my constant.
    (define foo "foo")))
