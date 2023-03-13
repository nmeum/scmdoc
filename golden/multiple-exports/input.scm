;;> This is a library which contains multiple export declarations.

(define-library (scmdoc test simple)
  (export const1)
  (export const2)

  (begin
    ;;> First constant.
    (define const1 1)

    ;;> Second constant.
    (define const2 2)))
