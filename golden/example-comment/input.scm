;;> This is a library which contains an example usage.
;;>
;;> Consider the following example:
;;>
;;> ```
;;> (import (scmdoc test simple))
;;>
;;> (define (my-func x)
;;>   (* my-id 2))
;;> ```
(define-library (scmdoc test simple)
  (export my-id)

  (begin
    ;;> This identifier is documented and exported.

    (define (my-id x) 23)))
