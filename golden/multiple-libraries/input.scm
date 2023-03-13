;;> This is the first library
(define-library (scmdoc first library)
  (export my-id)

  (begin
    ;;> Some identifier.
    (define my-id 1)))

;;> This is the second library
(define-library (scmdoc second library)
  (export other-id)

  (begin
    ;;> Other identifier.
    (define other-id 2)))
