;;> This is a library definition which includes a syntax definition.
(define-library (example syntax)
  (export my-syntax)

  (begin
    ;;> My custom syntax, should be used as follows.
    ;;>
    ;;> ```
    ;;> (my-syntax
    ;;>   (display "foo")
    ;;>   (newline))
    ;;> ```
    (define-syntax my-syntax
      (syntax-rules ()
        ((my-syntax BODY ...)
         (begin BODY ...))))))
