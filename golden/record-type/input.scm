;;> This is a library with a record type definition.
(define-library (scmdoc record)
  (export kons pare? kar set-kar! kdr)

  (begin
    ;;> This is my custom record type
    (define-record-type pare
      ;;> Create a new pare.
      (kons x y)
      ;;> Predicate to check if the given `obj` is a pare.
      pare?
      (x
        ;;> Obtain the first element of the pare.
        kar
        ;;> Modify the first element of the pare.
        set-kar!)
      (y
        ;;> Obtain the second element of the pare.
        kdr))))
