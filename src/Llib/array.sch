(define-expander array-ref
   (lambda (x e)
      (match-case x
         ((?- ?array)
          `(array-ref0 ,@(e `(,array) e)))
         ((?- ?array ?i1)
          (e `(array-ref1 ,array ,i1) e))
         ((?- ?array ?i1 ?i2)
          (e `(array-ref2 ,array ,i1 ,i2) e))
         ((?- ?array ?i1 ?i2  ?i3 .  ?rest)
          `(array-ref ,@(e `(,array ,i1 ,i2 ,i3 . ,rest) e)))
         (else
          (error "array-ref" "unrecognized array-ref call"  x))
         )))


(define-expander array-set!
   (lambda (x e)
      (match-case x
         ((?- ?array ?v)
          `(array-set0! ,@(e `(,array ,v) e)))
         ((?- ?array ?i1 ?v)
          (e `(array-set1! ,array ,i1 ,v) e))
         ((?- ?array ?i1 ?i2 ?v)
          (e `(array-set2! ,array ,i1 ,i2 ,v) e))
         ((?- ?array ?i1 ?i2  ?iv1 ?iv2 .  ?rest)
          `(array-set! ,@(e `(,array ,i1 ,i2 ,iv1 ,iv2 . ,rest) e)))
         (else
          (error "array-set!" "unrecognized array-ref call"  x))
         )))