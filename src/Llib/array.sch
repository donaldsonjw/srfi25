
(define-expander vector-
   (lambda (x e)
      (match-case x
         ((?- ?t ?l ?r)
          (let ((index (gensym "i"))
                (vg (gensym "v"))
                (lg (gensym "l"))
                (rg (gensym "r"))
                (tg (gensym "t")))
             (e `(let ((,lg ,l)
                       (,rg ,r)
                       (,tg ,t))
                    (if (=fx (vector-length ,lg) (vector-length ,rg))
                        (do ((,index 0 (+fx ,index 1)))
                            ((=fx ,index (vector-length ,lg)) ,tg)
                            (let ((,vg (-fx (vector-ref ,lg ,index) (vector-ref ,rg ,index))))
                               (vector-set! ,tg ,index ,vg)))
                        (error "vector-" "vector lengths must be equal" (list ,l ,r)))) e)))
         (else
          (error "vector-" "invlaid syntax" x)))))

(define-expander vector*
   (lambda (x e)
      (match-case x
         ((?- ?l ?r)
          (let ((index (gensym "i"))
                (res (gensym "res"))
                (lg (gensym "l"))
                (rg (gensym "r")))
             (e `(let ((,lg ,l)
                       (,rg ,r))
                    (if (=fx (vector-length ,lg) (vector-length ,rg))
                      (do ((,index 0 (+fx ,index 1))
                           (,res 0 (+fx ,res (*fx (vector-ref ,lg ,index) (vector-ref ,rg ,index)))))
                          ((=fx ,index (vector-length ,lg)) ,res))
                      (error "vector*" "vector lengths must be equal" (list ,lg ,rg)))) e)))
         (else
          (error "vector*" "invlaid syntax" x)))))


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
