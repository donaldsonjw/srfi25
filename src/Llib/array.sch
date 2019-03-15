(define-expander make-stack-vector
   (lambda (x e)
      (match-case x
         ((?- ?len)
          (e `(cond-expand
                 (bigloo-c
                  (pragma::vector
                     "({int byte_size = VECTOR_SIZE + (($1-1) * OBJ_SIZE);\n
                        obj_t result = (obj_t)alloca(byte_size);\n
                        #if ( !defined (TAG_VECTOR) ) \n
                        result->vector.header = MAKE_HEADER(VECTOR_TYPE, byte_size);
                        #endif\n
                        result->vector.length = $1;\n
                        BVECTOR(result);\n
                      })"
                     ,len))
                 (else
                  (make-vector ,len))) e))
         (else
          (error "make-stack-vector" "invalid syntax" x)))))

(define-expander vector-
   (lambda (x e)
      (match-case x
         ((?- ?t ?l ?r)
          (e `(if (=fx (vector-length ,l) (vector-length ,r))
                  (do ((i 0 (+fx i 1)))
                      ((=fx i (vector-length ,l)) ,t)
                      (vector-set! ,t i (-fx (vector-ref ,l i) (vector-ref ,r i))))
                  (error "vector-" "vector lengths must be equal" (list ,l ,r))) e))
         (else
          (error "vector-" "invlaid syntax" x)))))

(define-expander vector*
   (lambda (x e)
      (match-case x
         ((?- ?l ?r)
          (e `(if (=fx (vector-length ,l) (vector-length ,r))
                  (do ((i 0 (+fx i 1))
                       (res 0 (+fx res (*fx (vector-ref ,l i) (vector-ref ,r i)))))
                      ((=fx i (vector-length ,l)) res))
                  (error "vector*" "vector lengths must be equal" (list ,l ,r))) e))
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
