(define-syntax receive*
   (syntax-rules ()
      ((_ vals producer expr ...)
       (let ((vals (call-with-values (lambda () producer)
                                     (lambda args (list->vector args)))))
          expr ...))))

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
