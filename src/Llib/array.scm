(module srfi25/array
   (export
      (class %shape
         vec::vector)
      (abstract-class %array-base)
      (class %array::%array-base
         index::procedure
         shape::%shape
         vec::vector)

      (inline array? obj)
      (make-array shape::%shape #!optional (default #unspecified))
      (array shape::%shape #!rest vals)
      (array-length array::%array dim)
      (array-size array::%array)
      (array-equal? arr1 arr2)
      (array-rank array::%array)
      (array-start array::%array k)
      (array-end array::%array k)
      (array-shape array::%array)
      (array-ref array::%array #!rest indices)
      (array-set! array::%array #!rest indices+val)
      (share-array array::%array shape::%shape proc::procedure)
      (array-ref2 array::%array i1 i2)
      (array-set2! array::%array i1 i2 val)
      (array-ref1 array::%array i1)
      (array-set1! array::%array i1 val)
      (array-set0! array::%array val)
      (array-ref0 array::%array)

      (shape::%shape #!rest bounds)
      (inline shape? obj)
      (inline shape-rank shape::%shape)
      (inline shape-start shape::%shape dim)
      (inline shape-end shape::%shape dim)
      (inline shape-copy shape::%shape)
      (inline shape-bounded? shape::%shape dim v)
      
      
      ))

(define (pairwise-non-decreasing? lst)
   (let ((len (length lst)))
      (and (even? len)
           (bind-exit (return)
              (let loop ((lst lst))
                 (if (pair? lst)
                     (let ((lower (car lst))
                           (upper (cadr lst)))
                        (if (<=fx lower upper)
                            (loop (cddr lst))
                            (return #f)))
                     #t))))))

(define-inline (shape? obj)
   (isa? obj %shape))

(define (shape #!rest bounds)
   (if (pairwise-non-decreasing? bounds)
       (let ((res (make-vector (/fx (length bounds) 2))))
          (do ((i 0 (+fx i 1))
               (lst bounds (cddr lst)))
              ((=fx i (vector-length res)) (instantiate::%shape (vec res)))
              (vector-set! res i (cons (car lst) (cadr lst)))))
       (error "shape"
          "bounds must be an even number of integers that are pairwise non-decreasing"
          bounds)))

(define-inline (shape-rank shape::%shape)
   (vector-length (-> shape vec)))

(define-inline (shape-start shape::%shape dim)
   (if (<fx dim (shape-rank shape))
       (car (vector-ref (-> shape vec) dim))
       (raise (instantiate::&index-out-of-bounds-error
                 (proc "shape-start")
                 (msg "invalid index")
                 (obj shape)
                 (index dim)))))

(define-inline (shape-length shape::%shape dim)
   (-fx (shape-end shape dim)
      (shape-start shape dim)))

(define-inline (shape-end shape::%shape dim)
   (if (<fx dim (shape-rank shape))
       (cdr (vector-ref (-> shape vec) dim))
       (raise (instantiate::&index-out-of-bounds-error
                 (proc "shape-end")
                 (msg "invalid index")
                 (obj shape)
                 (index dim)))))

(define-inline (shape-bounded? shape::%shape dim v)
   (and (>=fx v (shape-start shape dim))
        (<fx v (shape-end shape dim))))

(define (shape->num-elements shape::%shape)
   (do ((i 0 (+fx i 1))
        (num-elements 1
           (let* ((bound (vector-ref (-> shape vec) i))
                  (bound-len (-fx (cdr bound)
                                (car bound))))
              (*fx num-elements bound-len))))
       ((=fx i (vector-length (-> shape vec))) num-elements)))


(define-inline (index-normalize lower-bound index)
      (+fx index (-fx 0 lower-bound)))

(define (bound-size bound::pair)
   (-fx (cdr bound) (car bound)))

(define (bounded? bound::pair v)
   (and (>=fx v (car bound))
        (<fx v (cdr bound))))

(define (all-bounded? shape::%shape indices)
   (bind-exit (return)
      (do ((i 0 (+fx i 1))
           (lst indices (cdr lst)))
          ((=fx i (shape-rank shape)) #t)
          (let ((bound (vector-ref (-> shape vec) i)))
             (when (not (bounded? bound (car lst)))
                (return #f))))))

(define (shape->index-procedure shape::%shape)
   (let ((rank (shape-rank shape)))
      (cond ((=fx rank 0)
             (lambda (#!optional val)
                (values 0 val)))
            ((=fx rank 1)
             (let ((lower (shape-start shape 0)))
                (lambda (index #!optional val)
                   (if (shape-bounded? shape 0 index)
                       (values (index-normalize lower index)
                          val)
                       (error "indexing" "invalid index" (list index))))))
            ((=fx rank 2)
             (let ((lower0 (shape-start shape 0))
                   (lower1 (shape-start shape 1))
                   (length1 (shape-length shape 1)))
                (lambda (i0 i1 #!optional val)
                   (if (and (shape-bounded? shape 0 i0)
                            (shape-bounded? shape 1 i1))
                       (values (+fx (*fx (index-normalize lower0 i0)
                                       length1)
                                  (index-normalize lower1 i1))
                          val)
                       (error "indexing" "invalid indices" (list i0 i1))))))
            (else (lambda indices+val
                     (if (and (>fx rank 0)
                              (>=fx (length indices+val) rank)
                              (all-bounded? shape indices+val))
                         (do ((i 0 (+ i 1))
                              (lst indices+val (cdr lst))
                              (offset 0
                                 (let ((lower (shape-start shape i)))
                                    (+fx (*fx offset (shape-length shape i))
                                       (index-normalize lower (car lst))))))
                             ((=fx i rank) (values offset
                                              (if (pair? lst) (car lst) '()))))
                         (error "indexing" "invalid indices" indices+val)))))))



(define-inline (shape-copy shape::%shape)
   (let ((new-vec (make-vector (vector-length (-> shape vec)))))
      (do ((i 0 (+fx i 1)))
          ((=fx i (vector-length (-> shape vec))))
          (let* ((bound (vector-ref (-> shape vec) i))
                 (lower (car bound))
                 (upper (cdr bound)))
             (vector-set! new-vec i (cons lower upper))))
      (instantiate::%shape (vec new-vec))))

(define-inline (array? obj)
   (isa? obj %array-base))

(define (array-length array::%array dim)
   (-fx (array-end array dim)
      (array-start array dim)))

(define (array-size array::%array)
   (do ((i 0 (+fx i 1))
        (size 1 (*fx size (array-length array i))))
       ((=fx i (array-rank array)) size)))

(define (array-equal? arr1 arr2)
   (equal? arr1 arr2))

(define (make-array shape::%shape #!optional (default #unspecified))
   (instantiate::%array (index (shape->index-procedure shape))
                          (shape (shape-copy shape))
                          (vec (make-vector (shape->num-elements shape)
                                  default))))

(define (array shape::%shape #!rest vals)
   (if (= (shape->num-elements shape) (length vals))
       (let ((new-array::%array (make-array shape)))
          (let loop ((i 0)
                     (lst vals))
             (if (pair? lst)
                 (let ((curr (car lst)))
                    (vector-set! (-> new-array vec) i curr)
                    (loop (+fx i 1)
                       (cdr lst)))
                 new-array)))
       (error "array" "provided values and shape are not compatible"
          (cons shape vals))))

(define (array-shape array::%array)
   (shape-copy (-> array shape)))

(define (array-rank array::%array)
   (vector-length (-> array shape vec)))

(define (array-start array::%array k)
   (car (vector-ref (-> array shape vec) k)))

(define (array-end array::%array k)
   (cdr (vector-ref (-> array shape vec) k)))

(define (vector-or-rank-1-array? x)
   (or (vector? x)
       (and (array? x)
            (=fx (array-rank x) 1)
            (=fx (array-start x 0) 0))))

(define (vector/rank-1-array->list x)
   (cond ((vector? x)
          (vector->list x))
         ((array? x)
          (do ((i 0 (+fx i 1))
               (res '() (cons (array-ref x i) res)))
              ((=fx i (array-end x 0)) (reverse! res))))
         (else
          (error "vector/rank-1-array->list" "unsupported type" x))))

(define (array-ref0 array::%array)
   (vector-ref (-> array vec) 0))

(define (array-ref1 array::%array i1)
   (if (vector-or-rank-1-array? i1)
       (array-ref array i1)
       (vector-ref (-> array vec)
          ((-> array index) i1))))

(define (array-ref2 array::%array i1 i2)
   (vector-ref (-> array vec)
      ((-> array index) i1 i2)))

(define (array-ref array::%array #!rest indices)
   (let ((array::%array array)
         (len (length indices)))
      (cond  ((and (=fx len 1)
                   (vector-or-rank-1-array? (car indices)))
              (vector-ref (-> array vec)
                 (apply (-> array index)
                    (vector/rank-1-array->list (car indices)))))
             ((>fx len 0)
              (vector-ref (-> array vec)
                 (apply (-> array index) indices)))
             ((=fx len 0)
              (vector-ref (-> array vec) 0))
             (else
              (error "array-ref" "invalid indices: "  indices)))))

(define (array-set0! array::%array val)
   (vector-set! (-> array vec) 0 val))

(define (array-set1! array::%array i1 val)
   (if (vector-or-rank-1-array? i1)
       (array-set! array i1 val)
       (vector-set! (-> array vec)
          ((-> array index) i1) val)))

(define (array-set2! array::%array i1 i2 val)
   (vector-set! (-> array vec)
      ((-> array index) i1 i2) val))

(define (array-set! array::%array #!rest indices+val)
   (let ((array::%array array)
         (len (length indices+val)))
      (cond ((and (=fx len 2)
                  (vector-or-rank-1-array? (car indices+val)))
             (call-with-values
                (lambda ()
                   (apply (-> array index)
                      (append! (vector/rank-1-array->list (car indices+val))
                         (cdr indices+val))))
                (lambda (index val)
                   (vector-set! (-> array vec) index val))))
             ((>=fx len 2)
              (call-with-values
                 (lambda ()
                    (apply (-> array index) indices+val))
                 (lambda (index val)
                    (vector-set! (-> array vec) index val))))
             ((=fx len 1)
              (vector-set! (-> array vec) 0 (car indices+val)))
             (else
              (error "array-set!" "invalid arguments" indices+val)))))

(define (share-array array::%array shape::%shape proc::procedure)
   (let ((copy::%shape (shape-copy shape)))
      (instantiate::%array (shape copy)
                           (index (lambda indices+val
                                     (if (and (>=fx (length indices+val)
                                                 (shape-rank copy))
                                              (all-bounded? copy indices+val))
                                         (call-with-values
                                            (lambda () (apply proc (take indices+val
                                                                 (shape-rank copy))))
                                            (lambda indices
                                               (apply (-> array index)
                                                  (append! indices
                                                     (list-tail indices+val
                                                        (shape-rank copy))))))
                                         (error "indexing" "invalid indices: "
                                            indices+val))))
                           (vec (-> array vec)))))

(define-method (object-equal? shap1::%shape obj)
   (if (not (isa? obj %shape))
       #f
       (let ((shap2::%shape obj))
          (and (= (vector-length (-> shap1 vec))
               (vector-length (-> shap2 vec)))
            (equal? (-> shap1 vec)
               (-> shap2 vec))))))

(define-method (object-equal? arr1::%array obj)
   (if (not (array? obj))
       #f
       (let ((arr2::%array obj))
          (and (equal?  (array-shape arr1) (array-shape arr2))
               (let* ((r (array-rank arr1))
                      (ks (make-vector r 0)))
                  (let loop ((d 0))
                     (if (<fx d r)
                         (let ((e (array-end arr1 d)))
                            (do ((k (array-start arr1 d) (+fx k 1))
                                 (true #t (and true (loop (+fx d 1)))))
                                ((=fx k e) true)
                                (vector-set! ks d k)))
                         (equal? (array-ref arr1 ks)
                            (array-ref arr2 ks)))))))))


