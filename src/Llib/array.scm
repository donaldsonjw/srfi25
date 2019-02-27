(module srfi25/array
   (import srfi25/shape
           srfi25/apply)
   (export
      (abstract-class %array-base
         shape::%shape
         vec::vector)
      (final-class %array::%array-base)
      (final-class %shared-array::%array-base
         index::procedure)
      (inline array? obj)
      (make-array shape::%shape #!optional (default #unspecified))
      (array shape::%shape #!rest vals)
      (inline array-length array::%array-base dim::long)
      (inline array-size array::%array-base)
      (inline array-equal? arr1::%array-base arr2::%array-base)
      (inline array-rank array::%array-base)
      (inline array-start array::%array-base k::long)
      (inline array-end array::%array-base k::long)
      (inline array-shape array::%array-base)
      (array-ref* array::%array-base #!rest indices)
      (array-set*! array::%array-base #!rest indices+val)
      (share-array array::%array-base shape::%shape proc::procedure)
      (inline array-ref2 array::%array-base i1::long i2::long)
      (inline array-set2! array::%array-base i1::long i2::long val)
      (inline array-ref1 array::%array-base i1)
      (inline array-set1! array::%array-base i1 val)
      (inline array-set0! array::%array-base val)
      (inline array-ref0 array::%array-base)
      (inline rank-1-array? x)
      (inline vector/rank-1-array->list x)
      (array-for-each-index arr::%array-base proc::procedure . o)
      (array-tabulate shp::%shape proc::procedure)
      (array-tabulate! shp::%shape proc::procedure ind)
      (array->list arr::%array-base)
      (array->vector arr::%array-base)
      (array-map! arr::%array-base x y . o)
      (array-map x y . o)
      (array-retabulate! arr::%array-base shp::%shape proc::procedure . o)
      (array-append a1::%array-base a2::%array-base #!optional (dim 0))
      (array-copy::%array-base array::%array-base)
      (array-set-w/vector! array::%array-base indices::vector val)
      (array-set-w/array! array::%array-base indices::%array-base val)
      (array-ref-w/vector array::%array-base indices::vector)
      (array-ref-w/array array::%array-base indices::%array-base)
      (array-transpose array::%array)))


(define-inline (index-normalize lower-bound index)
   (-fx index lower-bound))
   
(define (all-bounded? shape::%shape indices::pair-nil)
   (do ((i 0 (+fx i 1))
        (lst indices (cdr lst))
        (res #t (and res (shape-bounded? shape i (car lst)))))
       ((or (not res) (=fx i (shape-rank shape))) res)))


(define (get-array-index-procedure shape::%shape)
   (let ((rank (shape-rank shape)))
      (case rank
         ((0)
          (lambda (shape val)
             (values 0 val)))
         ((1)
          (lambda (index shape val)
             (let ((lower (shape-start shape 0)))
                (if (shape-bounded? shape 0 index)
                    (values (index-normalize lower index)
                       val)
                    (error "indexing" "invalid index" (list index))))))
         ((2)
          (lambda (i0 i1 shape val)
             (let ((lower0 (shape-start shape 0))
                   (lower1 (shape-start shape 1))
                   (length1 (shape-length shape 1)))
                (if (and (shape-bounded? shape 0 i0)
                         (shape-bounded? shape 1 i1))
                    (values (+fx (*fx (index-normalize lower0 i0)
                                    length1)
                               (index-normalize lower1 i1))
                       val)
                    (error "indexing" "invalid indices" (list i0 i1))))))
         ((3)
          (lambda (i0 i1 i2 shape val)
             (let ((lower0 (shape-start shape 0))
                   (lower1 (shape-start shape 1))
                   (lower2 (shape-start shape 2))
                   (length1 (shape-length shape 1))
                   (length2 (shape-length shape 2)))
                (if (and (shape-bounded? shape 0 i0)
                         (shape-bounded? shape 1 i1)
                         (shape-bounded? shape 2 i2))
                    (values (+fx (*fx (+fx (*fx (index-normalize lower0 i0)
                                              length1)
                                         (index-normalize lower1 i1))
                                    length2)
                               (index-normalize lower2 i2))
                       val)
                    (error "indexing" "invalid indices" (list i0 i1))))))
         (else (lambda indices+shape+val
                  (let* ((len (length indices+shape+val))
                         (shape (list-ref indices+shape+val (-fx len 2)))
                         (val (list-ref indices+shape+val (-fx len 1)))
                         (rank (shape-rank shape)))
                     (if (and (>fx rank 0)
                              (=fx len (+ rank 2))
                              (all-bounded? shape indices+shape+val))
                         (do ((i 0 (+fx i 1))
                              (lst indices+shape+val (cdr lst))
                              (offset 0
                                 (let ((lower (shape-start shape i)))
                                    (+fx (*fx offset (shape-length shape i))
                                       (index-normalize lower (car lst))))))
                             ((=fx i rank) (values offset val)))
                         (error "indexing" "invalid indices" indices+shape+val))))))))


(define-inline (array? obj)
   (isa? obj %array-base))

(define-inline (array-length array::%array-base dim::long)
   (-fx (array-end array dim)
      (array-start array dim)))

(define-inline (array-size array::%array-base)
   (do ((i 0 (+fx i 1))
        (size 1 (*fx size (array-length array i))))
       ((=fx i (array-rank array)) size)))

(define-inline (array-equal? arr1 arr2)
   (equal? arr1 arr2))

(define (make-array shape::%shape #!optional (default #unspecified))
   (instantiate::%array (shape (shape-copy shape))
                        (vec (make-vector (shape-size shape)
                                  default))))

(define (array shape::%shape #!rest vals)
   (if (= (shape-size shape) (length vals))
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

(define (array-copy::%array-base array::%array-base)
   (let ((res (make-array (-> array shape))))
      (array-for-each-index array 
         (lambda (iv)
            (array-set-w/vector! res iv (array-ref-w/vector array iv)))
         (make-vector (array-rank array)))
      res))

(define-inline (array-shape array::%array-base)
   (shape-copy (-> array shape)))

(define-inline (array-rank array::%array-base)
   (vector-length (-> array shape vec)))

(define-inline (array-start array::%array-base k::long)
   (car (vector-ref (-> array shape vec) k)))

(define-inline (array-end array::%array-base k::long)
   (cdr (vector-ref (-> array shape vec) k)))

(define-inline (rank-1-array? x)
   (and (array? x)
        (=fx (array-rank x) 1)
        (=fx (array-start x 0) 0)))

(define-inline (vector/rank-1-array->list x)
   (cond ((vector? x)
          (vector->list x))
         ((rank-1-array? x)
          (do ((i 0 (+fx i 1))
               (res '() (cons (array-ref1 x i) res)))
              ((=fx i (array-end x 0)) (reverse! res))))
         (else
          (error "vector/rank-1-array->list" "unsupported type" x))))

(define-inline (array-ref0 array::%array-base)
   (if (isa? array %array)
       (let ((array::%array array))
          (vector-ref (-> array vec) 0))
       (let ((array::%shared-array array))
          (vector-ref (-> array vec)
             ((-> array index) #unspecified)))))

(define (array-ref-w/vector array::%array-base indices::vector)
   (if (isa? array %array)
       (case (vector-length indices)
          ((0) (array-ref0 array))
          ((1) (array-ref1 array (vector-ref indices 0)))
          ((2)
           (array-ref2 array (vector-ref indices 0) (vector-ref indices 1)))
          (else
           (vector-ref (-> array vec)
              (apply-index-proc-to-vector (get-array-index-procedure (-> array shape))
                 indices (-> array shape) #unspecified))))
       (let ((shared-array::%shared-array array))
          (vector-ref (-> shared-array vec)
             (apply-index-proc-to-vector (-> shared-array index) indices
                (-> array shape) #unspecified)))))

(define (array-ref-w/array array::%array-base indices::%array-base)
   (if (isa? array %array)
       (vector-ref (-> array vec)
          (apply-index-proc-to-array (get-array-index-procedure
                              (-> array shape))
              indices  (-> array shape) #unspecified))
       (let ((shared-array::%shared-array array))
          (vector-ref (-> shared-array vec)
             (apply-index-proc-to-array (-> shared-array index)
                 indices (-> shared-array shape) #unspecified)))))

(define-inline (array-ref1 array::%array-base i1)
   (cond ((rank-1-array? i1)
          (array-ref-w/array array i1))
         ((vector? i1)
          (array-ref-w/vector array i1))
         (else
          (if (isa? array %array)
              (let* ((array::%array array)
                     (lower (array-start array 0)))
                 (vector-ref (-> array vec)
                    (-fx i1 lower)))
              (let ((array::%shared-array array))
                 (vector-ref (-> array vec)
                    ((-> array index) i1 (-> array shape) #unspecified)))))))

(define-inline (array-ref2 array::%array-base i1::long i2::long)
   (if (isa? array %array)
       (let* ((array::%array array)
              (lower1 (array-start array 0))
              (lower2 (array-start array 1))
              (length1 (array-length array 1)))
          (vector-ref (-> array vec)
             (+fx (*fx (-fx i1 lower1)
                     length1)
                (-fx i2 lower2))))
       (let ((array::%shared-array array))
          (vector-ref (-> array vec)
             ((-> array index) i1 i2 (-> array shape) #unspecified)))))

(define (get-index-procedure array::%array-base)
   (if (isa? array %array)
       (get-array-index-procedure (-> array shape))
       (let ((shared-array::%shared-array array))
          (-> shared-array index))))

(define (array-ref* array::%array-base #!rest indices)
   (let ((len (length indices)))
      (cond  ((=fx len 0)
              (array-ref0 array))
             ((=fx len 1)
              (array-ref1 array (car indices)))
             ((=fx len 2)
              (array-ref2 array (car indices) (cadr indices)))
             ((>fx len 2)
              (vector-ref (-> array vec)
                 (apply (get-index-procedure array) 
                    (append! indices (list (-> array shape) #unspecified)))))
             (else
              (error "array-ref" "invalid indices: "  indices)))))

(define-inline (array-set0! array::%array-base val)
   (if (isa? array %array)
       (let ((array::%array array))
          (vector-set! (-> array vec) 0 val))
       (let ((array::%shared-array array))
          (vector-set! (-> array vec)
             ((-> array index) (-> array shape) #unspecified) val))))

(define (array-set-w/vector! array::%array-base indices::vector val)
   (if (isa? array %array)
       (case (vector-length indices) 
          ((0) (array-set0! array val))
          ((1) (array-set1! array (vector-ref indices 0) val))
          ((2)
           (array-set2! array (vector-ref indices 0) (vector-ref indices 1)
              val))
          (else
           (vector-set! (-> array vec)
              (apply-index-proc-to-vector (get-array-index-procedure
                                             (-> array shape))
                 indices (-> array shape) val) val)))
       (let ((shared-array::%shared-array array))
          (vector-set! (-> shared-array vec)
             (apply-index-proc-to-vector (-> shared-array index) indices
                (-> shared-array shape)
                val) val))))

(define (array-set-w/array! array::%array-base indices::%array-base val)
   (if (isa? array %array)
       (vector-set! (-> array vec)
          (apply-index-proc-to-array (get-array-index-procedure
                              (-> array shape))
             indices (-> array shape) val) val)
       (let ((shared-array::%shared-array array))
          (vector-set! (-> shared-array vec)
             (apply-index-proc-to-array (-> shared-array index) indices
                (-> shared-array shape) val) val))))

(define-inline (array-set1! array::%array-base i1 val)
   (cond ((rank-1-array? i1)
          (array-set-w/array! array i1 val))
         ((vector? i1)
          (array-set-w/vector! array i1 val))
         (else
          (if (isa? array %array)
              (let* ((array::%array array)
                     (lower (array-start array 0)))
                 (vector-set! (-> array vec)
                    (-fx i1 lower) val))
              (let ((array::%shared-array array))
                 (vector-set! (-> array vec)
                    ((-> array index) i1 (-> array shape) #unspecified) val))))))

(define-inline (array-set2! array::%array-base i1::long i2::long val)
   (if (isa? array %array)
       (let* ((array::%array array)
              (lower1 (array-start array 0))
              (lower2 (array-start array 1))
              (length1 (array-length array 1)))
          (vector-set! (-> array vec)
             (+fx (*fx (-fx i1 lower1)
                     length1)
                (-fx i2 lower2)) val))
       (let ((array::%shared-array array))
          (vector-set! (-> array vec)
             ((-> array index) i1 i2 (-> array shape) #unspecified) val))))

(define (array-set*! array::%array-base #!rest indices+val)
   (let ((len (length indices+val)))
      (cond ((=fx len 1)
             (array-set0! array (car indices+val)))
            ((=fx len 2)
             (array-set1! array (car indices+val)
                (cadr indices+val)))
            ((=fx len 3)
             (array-set2! array (list-ref indices+val 0) (list-ref indices+val 1)
                (list-ref indices+val 2)))
            ((>=fx len 3)
             (call-with-values
                (lambda ()
                   (apply (get-index-procedure array)
                      (append! (take indices+val (-fx len 1))
                         (list (-> array shape)
                            (list-ref indices+val (-fx len 1))))))
                (lambda (index val)
                   (vector-set! (-> array vec) index val))))
            (else
             (error "array-set!" "invalid arguments" indices+val)))))

(define (get-shared-array-index-procedure array::%array-base shp::%shape proc::procedure) 
   (let ((new-shp (shape-copy shp)))
      (values
         (case (shape-rank new-shp)
            ((0)
             (if (isa? array %array)
                 (lambda (shp val)
                    (call-with-values
                       (lambda () (proc))
                       (lambda indices
                          (apply (get-array-index-procedure (-> array shape))
                             (append! indices (list (-> array shape) val))))))
                 (let ((array::%shared-array array))
                    (lambda (shp val)
                    (call-with-values
                       (lambda () (proc))
                       (lambda indices
                          (apply (-> array index)
                             (append! indices (list (-> array shape) val)))))))))
            ((1)
             (lambda (index shp val)
                (if (isa? array %array)
                    (call-with-values
                       (lambda () (proc index))
                       (lambda indices
                          (apply (get-array-index-procedure (-> array shape))
                             (append! indices (list (-> array shape) val)))))
                    (let ((array::%shared-array array))
                       (call-with-values
                          (lambda () (proc index))
                          (lambda indices
                             (apply (-> array index)
                                (append! indices (list (-> array shape) val)))))))))
            ((2)
             (lambda (i1 i2 shp val)
                (if (isa? array %array)
                    (call-with-values
                       (lambda () (proc i1 i2))
                       (lambda indices
                          (apply (get-array-index-procedure (-> array shape))
                             (append! indices (list (-> array shape) val)))))
                    (let ((array::%shared-array array))
                       (call-with-values
                          (lambda () (proc i1 i2))
                          (lambda indices
                             (apply (-> array index)
                                (append! indices (list (-> array shape) val)))))))))
            ((3)
             (lambda (i1 i2 i3 shp val)
                (if (isa? array %array)
                    (call-with-values
                       (lambda () (proc i1 i2 i3))
                       (lambda indices
                          (apply (get-array-index-procedure (-> array shape))
                             (append! indices (list (-> array shape) val)))))
                    (let ((array::%shared-array array))
                       (call-with-values
                          (lambda () (proc i1 i2 i3))
                          (lambda indices
                             (apply (-> array index)
                                (append! indices (list (-> array shape) val)))))))))
            (else
             (if (isa? array %array)
                 (lambda indices+shape+val
                    
                    (let* ((len (length indices+shape+val))
                           (shape (list-ref indices+shape+val (-fx len 2)))
                           (val (list-ref indices+shape+val (-fx len 1))))
                       (if (and (=fx (-fx len 2)
                                   (shape-rank shape))
                                (all-bounded? shape indices+shape+val))
                           (call-with-values
                              (lambda () (apply proc (take indices+shape+val
                                                   (shape-rank shape))))
                              (lambda indices
                                 (apply (get-array-index-procedure (-> array shape))
                                    (append! indices
                                       (list (-> array shape) val)))))
                           (error "indexing" "invalid indices: "
                              indices+shape+val))))
                 (let ((array::%shared-array array))
                    (lambda indices+shape+val
                       
                       (let* ((len (length indices+shape+val))
                              (shape (list-ref indices+shape+val (-fx len 2)))
                              (val (list-ref indices+shape+val (-fx len 1))))
                          (if (and (=fx (-fx len 2)
                                      (shape-rank shape))
                                   (all-bounded? shape indices+shape+val))
                              (call-with-values
                                 (lambda () (apply proc (take indices+shape+val
                                                      (shape-rank shape))))
                                 (lambda indices
                                    (apply (-> array index)
                                       (append! indices
                                          (list (-> array shape) val)))))
                              (error "indexing" "invalid indices: "
                                 indices+shape+val))))))))
         new-shp)))


(define (share-array array::%array-base shape::%shape proc::procedure)
   (receive (index-proc copy)
      (get-shared-array-index-procedure array shape proc)
      (instantiate::%shared-array (shape copy)
                                  (index index-proc)
                                  (vec (-> array vec)))))


(define-method (object-equal? arr1::%array-base obj)
   (if (not (array? obj))
       #f
       (let ((arr2::%array-base obj))
          (and (equal?  (-> arr1 shape) (-> arr2 shape))
               (let* ((r (array-rank arr1))
                      (ks (make-vector r 0)))
                  (let loop ((d 0))
                     (if (<fx d r)
                         (let ((e (array-end arr1 d)))
                            (do ((k (array-start arr1 d) (+fx k 1))
                                 (true #t (and true (loop (+fx d 1)))))
                                ((=fx k e) true)
                                (vector-set! ks d k)))
                         (let ((res (equal? (array-ref-w/vector arr1 ks)
                                       (array-ref-w/vector arr2 ks))))
                            res))))))))


;;; array-for-each-index from arlib provided with srfi25 
(define (array-for-each-index arr::%array-base proc::procedure . o)
  (if (null? o)
      (array-for-each-index/arguments arr proc)
      (if (vector? (car o))
          (array-for-each-index/vector arr proc (car o))
          (array-for-each-index/array arr proc (car o)))))

(define (array-for-each-index/arguments arr::%array-base proc::procedure)
  (let ((r (array-rank arr)))
    (let ((vec (make-vector r)))
      (let do-dim ((d 0))
        (if (=fx d r)
            (apply-to-vector proc vec)
            (let ((e (array-end arr d)))
              (do ((k (array-start arr d) (+fx k 1)))
                ((=fx k e))
                (vector-set! vec d k)
                (do-dim (+fx d 1)))))))))

(define (array-for-each-index/vector arr::%array-base proc::procedure ind::vector)
  ;; ind is a vector
   (let ((r (array-rank arr)))
      (let do-dim ((d 0))
         (if (= d r)
             (proc ind)
             (let ((e (array-end arr d)))
                (do ((k (array-start arr d) (+fx k 1)))
                    ((=fx k e))
                    (vector-set! ind d k)
                    (do-dim (+fx d 1))))))))

(define (array-for-each-index/array arr::%array-base proc::procedure ind::%array-base)
  ;; ind is an array but not a vector
  (let ((r (array-rank arr)))
    (let do-dim ((d 0))
      (if (= d r)
          (proc ind)
          (let ((e (array-end arr d)))
            (do ((k (array-start arr d) (+fx k 1)))
              ((=fx k e))
              (array-set1! ind d k)
              (do-dim (+fx d 1))))))))


;;; array-tabulate and friends from arlib provided with srfi25
(define (array-tabulate shp::%shape proc::procedure)
  (let ((arr (make-array shp)))
     
     (shape-for-each
        shp
        (lambda (ix) (array-set*! arr ix (apply-to-vector proc ix)))
        (make-vector (shape-rank shp)))
     arr))
        
(define (array-tabulate! shp::%shape proc::procedure ind)
   (let ((arr (make-array shp)))
      (shape-for-each
         shp
         (lambda (ix) (array-set*! arr ix (proc ix)))
         ind)
      arr))

(define (array-retabulate! arr::%array-base shp::%shape proc::procedure . o)
  (if (null? o)
      (shape-for-each
       shp
       (lambda (ix)
          (array-set-w/vector! arr ix (apply-to-vector proc ix)))
       (make-vector (shape-rank shp)))
      (shape-for-each
         shp
         (lambda (ix)
            (array-set*! arr ix (proc ix)))
         (car o))))


;;; array-map and friends from arlib provided with srfi25
(define (array-map! arr::%array-base x y . o)
  (if (shape? x)
      (array-map/vector! arr x y (apply vector o))
      (array-map/vector! arr (-> arr shape) x (apply vector y o))))

(define (array-map/vector! arr::%array-base shp::%shape proc::procedure args::vector)
  (let ((rank (vector-length args)))
    (let ((argv (make-vector rank)))
      (shape-for-each
         shp
         (lambda (ix)
            (do ((k 0 (+fx k 1)))
                ((=fx k rank))
                (vector-set! argv k (array-ref-w/vector (vector-ref args k) ix)))
            (array-set-w/vector! arr ix (apply-to-vector proc argv)))
         (make-vector (shape-rank shp))))))

(define (array-map x y . o)
  (if (shape? x)
      (let ((arr (make-array x)))
        (array-map/vector! arr x y (apply vector o))
        arr)
      (let* ((ya::%array-base y)
            (shp (-> ya shape)))
        (let ((arr (make-array shp)))
          (array-map/vector! arr shp x (apply vector y o))
          arr))))


(define (array->vector arr::%array-base)
   (let ((vec (make-vector (array-size arr))))
      (let ((k 0))
         (shape-for-each
            (array-shape arr)
            (lambda (index)
               (vector-set! vec k (array-ref-w/vector arr index))
               (set! k (+fx k 1)))
            (make-vector (array-rank arr)))
         vec)))

(define (array->list arr::%array-base)
   (let ((lst '()))
      (shape-for-each
         (array-shape arr)
         (lambda (index)
            (set! lst (cons (array-ref-w/vector arr index) lst)))
         (make-vector (array-rank arr)))
      (reverse! lst)))


(define (calculate-append-shape a1::%array-base a2::%array-base dim)
   (do ((i 0 (+fx i 1))
        (res '() (cons* (if (=fx i dim)
                            (+fx (array-end a1 i)
                               (array-length a2 i))
                            (array-end a1 i))
                    (array-start a1 i)
                    res)))
       ((= i (array-rank a1))
        (apply shape (reverse! res)))))


;;; heavily modeled off of the implementation of array-concatenate in gauche scheme
(define (array-append a1::%array-base a2::%array-base #!optional (dim 0))
   (if  (and (=fx (array-rank a1) (array-rank a2))
             (do ((i 0 (+fx i 1))
                  (res #t (and res (=fx (array-length a1 i)
                                      (array-length a2 i)))))
                 ((or (=fx i dim)
                      (=fx i (array-rank a1))) res)))
        (let* ((new-shape (calculate-append-shape a1 a2 dim))
               (new-array (make-array new-shape)))
           (array-for-each-index a1
              (lambda (indices) (array-set-w/vector! new-array indices (array-ref-w/vector a1 indices)))
              (make-vector (array-rank a1)))
           (array-for-each-index a2
              (lambda (indices)
                 (let ((new-indices (copy-vector indices (vector-length indices))))
                    (do ((i 0 (+fx i 1)))
                        ((=fx i (array-rank a1)))
                        (vector-set! new-indices i (+fx (vector-ref new-indices i)
                                                      (-fx (array-start a1 i)
                                                         (array-start a2 i)))))
                    (vector-set! new-indices dim
                       (+fx (vector-ref new-indices dim)
                          (array-length a1 dim)))
                    (array-set-w/vector! new-array new-indices (array-ref-w/vector a2 indices)))
                 new-array)
              (make-vector (array-rank a2)))
           new-array)
        (error "array-append" "incompatible arrays" (list a1 a2))))


(define (array-transpose array::%array)
   (if (=fx (array-rank array) 2)
       (share-array array (shape (array-start array 1) (array-end array 1)
                             (array-start array 0) (array-end array 0))
          (lambda (i j) (values j i)))
       (error "array-transpose" "transpose only supported for rank 2 arrays" array)))


