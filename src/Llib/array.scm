(module srfi25/array
   (export
      (class %shape
         vec::vector)
      (abstract-class %array-base)
      (class %array::%array-base
         index::procedure
         shape::%shape
         vec::vector)

      (shape::%shape #!rest bounds)
      (inline shape? obj)
      (inline shape-rank shape::%shape)
      (inline shape-start shape::%shape dim)
      (inline shape-end shape::%shape dim)
      (inline shape-copy shape::%shape)
      
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
      (share-array array::%array shape::%shape proc::procedure)))

(define (pairwise-non-decreasing? lst)
   (let ((len (length lst)))
      (and (even? len)
           (bind-exit (return)
              (let loop ((lst lst))
                 (if (pair? lst)
                     (let ((lower (car lst))
                           (upper (cadr lst)))
                        (if (<= lower upper)
                            (loop (cddr lst))
                            (return #f)))
                     #t))))))

(define-inline (shape? obj)
   (isa? obj %shape))

(define (shape #!rest bounds)
   (if (pairwise-non-decreasing? bounds)
       (let ((res (make-vector (/fx (length bounds) 2))))
          (do ((i 0 (+ i 1))
               (lst bounds (cddr lst)))
              ((= i (vector-length res)) (instantiate::%shape (vec res)))
              (vector-set! res i (cons (car lst) (cadr lst)))))
       (error "shape"
          "bounds must be an even number of integers that are pairwise non-decreasing"
          bounds)))

(define-inline (shape-rank shape::%shape)
   (vector-length (-> shape vec)))

(define-inline (shape-start shape::%shape dim)
   (car (vector-ref (-> shape vec) dim)))

(define-inline (shape-end shape::%shape dim)
   (cdr (vector-ref (-> shape vec) dim)))


(define (shape->num-elements shape::%shape)
   (do ((i 0 (+ i 1))
        (num-elements 1
           (let* ((bound (vector-ref (-> shape vec) i))
                  (bound-len (- (cdr bound)
                                (car bound))))
              (* num-elements bound-len))))
       ((= i (vector-length (-> shape vec))) num-elements)))


(define (index-normalize bound::pair index)
      (+ index (- 0 (car bound))))

(define (bound-size bound::pair)
   (- (cdr bound) (car bound)))

(define (bounded? bound::pair v)
   (and (>= v (car bound))
        (< v (cdr bound))))

(define (all-bounded? shape::%shape indices)
   (bind-exit (return)
      (do ((i 0 (+ i 1))
           (lst indices (cdr lst)))
          ((= i (vector-length (-> shape vec))) #t)
          (let ((bound (vector-ref (-> shape vec) i)))
             (when (not (bounded? bound (car lst)))
                (return #f))))))

(define (shape->index-procedure shape::%shape)
   (let ((len (vector-length (-> shape vec))))
      (lambda (indices context)
         (if (and (= (length indices) len)
                  (all-bounded? shape indices))
             (do ((i 0 (+ i 1))
                  (lst indices (cdr lst))
                  (offset 0
                     (let ((bound (vector-ref (-> shape vec) i)))
                        (+ (* offset (bound-size bound))
                           (index-normalize bound (car lst))))))
                 ((= i len) offset))
             (error context "invalid indices" indices)))))


(define-inline (shape-copy shape::%shape)
   (instantiate::%shape (vec (copy-vector (-> shape vec)
                                (vector-length (-> shape vec))))))

(define-inline (array? obj)
   (isa? obj %array-base))

(define (array-length array::%array dim)
   (- (array-end array dim)
      (array-start array dim)))

(define (array-size array::%array)
   (do ((i 0 (+ i 1))
        (size 1 (* size (array-length array i))))
       ((= i (array-rank array)) size)))

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
                    (loop (+ i 1)
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

(define (array-dim-length array::%array k)
   (- (array-end array k)
      (array-start array k)))

(define (vector-or-rank-1-array? x)
   (or (vector? x)
       (and (array? x)
            (= (array-rank x) 1)
            (= (array-start x 0) 0))))

(define (vector/rank-1-array->list x)
   (let ((vec (if (array? x)
                  (let ((a::%array x))
                     (-> a vec))
                  x)))
      (vector->list vec)))

(define (array-ref array::%array #!rest indices)
   (let ((len (length indices)))
      (cond  ((and (= len 1)
                   (vector-or-rank-1-array? (car indices)))
              (vector-ref (-> array vec)
                 ((-> array index) (vector/rank-1-array->list (car indices))
                                   "array-ref")))
             ((> len 0)
              (vector-ref (-> array vec)
                 ((-> array index) indices "array-ref")))
             (else
              (error "array-ref" "invalid indices: "  indices)))))

(define (array-set! array::%array #!rest indices+val)
   (let ((len (length indices+val)))
      (cond ((and (= len 2)
                  (vector-or-rank-1-array? (car indices+val)))
             (vector-set! (-> array vec)
                ((-> array index) (vector/rank-1-array->list (car indices+val))
                                  "array-set!") (cadr indices+val)))
            ((>= len 2)
             (vector-set! (-> array vec)
                ((-> array index) (take indices+val (- len 1)) "array-set!")
                (car (last-pair indices+val))))
            (else
             (error "array-set!" "invalid arguments" indices+val)))))

(define (share-array array::%array shape::%shape proc::procedure)
   (let ((copy::%shape (shape-copy shape)))
      (instantiate::%array (shape copy)
                           (index (lambda (indices context)
                                     (if (and (= (length indices)
                                                 (vector-length (-> copy vec)))
                                              (all-bounded? copy indices))
                                         (call-with-values
                                            (lambda () (apply proc indices))
                                            (lambda indices
                                               ((-> array index) indices
                                                                 context)))
                                         (error context "invalid indices: "
                                            indices))))
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
   (if (not (isa? obj %array))
       #f
       (let ((arr2::%array obj))
          (and (isa? obj %array)
               (equal? (-> arr1 shape) (let ((arr2::%array obj))
                                          (-> arr2 shape)))
               (let* ((r (array-rank arr1))
                      (ks (make-vector r 0)))
                  (let loop ((d 0))
                     (if (< d r)
                         (let ((e (array-end arr1 d)))
                            (do ((k (array-start arr1 d) (+ k 1))
                                 (true #t (and true (loop (+ d 1)))))
                                ((= k e) true)
                                (vector-set! ks d k)))
                         (equal? (array-ref arr1 ks)
                            (array-ref arr2 ks)))))))))





