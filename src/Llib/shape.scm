(module srfi25/shape
   (include "array.sch")
   (import srfi25/array
           srfi25/apply)
   (export
      (class %shape
         vec::vector)      
      (shape::%shape #!rest bounds)
      (inline shape? obj)
      (inline shape-rank shape::%shape)
      (inline shape-start shape::%shape dim::long)
      (inline shape-end shape::%shape dim::long)
      (inline shape-size shape::%shape)
      (inline shape-length shape::%shape dim::long)
      (inline shape-copy shape::%shape)
      (inline shape-bounded? shape::%shape dim::long v::long)
      (inline shape->list shape::%shape)
      (shape->array shp::%shape)
      (shape-for-each shp::%shape proc::procedure . o)))


(define-method (object-hashnumber obj::%shape)
   (do ((i 0 (+fx i 1))
        (res 0))
       ((=fx i (vector-length (-> obj vec))) res)

       (let ((start (car (vector-ref (-> obj vec) i)))
             (end (cdr (vector-ref (-> obj vec) i))))
          (set! res (bit-xor res
                       (bit-xor (get-hashnumber start)
                          (get-hashnumber end)))))))

(define-inline (pairwise-non-decreasing?::bool lst)
   (let ((len (length lst)))
      (and (evenfx? len)
           (let loop ((lst lst))
              (if (pair? lst)
                  (let ((lower (car lst))
                        (upper (cadr lst)))
                     (if (<=fx lower upper)
                         (loop (cddr lst))
                         #f))
                  #t)))))

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

(define-inline (shape->list shape::%shape)
   (do ((i 0 (+fx i 1))
        (res '() (cons* (cdr (vector-ref (-> shape vec) i))
                    (car (vector-ref (-> shape vec) i))
                    res)))
       ((= i (vector-length (-> shape vec))) (reverse! res))))

(define (shape->array shp::%shape)
   (let ((lst (shape->list shp)))
      (apply array (cons (shape 0 (/fx (length lst) 2) 0 2) lst))))


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

(define-inline (shape-size shape::%shape)
   (do ((i 0 (+fx i 1))
        (num-elements 1
           (let* ((bound (vector-ref (-> shape vec) i))
                  (bound-len (-fx (cdr bound)
                                (car bound))))
              (*fx num-elements bound-len))))
       ((=fx i (vector-length (-> shape vec))) num-elements)))

(define-inline (shape-copy shape::%shape)
   (let ((new-vec (make-vector (vector-length (-> shape vec)))))
      (do ((i 0 (+fx i 1)))
          ((=fx i (vector-length (-> shape vec))))
          (let* ((bound (vector-ref (-> shape vec) i))
                 (lower (car bound))
                 (upper (cdr bound)))
             (vector-set! new-vec i (cons lower upper))))
      (instantiate::%shape (vec new-vec))))


;;;; shape-for-each from arlib provided with srfi25
(define (shape-for-each shp::%shape proc::procedure . o)
   (print "calling shape-for-each")
  (if (null? o)
      (shape-for-each/arguments shp proc)
      (if (vector? (car o))
          (shape-for-each/vector shp proc (car o))
          (shape-for-each/array shp proc (car o)))))

(define (shape-for-each/arguments shp::%shape proc::procedure)
   (let ((r (shape-rank shp)))
      (let ((vec (make-vector r)))
         (let do-dim ((d 0))
            (if (= d r)
                (apply-to-vector proc vec)
                (let ((e (shape-end shp d)))
                   (do ((k (shape-start shp d) (+ k 1)))
                       ((= k e))
                       (vector-set! vec d k)
                       (do-dim (+ d 1)))))))))

(define (shape-for-each/vector shp::%shape proc::procedure vec::vector)
  (let ((r (shape-rank shp)))
    (let do-dim ((d 0))
      (if (= d r)
          (proc vec)
          (let ((e (shape-end shp d)))
             (do ((k (shape-start shp d) (+ k 1)))
              ((= k e))
              (vector-set! vec d k)
              (do-dim (+ d 1))))))))

(define (shape-for-each/array shp::%shape proc::procedure arr::%array-base)
  ;; arr is not vector
  (let ((r (shape-rank shp)))
    (let do-dim ((d 0))
      (if (= d r)
          (proc arr)
          (let ((e (shape-end shp d)))
            (do ((k (shape-start shp d) (+ k 1)))
              ((= k e))
              (array-set! arr d k)
              (do-dim (+ d 1))))))))

(define-method (object-equal? shap1::%shape obj)
   (if (not (isa? obj %shape))
       #f
       (let ((shap2::%shape obj))
          (equal? (-> shap1 vec)
             (-> shap2 vec)))))


