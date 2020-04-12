(module srfi25/shape
   (include "vector-utils.sch")
   (import srfi25/store
           srfi25/array
           srfi25/array-base
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
      (inline shape-translation-vector shp::%shape)
      (inline shape-coefficient-vector shp::%shape)
      (inline shape->startv/endv shp::%shape)
      (inline shape-for-each shp::%shape proc::procedure . o)
      (inline shape-for-each/arguments shp::%shape proc::procedure)
      (inline shape-for-each/vector shp::%shape proc::procedure vec::vector)
      (inline shape-for-each/array shp::%shape proc::procedure arr::%array-base)))


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
       (instantiate::%shape (vec (list->vector bounds)))
       (error "shape"
          "bounds must be an even number of integers that are pairwise non-decreasing"
          bounds)))

(define-inline (shape->list shape::%shape)
   (vector->list (-> shape vec)))

(define (shape->array shp::%shape)
   (let ((lst (shape->list shp)))
      (apply array (cons (shape 0 (/fx (length lst) 2) 0 2) lst))))


(define-inline (shape-rank shape::%shape)
   (/fx (vector-length (-> shape vec)) 2))

(define-inline (shape-start shape::%shape dim)
   (if (<fx dim (shape-rank shape))
       (vector-ref (-> shape vec) (*fx dim 2))
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
       (vector-ref (-> shape vec) (+fx 1 (*fx dim 2)))
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
           (*fx num-elements (shape-length shape i))))
       ((=fx i (shape-rank shape)) num-elements)))

(define-inline (shape-copy shape::%shape)
   (instantiate::%shape (vec (copy-vector (-> shape vec)
                                (vector-length (-> shape vec))))))


(define-inline (shape->startv/endv shp::%shape)
   (let* ((rank (shape-rank shp))
          (startv (make-vector rank))
          (endv (make-vector rank)))
      (do ((i 0 (+fx i 1)))
          ((= i rank) (values startv endv))
          (vector-set! startv i (shape-start shp i))
          (vector-set! endv i (shape-end shp i)))))

(define-inline (shape-translation-vector shp::%shape)
   (let ((res (make-vector (shape-rank shp))))
      (do ((i 0 (+fx i 1)))
          ((=fx i (shape-rank shp)) res)
          (vector-set! res i (shape-start shp i)))))

(define-inline (shape-coefficient-vector shp::%shape)
   (let* ((rank  (shape-rank shp))
          (res (make-vector rank 1)))
      (let ((offset 1))
         (do ((i (-fx rank 1) (-fx i 1)))
             ((<fx i 0) res)
             (set! offset (*fx offset
                             (if (<fx (+fx i 1) rank)
                                 (shape-length shp (+fx i 1))
                                 1)))
             (vector-set! res i offset)))))


;;;; shape-for-each from arlib provided with srfi25
(define-inline (shape-for-each shp::%shape proc::procedure . o)
  (if (null? o)
      (shape-for-each/arguments shp proc)
      (if (vector? (car o))
          (shape-for-each/vector shp proc (car o))
          (shape-for-each/array shp proc (car o)))))

(define-inline (shape-for-each/arguments shp::%shape proc::procedure)
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

(define-inline (shape-for-each/vector shp::%shape proc::procedure vec::vector)
  (let ((r (shape-rank shp)))
    (let do-dim ((d 0))
      (if (= d r)
          (proc vec)
          (let ((e (shape-end shp d)))
             (do ((k (shape-start shp d) (+ k 1)))
              ((= k e))
              (vector-set! vec d k)
              (do-dim (+ d 1))))))))

(define-inline (shape-for-each/array shp::%shape proc::procedure arr::%array-base)
  ;; arr is not vector
  (let ((r (shape-rank shp)))
    (let do-dim ((d 0))
      (if (= d r)
          (proc arr)
          (let ((e (shape-end shp d)))
            (do ((k (shape-start shp d) (+ k 1)))
              ((= k e))
              (array-set1! arr d k)
              (do-dim (+ d 1))))))))

(define-method (object-equal? shap1::%shape obj)
   (if (not (isa? obj %shape))
       #f
       (let ((shap2::%shape obj))
          (equal? (-> shap1 vec)
             (-> shap2 vec)))))

(define-method (object-equal? arr1::%array-base obj)
   (if (not (array? obj))
       #f
       (let ((arr2::%array-base obj))
          (and (equal?  (array-shape arr1) (array-shape arr2))
               (let* ((r (array-rank arr1))
                      (ks (make-vector r)))
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


