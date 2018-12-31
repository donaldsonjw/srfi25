(module srfi25/array-lib
   (import srfi25/array)
   (export
      (shape-for-each shp::%shape proc::procedure . o)
      (array-for-each-index arr::%array-base proc::procedure . o)
      
      (tabulate-array shp::%shape proc::procedure)
      (tabulate-array! shp::%shape proc::procedure ind)
      (array-retabulate! arr::%array-base shp::%shape proc::procedure . o)
      (array-map! arr::%array-base x y . o)
      (array-map x y . o)
      (array->vector arr::%array-base)
      (array->list arr::%array-base)
      (share-row arr::%array-base k)
     ;(share-array/prefix arr::%array-base . js)
      ;(share-array/prefix! arr::%array-base fix in . out)
      (share-column arr::%array-base k)
      (share-array/origin arr::%array-base . xs)
      (transpose a::%array . p0)))


(define-inline (array:apply-to-vector r proc vec)
   (apply proc (vector->list vec)))

;; adaptation of arlib provided with srfi25

;; (shape-for-each shp proc [index-object])
;;; passes each index in shape to proc in row-major orderd, using
;;; index-object if provided.

(define (shape-for-each shp::%shape proc::procedure . o)
  (if (null? o)
      (array:arlib:shape-for-each/arguments shp proc)
      (if (vector? (car o))
          (array:arlib:shape-for-each/vector shp proc (car o))
          (array:arlib:shape-for-each/array shp proc (car o)))))

(define (array:arlib:shape-for-each/arguments shp::%shape proc)
   (let ((r (shape-rank shp)))
      (let ((vec (make-vector r)))
         (let do-dim ((d 0))
            (if (= d r)
                (array:apply-to-vector r proc vec)
                (let ((e (shape-end shp d)))
                   (do ((k (shape-start shp d) (+ k 1)))
                       ((= k e))
                       (vector-set! vec d k)
                       (do-dim (+ d 1)))))))))

(define (array:arlib:shape-for-each/vector shp::%shape proc::procedure vec::vector)
  (let ((r (shape-rank shp)))
    (let do-dim ((d 0))
      (if (= d r)
          (proc vec)
          (let ((e (shape-end shp d)))
             (do ((k (shape-start shp d) (+ k 1)))
              ((= k e))
              (vector-set! vec d k)
              (do-dim (+ d 1))))))))

(define (array:arlib:shape-for-each/array shp::%shape proc::procedure arr::%array-base)
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


;;; (array-for-each-index arr proc [ind])
;;; is equivalent to
;;;
;;;   (shape-for-each-index (array-shape arr) proc [ind])
;;;
;;; but is implemented without allocation of the shape, to prove
;;; that it can be so implemented.

(define (array-for-each-index arr::%array-base proc::procedure . o)
  (if (null? o)
      (array:arlib:array-for-each-index/arguments arr proc)
      (if (vector? (car o))
          (array:arlib:array-for-each-index/vector arr proc (car o))
          (array:arlib:array-for-each-index/array arr proc (car o)))))

(define (array:arlib:array-for-each-index/arguments arr::%array-base proc::procedure)
  (let ((r (array-rank arr)))
    (let ((vec (make-vector r)))
      (let do-dim ((d 0))
        (if (= d r)
            (array:apply-to-vector r proc vec)
            (let ((e (array-end arr d)))
              (do ((k (array-start arr d) (+ k 1)))
                ((= k e))
                (vector-set! vec d k)
                (do-dim (+ d 1)))))))))

(define (array:arlib:array-for-each-index/vector arr::%array-base proc::procedure ind::vector)
  ;; ind is a vector
  (let ((r (array-rank arr)))
    (let do-dim ((d 0))
      (if (= d r)
          (proc ind)
          (let ((e (array-end arr d)))
            (do ((k (array-start arr d) (+ k 1)))
              ((= k e))
              (vector-set! ind d k)
              (do-dim (+ d 1))))))))

(define (array:arlib:array-for-each-index/array arr::%array-base proc::procedure ind::%array-base)
  ;; ind is an array but not a vector
  (let ((r (array-rank arr)))
    (let do-dim ((d 0))
      (if (= d r)
          (proc ind)
          (let ((e (array-end arr d)))
            (do ((k (array-start arr d) (+ k 1)))
              ((= k e))
              (array-set! ind d k)
              (do-dim (+ d 1))))))))

;;; (tabulate-array shp proc)
;;; (tabulate-array! shp proc ind)
;;; returns a newly allocated array of the given shape with initial
;;; contents at each index whatever proc returns given the indices.
;;; The latter procedure reuses ind for package of indices.

(define (tabulate-array shp::%shape proc::procedure)
  (let ((arr (make-array shp)))
    (array:arlib:shape-for-each/vector
     shp
     (lambda (ix) (array-set! arr ix (array:apply-to-vector (shape-rank shp) proc ix)))
     (make-vector (shape-rank shp)))
    arr))
        
(define (tabulate-array! shp::%shape proc::procedure ind)
  (let ((arr (make-array shp)))
    (if (vector? ind)
        (array:arlib:shape-for-each/vector
         shp
         (lambda (ix) (array-set! arr ix (proc ix)))
         ind)
        (array:arlib:shape-for-each/array
         shp
         (lambda (ix) (array-set! arr ix (proc ix)))
         ind))
    arr))

;;; (array-retabulate! arr shp proc [index-object])
;;; sets the elements of arr in shape to the value of proc at that
;;; index, using index-object if provided.

(define (array-retabulate! arr::%array-base shp::%shape proc::procedure . o)
  (if (null? o)
      (array:arlib:shape-for-each/vector
       shp
       (lambda (ix)
          (array-set! arr ix (array:apply-to-vector (shape-rank shp) proc ix)))
       (make-vector (shape-rank shp)))
      (if (vector? (car o))
          (array:arlib:shape-for-each/vector
           shp
           (lambda (ix)
             (array-set! arr ix (proc ix)))
           (car o))
          (array:arlib:shape-for-each/array
           shp
           (lambda (ix)
             (array-set! arr ix (proc ix)))
           (car o)))))

;;; (array-map! array [shape] proc array0 array1...)
;;; stores to the elements of array (in shape) the values of proc at
;;; the contents of arrayk at corresponding index.
(define (array-map! arr::%array-base x y . o)
  (if (shape? x)
      (array:arlib:map! arr x y (apply vector o))
      (array:arlib:map! arr (array-shape arr) x (apply vector y o))))

(define (array:arlib:map! arr::%array-base shp::%shape proc::procedure args::vector)
  (let ((rank (vector-length args)))
    (let ((argv (make-vector rank)))
      (array:arlib:shape-for-each/vector
       shp
       (lambda (ix)
          (do ((k 0 (+ k 1)))
              ((= k rank))
              (vector-set! argv k (array-ref (vector-ref args k) ix)))
          (array-set! arr ix (array:apply-to-vector rank proc argv)))
       (make-vector (shape-rank shp))))))

;;; (array-map [shape] proc array0 array1 ...)
;;; creates a new array with elements initialized to the values of
;;; proc at contents of arrayk (in shape).

(define (array-map x y . o)
  (if (shape? x)
      (let ((arr (make-array x)))
        (array:arlib:map! arr x y (apply vector o))
        arr)
      (let ((shp (array-shape y)))
        (let ((arr (make-array shp)))
          (array:arlib:map! arr shp x (apply vector y o))
          arr))))

;;; SRFI-25 mailing list requested array->vector; they also requested the
;;; ability to use an array as an index of an element, and array->list is
;;; an attempt to provide for that.

(define (array->vector arr::%array-base)
  (let ((vec (make-vector (array-size arr))))
    (let ((k 0))
      (shape-for-each
       (array-shape arr)
       (lambda (index)
         (vector-set! vec k (array-ref arr index))
         (set! k (+ k 1)))
       (make-vector (array-rank arr)))
      vec)))


;;; It needs to be said that more efficient implementations are
;;; possible, even within SRFI-25.

(define (array->list arr::%array-base)
  (vector->list (array->vector arr)))

;;; (share-row arr k)
;;; shares whatever the first index is about.
;;; The result has one dimension less.

(define (share-row arr::%array-base k)
  (share-array
   arr
   (let ((bounds (array->list (array-shape arr))))
     (apply shape (cddr bounds)))
   (lambda ks
     (apply values k ks))))

;;; (share-array/prefix arr k ...)

; (define (share-array/prefix arr::%array-base . js)
;   (if (or (null? js)
;           (integer? (car js)))
;       (share-array
;        arr
;        (let ((bounds (array->list (array-shape arr))))
;           (apply shape (list-tail bounds (* 2 (length js)))))
;        (lambda ks
;           (apply values (append js ks))))
;       (apply (lambda (fix)
;                 (share-array/prefix!
;                    arr
;                    fix
;                    (make-vector (- (array-rank arr)
;                                    (if (vector? fix)
;                                        (vector-length fix)
;                                        (array-end fix 0))))))
;          js)))

; (define (share-array/prefix! arr::%array-base fix in . out)
;   (let* ((out (if (pair? out)
;                   ((lambda (out) out) out)
;                   (make-vector (array-rank arr))))
;          (fix-ref (if (vector? fix) vector-ref array-ref))
;          (in-ref (if (vector? in) vector-ref array-ref))
;          (out-set! (if (vector? out) vector-set! array-set!))
;          (m (if (vector? fix)
;                 (vector-length fix)
;                 (array-end fix 0)))
;          (n (if (vector? out)
;                 (vector-length out)
;                 (array-end out 0))))
;     (do ((k 0 (+ k 1)))
;       ((= k m))
;       (out-set! out k (fix-ref fix k)))
;     (share-array/index!
;      arr
;      (let ((bounds (array->list (array-shape arr))))
;        (apply shape (list-tail bounds (if (vector? fix)
;                                           (* 2 (vector-length fix))
;                                           (* 2 (array-end fix 0))))))
;      (lambda (in)
;        (do ((k m (+ k 1)))
;          ((= k n))
;          (out-set! out k (in-ref in (- k m))))
;        out)
;      in)))

;;; (share-column arr k)
;;; shares whatever the second index is about.
;;; The result has one dimension less.

(define (share-column arr::%array-base k)
   (share-array
      arr
      (let ((bounds (array->list (array-shape arr))))
         (apply shape
            (car bounds) (cadr bounds)
            (cddddr bounds)))
      (lambda ks
         (apply values (car ks) k (cdr ks)))))

;;; (share-array/origin arr k ...)
;;; (share-array/origin arr index)
;;; change origin to k ..., with index a vector or zero-based
;;; one-dimensional array that contains k ...
;;;
;;; This is useful for writing array-append. Maybe for something
;;; else too - who knows.

(define (share-array/origin arr::%array-base . xs)
  (let ((new (if (or (null? xs)
                     (integer? (car xs)))
                 xs
                 (apply (lambda (x)
                          (if (vector? x)
                              (vector->list x)
                              (if (array? x)
                                  (array->list x)
                                  (error "share-array/origin" "all indexes must be vectors or zero-based 1-dimensional arrays" x))))
                        xs))))
    (do ((k (array-rank arr) (- k 1))
         (old '() (cons (array-start arr (- k 1)) old)))
      ((= k 0)
       (let ((ds (map - new old)))
         (share-array
          arr
          (tabulate-array
           (shape 0 (array-rank arr) 0 2)
           (lambda (r k)
             (case k
               ((0) (+ (array-start arr r) (list-ref ds r)))
               ((1) (+ (array-end arr r) (list-ref ds r))))))
          (lambda ks
            (apply values (map - ks ds)))))))))

;;; SRFI-25 mailing list requested making shapes their own type. Here's
;;; an example of how manipulating shapes as arrays can be useful. The
;;; example also tests that higher level libraries are indeed easy to
;;; write on top of this SRFI.

;;; (array-append arr1 arr2 dim)
;;; appends two arrays along a specified dimension. The arrays must
;;; have equally many dimensions and all other dimensions equally long.
;;;
;;; Generalize to more arrays and maybe rewrite with shape-for-each or
;;; what have you.

; (define (array-append dim arr::%array-base . ars)
;   (let* ((total (do ((m (array-length arr dim)
;                         (+ m (array-length (car r) dim)))
;                      (r ars (cdr r)))
;                   ((null? r) m)))
;          (common (array-shape arr))
;          (origin (array->vector (share-column common 0)))
;          (index (make-vector (array-rank arr))))
;     (array-set! common dim 1 (+ (array-start arr dim) total))
;     (let ((result (make-array common)))
;       (array-set! common dim 1 (array-start arr dim))
;       (let wok ((arr arr)
;                 (ars ars))
;         (vector-set! origin dim (array-ref common dim 1))
;         (let ((arr1 (share-array/origin arr origin)))
;           (array-set! common dim 0 (array-start arr1 dim))
;           (array-set! common dim 1 (array-end arr1 dim))
;           (shape-for-each
;            common
;            (lambda (index)
;              (array-set! result index (array-ref arr1 index)))
;            index))
;         (if (pair? ars)
;             (wok (car ars) (cdr ars))))
;       result)))

;;; Transpose, as permutation of dimensions, is applicable to all
;;; arrays. The default is reversal.

;;; The implementation uses multiplication by permutation
;;; matrix but matrix multiplication is not exported.

(define (array:arlib:matrix-times a::%array-base b::%array-base)
  (or (and (= (array-rank a) 2)
           (= (array-rank b) 2))
      (error "matrix-times" "arrays are not matrices" (cons a b)))
  (let ((r0 (array-start a 0))  (rn (array-end a 0))
        (t0 (array-start a 1))  (tn (array-end a 1))
        (u0 (array-start b 0))  (un (array-end b 0)) 
        (k0 (array-start b 1))  (kn (array-end b 1)))
    (or (= (- tn t0) (- un u0))
        (error "matrix-times" "matrices are not compatible" (cons a b)))
    (let ((ab (make-array (shape r0 rn k0 kn))))
      (do ((r r0 (+ r 1)))
        ((= r rn))
        (do ((k k0 (+ k 1)))
          ((= k kn))
          (do ((t t0 (+ t 1))
               (u u0 (+ u 1))
               (s 0 (+ s (* (array-ref a r t)
                            (array-ref b u k)))))
            ((and (= t tn)
                  (= u un))
             (array-set! ab r k s)))))
      ab)))

; This is a generalized transpose. It can permute the dimensions any which 
; way. The permutation is provided by a permutation matrix: a square matrix
; of zeros and ones, with exactly one one in each row and column, or a
; permutation of the rows of an identity matrix; the size of the matrix
; must match the number of dimensions of the array.
;
; The default permutation is [ 0 1 | 1 0 ] of course, but any permutation
; array can be specified, and the shape array of the original array is then
; multiplied with it, and index column vectors of the new array with its
; inverse, from left, to permute the rows appropriately.

(define (array:arlib:permutation-matrix . ds)
  (let* ((n (length ds))
         (arr (make-array (shape 0 n 0 n) 0)))
    (do ((k 0 (+ k 1))
         (ds ds (cdr ds)))
      ((= k n))
      (array-set! arr k (car ds) 1))
    arr))

;;; (transpose arr k ...)
;;; shares arr with permuted dimensions. Each dimension from 0
;;; inclusive to rank exclusive must appear once in k ...

(define (transpose a::%array . p0)
  (let* ((r (array-rank a))
         (permutation (apply array:arlib:permutation-matrix
                             (if (pair? p0)
                                 p0
                                 (do ((ds '() (cons d ds))
                                      (d 0 (+ d 1)))
                                   ((= d r)
                                    ;; reverse dimensions
                                    ds)))))
         (inverse-permutation (share-array permutation
                                           (array-shape permutation)
                                           (lambda (r k)
                                             ;; transpose
                                             (values k r)))))
    (share-array
     a
     (array:arlib:matrix-times permutation (array-shape a))
     (lambda ks0
       (apply values
              (array->list
               (array:arlib:matrix-times
                inverse-permutation
                (apply array (shape 0 r 0 1) ks0))))))))

;;; (share-array/index! array subshape proc index)

; (define (share-array/index! array::%array subshape proc index)
;   (array:share/index! array subshape proc index))

; ;;; Take every nth slice along dimension d into a shared array. This
; ;;; preserves the origin.

; (define (share-nths arr d n)
;   (let* ((bounds (array->vector (array-shape arr)))
;          (b (vector-ref bounds (* 2 d)))
;          (e (vector-ref bounds (+ (* 2 d) 1))))
;     (vector-set! bounds (+ (* 2 d) 1) (+ b (quotient (+ n (- e b 1)) n)))
;     (share-array
;      arr
;      (apply shape (vector->list bounds))
;      (lambda ks
;        (apply values
;               (let d/nk ((u 0) (ks ks))
;                 (if (= u d)
;                     (cons (+ b (* n (- (car ks) b))) (cdr ks))
;                     (cons (car ks) (d/nk (+ u 1) (cdr ks))))))))))