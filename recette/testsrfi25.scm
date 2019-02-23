(module testsrfi25
   (library srfi25 btest)
   (main main))


(define i_4 (let* ((i (make-array
                         (shape 0 4 0 4)
                         0))
                   (d (share-array i
                         (shape 0 4)
                         (lambda (k)
                            (values k k)))))
               (do   ((k 0 (+ k 1))) ((= k 4))
                     (array-set! d k 1))
               i))

(define threed123
  (array (shape 0 1 0 2 0 3)
         'a 'b 'c
         'd 'e 'f))

(define threed312
  (array (shape 0 3 0 1 0 2)
         'a 'd
         'b 'e
         'c 'f))

(define rot231 (list 1 2 0))



;;; The frivolous board game exercises share of share of share.

;;; A three dimensional chess board with two phases: piece and colour
;;; of piece. Think of pieces in a cube with height, width and depth,
;;; and piece colours in a parallel cube. We put pink jays around and
;;; grey crows inside the board proper. Later we put in a blue rook.

(define board
  (tabulate-array
   (shape -1 9 -1 9 -1 9 0 2)
   (lambda (t u v w)
     (case w
       ((0) (if (and (< -1 u 8)
                     (< -1 v 8)
                     (< -1 t 8))
                'crow
                'jay))
       ((1) (if (and (< -1 u 8)
                     (< -1 v 8)
                     (< -1 t 8))
                'grey
                'pink))))))

;;; A cylinder with height 4, width 4, depth 6, both phases, centered
;;; inside the board. Top left front corner is at 0 0 0 of cylinder but
;;; 2 2 1 of board.

(define board-cylinder
  (share-array
   board
   (shape 0 4 0 4 0 6 0 2)
   (lambda (t u v w)
     (values (+ t 2) (+ u 2) (+ v 1) w))))


;;; The center cube with side 2 of the cylinder, hence of the board,
;;; with both phases. Top left corner is 0 0 0 of center but 1 1 2
;;; of cylinder and 3 3 3 of board.

(define board-center
  (share-array
   board-cylinder
   (shape 0 2 0 2 0 2 0 2)
   (lambda (t u v w)
     (values (+ t 1) (+ u 1) (+ v 2) w))))


;;; Front face of center cube, in two dimensions plus phase. Top left
;;; corner is 0 0 of face but 0 0 0 of center and 1 1 2 of cylinder
;;; 3 3 3 of board.

(define board-face
  (share-array
   board-center
   (shape 0 2 0 2 0 2)
   (lambda (t u w)
     (values t u 0 w))))


;;; Left side of face in three dimensions plus phase. Top is 0 0 0 of
;;; pillar but 0 0 of face and 0 0 0 of center and 1 1 2 of cylinder
;;; and 3 3 3 of board. Bottom is 1 0 0 of pillar but 1 0 of face and
;;; 1 0 0 of center and 2 1 2 of cylinder and 4 3 3 of board.

(define board-pillar
  (share-array
   board-face
   (shape 0 2 0 1 0 1 0 2)
   (lambda (t u v w)
     (values t 0 w))))


;;; Pillar upside down. Now top 0 0 0 is 1 0 of face and 1 0 0 of center
;;; and 2 1 2 of cylinder and 4 3 3 of board.

(define board-reverse-pillar
  (share-array
   board-pillar
   (shape 0 2 0 1 0 1 0 2)
   (lambda (t u v w)
     (values (- 1 t) u v w))))


;;; Bottom of pillar.

(define board-cubicle
  (share-array
   board-pillar
   (shape 0 2)
   (lambda (w)
     (values 1 0 0 w))))


;;; Top of upside down pair.

(define board-reverse-cubicle
  (share-array
   board-reverse-pillar
   (shape 0 2)
   (lambda (w)
     (values 0 0 0 w))))


;;; Piece phase of cubicle.

(define board-piece
  (share-array
   board-cubicle
   (shape)
   (lambda ()
     (values 0))))


;;; Colour phase of the other cubicle that is actually the same cubicle.

(define board-colour
  (share-array
   board-reverse-cubicle
   (shape)
   (lambda ()
     (values 1))))


;;; Put a blue rook at the bottom of the pillar and at the top of the
;;; upside pillar.

(array-set! board-piece 'rook)
(array-set! board-colour 'blue)


;;; Build the same chess position directly.

(define board-two
  (tabulate-array
   (shape -1 9 -1 9 -1 9 0 2)
   (lambda (t u v w)
     (if (and (= t 4) (= u 3) (= v 3))
         (case w
           ((0) 'rook)
           ((1) 'blue))
         (case w
           ((0) (if (and (< -1 u 8)
                         (< -1 v 8)
                         (< -1 t 8))
                    'crow
                    'jay))
           ((1) (if (and (< -1 u 8)
                         (< -1 v 8)
                         (< -1 t 8))
                    'grey
                    'pink)))))))

;;; Permute the dimensions of the chess board in two different ways.
;;; The transpose also exercises matrix multiplication.

(define board-three
  (share-array
   board-two
   (shape 0 2 -1 9 -1 9 -1 9)
   (lambda (w t u v)
      (values t u v w))))


;; Just see that empty share does not crash. No index is valid. Just by
;;; the way. There is nothing to be done with it.

(define board-nothing
  (share-array
   board
   (shape 0 3 1 1 0 3)
   (lambda (t u v)
     (values 0 0 0))))

;;; Multiplication table

(define four-by-four
  (array (shape 0 4 0 4)
         0 0 0 0
         0 1 2 3
         0 2 4 6
         0 3 6 9))

(define-test-suite srfi25-tests
   
   (test "shape works"
      (assert-true (shape? (shape)))
      (assert-true (shape? (shape -1 -1)))
      (assert-true (shape? (shape -1 0)))
      (assert-true (shape? (shape -1 1)))
      (assert-true (shape?
                      (shape 1 2 3 4 5 6 7 8 1 2 3 4 5 6 7 8 1 2 3 4 5 6 7 8))))

   (test "shape equality works"
      (assert-true (equal? (shape 0 2) (shape 0 2)))
      (assert-false (equal? (shape 1 2 3 4) (shape 1 2 3 5)))
      (assert-true (equal? (shape 1 2 3 4) (shape 1 2 3 4))))

   (test "make-array works"
      (assert-true (array? (make-array (shape) *)))
      (assert-true (array? (make-array (shape -1 -1))))
      (assert-true (array? (make-array (shape -1 -1) *)))
      (assert-true (array? (make-array (shape -1 1))))
      (assert-true (array?
                      (make-array (shape 1 2 3 4 5 6 7 8 1 2 3 4 5 6 7 8 1 2 3 4) *))))

   (test "array works"
      (assert-true (array? (array (shape) *)))
      (assert-true (array? (array (shape -1 -1))))
      (assert-true (array? (array (shape -1 1) * *)))
      (assert-true (array? (array (shape 1 2 3 4 5 6 7 8 1 2 3 4 5 6 7 8) *))))

   (test "array equality works"
      (assert-true (equal? (array (shape 0 2 0 2) 1 2 3 4)
                      (array (shape 0 2 0 2) 1 2 3 4)))
      (assert-false (equal? (array (shape 0 2 0 2) 1 2 3 4)
                       (array (shape 0 2 0 2) 1 2 3 5)))
      (assert-false (equal? (array (shape 0 2 0 2) 1 2 3 4)
                       (array (shape 1 3 1 3) 1 2 3 4))))

   (test "shape-rank works"
      (assert= (shape-rank (shape)) 0)
      (assert= (shape-rank (shape -1 -1)) 1)
      (assert= (shape-rank (shape -1 1)) 1)
      (assert= (shape-rank (shape 1 2 3 4 5 6 7 8)) 4))

   (test "array-rank of make-array works"
      (assert= (array-rank (make-array (shape))) 0)
      (assert= (array-rank (make-array (shape -1 -1))) 1)
      (assert= (array-rank (make-array (shape -1 1))) 1)
      (assert= (array-rank (make-array (shape 1 2 3 4 5 6 7 8))) 4))

   (test "array-rank of array works"
      (assert= (array-rank (array (shape) *)) 0)
      (assert= (array-rank (array (shape -1 -1))) 1)
      (assert= (array-rank (array (shape -1 1) * *)) 1)
      (assert= (array-rank (array (shape 1 2 3 4 5 6 7 8) *)) 4))

   (test "shape-start works"
      (assert= (shape-start (shape -1 -1) 0) -1)
      (assert-exception-thrown (shape-start (shape -1 -1) 1) &index-out-of-bounds-error)
      (assert= (shape-start (shape -1 1) 0) -1)
      (assert-exception-thrown (shape-start (shape -1 1) 1) &index-out-of-bounds-error)
      (assert= (shape-start (shape 1 2 3 4 5 6 7 8) 0) 1)
      (assert= (shape-start (shape 1 2 3 4 5 6 7 8) 1) 3))

   (test "shape-end works"
      (assert= (shape-end (shape -1 -1) 0) -1)
      (assert-exception-thrown (shape-end (shape -1 -1) 1) &index-out-of-bounds-error)
      (assert= (shape-end (shape -1 1) 0) 1)
      (assert-exception-thrown (shape-end (shape -1 1) 1) &index-out-of-bounds-error)
      (assert= (shape-end (shape 1 2 3 4 5 6 7 8) 3) 8)
      (assert= (shape-end (shape 1 2 3 4 5 6 7 8) 1) 4))

   (test "array-start of make-array works"
      (assert= (array-start (make-array (shape -1 -1)) 0) -1)
      (assert= (array-start (make-array (shape -1 1)) 0) -1)
      (assert= (array-start (make-array (shape 1 2 3 4 5 6 7 8)) 0) 1)
      (assert= (array-start (make-array (shape 1 2 3 4 5 6 7 8)) 1) 3)
      (assert= (array-start (make-array (shape 1 2 3 4 5 6 7 8)) 2) 5)
      (assert= (array-start (make-array (shape 1 2 3 4 5 6 7 8)) 3) 7))

   (test "array-end of make-array works"
      (assert= (array-end (make-array (shape -1 -1)) 0) -1)
      (assert= (array-end (make-array (shape -1 1)) 0) 1)
      (assert= (array-end (make-array (shape 1 2 3 4 5 6 7 8)) 0) 2)
      (assert= (array-end (make-array (shape 1 2 3 4 5 6 7 8)) 1) 4)
      (assert= (array-end (make-array (shape 1 2 3 4 5 6 7 8)) 2) 6)
      (assert= (array-end (make-array (shape 1 2 3 4 5 6 7 8)) 3) 8))

   (test "array-start of array works"
      (assert= (array-start (array (shape -1 -1)) 0) -1)
      (assert= (array-start (array (shape -1 1) * *) 0) -1)
      (assert= (array-start (array (shape 1 2 3 4 5 6 7 8) *) 0) 1)
      (assert= (array-start (array (shape 1 2 3 4 5 6 7 8) *) 1) 3)
      (assert= (array-start (array (shape 1 2 3 4 5 6 7 8) *) 2) 5)
      (assert= (array-start (array (shape 1 2 3 4 5 6 7 8) *) 3) 7))
   
   (test "array-end of array works"
      (assert= (array-end (array (shape -1 -1)) 0) -1)
      (assert= (array-end (array (shape -1 1) * *) 0) 1)
      (assert= (array-end (array (shape 1 2 3 4 5 6 7 8) *) 0) 2)
      (assert= (array-end (array (shape 1 2 3 4 5 6 7 8) *) 1) 4)
      (assert= (array-end (array (shape 1 2 3 4 5 6 7 8) *) 2) 6)
      (assert= (array-end (array (shape 1 2 3 4 5 6 7 8) *) 3) 8))

   (test "array-ref of make-array works"
      (assert-eq? (array-ref (make-array (shape) 'a)) 'a)
      (assert-eq? (array-ref (make-array (shape -1 1) 'b) -1) 'b)
      (assert-eq? (array-ref (make-array (shape -1 1) 'c) 0) 'c)
      (assert-eq? (array-ref (make-array (shape 1 2 3 4 5 6 7 8) 'd) 1 3 5 7) 'd))

   (test "array-ref of make-array using vector indicies works"
      (assert-eq? (array-ref (make-array (shape) 'a) '#()) 'a)
      (assert-eq? (array-ref (make-array (shape -1 1) 'b) '#(-1)) 'b)
      (assert-eq? (array-ref (make-array (shape -1 1) 'c) '#(0)) 'c)
      (assert-eq? (array-ref (make-array (shape 1 2 3 4 5 6 7 8) 'd)
                     '#(1 3 5 7)) 'd))

    
   (test "array-ref of make-array using array indices works"

      (assert-eq? (array-ref (make-array (shape) 'a)
                     (array (shape 0 0))) 'a)
      (assert-eq? (array-ref (make-array (shape -1 1) 'b)
                     (array (shape 0 1) -1))
         'b)
      (assert-eq? (array-ref (make-array (shape -1 1) 'c)
                     (array (shape 0 1) 0))
         'c)
      (assert-eq? (array-ref (make-array (shape 1 2 3 4 5 6 7 8) 'd)
                     (array (shape 0 4) 1 3 5 7))
         'd))

   (test "array-set! of make-array with arguments works"
      (let ((arr (make-array (shape) 'o)))
         (array-set! arr 'a)
         (assert-eq? (array-ref arr) 'a))
      (let ((arr (make-array (shape -1 1) 'o)))
         (array-set! arr -1 'b)
         (array-set! arr 0 'c)
         (assert-eq? (array-ref arr -1) 'b)
         (assert-eq? (array-ref arr 0) 'c))
      (let ((arr (make-array (shape 1 2 3 4 5 6 7 8) 'o)))
         (array-set! arr 1 3 5 7 'd)
         (assert-eq? (array-ref arr 1 3 5 7) 'd)))

   (test "array-set! of make-array with vector works"
      (let ((arr (make-array (shape) 'o)))
         (array-set! arr '#() 'a)
         (assert-eq? (array-ref arr '#()) 'a))
      (let ((arr (make-array (shape -1 1) 'o)))
         (array-set! arr '#(-1) 'b)
         (array-set! arr '#(0) 'c)
         (assert-eq? (array-ref arr -1) 'b)
         (assert-eq? (array-ref arr 0) 'c))
      (let ((arr (make-array (shape 1 2 3 4 5 6 7 8) 'o)))
         (array-set! arr '#(1 3 5 7) 'd)
         (assert-eq? (array-ref arr 1 3 5 7) 'd)))

   (test "array-set! of make-array with array works"
      (let ((arr (make-array (shape) 'o)))
         (array-set! arr 'a)
         (assert-eq? (array-ref arr) 'a))
      (let ((arr (make-array (shape -1 1) 'o)))
         (array-set! arr (array (shape 0 1) -1) 'b)
         (array-set! arr (array (shape 0 1) 0) 'c)
         (assert-eq? (array-ref arr -1) 'b)
         (assert-eq? (array-ref arr 0) 'c))
      (let ((arr (make-array (shape 1 2 3 4 5 6 7 8) 'o)))
         (array-set! arr (array (shape 0 4) 1 3 5 7) 'd)
         (assert-eq? (array-ref arr 1 3 5 7) 'd))
      )
   
   (test "array sharing with changes works"
      (let* ((org (array (shape 6 9 0 2) 'a 'b 'c 'd 'e 'f))
             (brk (share-array
                     org
                     (shape 2 4 1 3)
                     (lambda (r k)
                        (values
                           (+ 6 (* 2 (- r 2)))
                           (- k 1)))))
             (swp (share-array
                     org
                     (shape 3 5 5 7)
                     (lambda (r k)
                        (values
                           (+ 7 (- r 3))
                           (- 1 (- k 5))))))
             (box (share-array
                     swp
                     (shape 0 1 2 3 4 5 6 7 8 9)
                     (lambda _ (values 4 6))))
             (org-contents (lambda ()
                              (list (array-ref org 6 0) (array-ref org 6 1)
                                 (array-ref org 7 0) (array-ref org 7 1)
                                 (array-ref org 8 0) (array-ref org 8 1))))
             (brk-contents (lambda ()
                              (list (array-ref brk 2 1) (array-ref brk 2 2)
                                 (array-ref brk 3 1) (array-ref brk 3 2))))
             (swp-contents (lambda ()
                              (list (array-ref swp 3 5) (array-ref swp 3 6)
                                 (array-ref swp 4 5) (array-ref swp 4 6))))
             (box-contents (lambda ()
                              (list (array-ref box 0 2 4 6 8)))))
         (assert-equal? (org-contents) '(a b c d e f))
         (assert-equal? (brk-contents) '(a b e f))
         (assert-equal? (swp-contents) '(d c f e))
         (assert-equal? (box-contents) '(e))
         (begin (array-set! org 6 0 'x) #t)
         (assert-equal? (org-contents) '(x b c d e f))
         (assert-equal? (brk-contents) '(x b e f))
         (assert-equal? (swp-contents) '(d c f e))
         (assert-equal? (box-contents) '(e))
         (begin (array-set! brk 3 1 'y) #t)
         (assert-equal? (org-contents) '(x b c d y f))
         (assert-equal? (brk-contents) '(x b y f))
         (assert-equal? (swp-contents) '(d c f y))
         (assert-equal? (box-contents) '(y))
         (begin (array-set! swp 4 5 'z) #t)
         (assert-equal? (org-contents) '(x b c d y z))
         (assert-equal? (brk-contents) '(x b y z))
         (assert-equal? (swp-contents) '(d c z y))
         (assert-equal? (box-contents) '(y))
         (begin (array-set! box 0 2 4 6 8 'e) #t)
         (assert-equal? (org-contents) '(x b c d e z))
         (assert-equal? (brk-contents) '(x b e z))
         (assert-equal? (swp-contents) '(d c z e))
         (assert-equal? (box-contents) '(e))))

   (test "shape passed to make-array and array are copied not referenced"
      (let ((shp::%shape (shape 10 12)))
         (let ((arr (make-array shp))
               (ars (array shp * *))
               (art (share-array (make-array shp) shp (lambda (k) k))))
            (set-car! (vector-ref (-> shp vec ) 0) '?)
            (set-cdr! (vector-ref (-> shp vec)  0) '!)
            (assert= (shape-rank shp) 1)
            (assert-eq? (shape-start shp 0) '?)
            (assert-eq? (shape-end shp 0) '!)
            (assert= (array-rank arr) 1)
            (assert= (array-start arr 0) 10)
            (assert= (array-end arr 0) 12)
            (assert= (array-rank ars) 1)
            (assert= (array-start ars 0) 10)
            (assert= (array-end ars 0) 12)
            (assert= (array-rank art) 1)
            (assert= (array-start art 0) 10)
            (assert= (array-end art 0) 12))))



   (test "array access with sharing index array"
      (let ((arr (array (shape 4 6 5 7) 'nw 'ne 'sw 'se))
            (ixn (array (shape 0 2 0 2) 4 6 5 4)))
         (let ((col0 (share-array
                        ixn
                        (shape 0 2)
                        (lambda (k)
                           (values k 0))))
               (row0 (share-array
                        ixn
                        (shape 0 2)
                        (lambda (k)
                           (values 0 k))))
               (wor1 (share-array
                        ixn
                        (shape 0 2)
                        (lambda (k)
                           (values 1 (- 1 k)))))
               (cod (share-array
                       ixn
                       (shape 0 2)
                       (lambda (k)
                          (case k
                             ((0) (values 1 0))
                             ((1) (values 0 1))))))
               (box (share-array
                       ixn
                       (shape 0 2)
                       (lambda (k)
                          (values 1 0)))))
            (assert-eq? (array-ref arr col0) 'nw)
            (assert-eq? (array-ref arr row0) 'ne)
            (assert-eq? (array-ref arr wor1) 'nw)
            (assert-eq? (array-ref arr cod) 'se)
            (assert-eq? (array-ref arr box) 'sw)
            (array-set! arr col0 'ul)
            (array-set! arr row0 'ur)
            (array-set! arr cod 'lr)
            (array-set! arr box 'll)
            (assert-eq? (array-ref arr 4 5) 'ul)
            (assert-eq? (array-ref arr 4 6) 'ur)
            (assert-eq? (array-ref arr 5 5) 'll)
            (assert-eq? (array-ref arr 5 6) 'lr)
            (array-set! arr wor1 'xx)
            (assert-eq? (array-ref arr 4 5) 'xx))))

;;;; arlib tests

   (test "i_4 vs tabulate-array"
      (assert-true (array-equal? i_4
                      (tabulate-array
                         (shape 0 4 0 4)
                         (lambda (j k)
                            (if (= j k) 1 0))))))
   (test "i_4 vs array"
      (assert-true (array-equal? i_4
                      (array
                         (shape 0 4 0 4)
                         1 0 0 0
                         0 1 0 0 
                         0 0 1 0
                         0 0 0 1))))
   (test "i_4 diagonal"
      (assert-true (array-equal? (share-array
                                    i_4
                                    (shape 0 4)
                                    (lambda (k)
                                       (values k k)))
                      (share-array
                         (array (shape) 1)
                         (shape 0 4)
                         (lambda (k)
                            (values))))))

   (test "i_4 codiagonal"
      (assert-true (array-equal? (share-array
                                    i_4
                                    (shape 0 4)
                                    (lambda (k)
                                       (values (- 3 k) k)))
                      (share-array
                         (array (shape) 0)
                         (shape 0 4)
                         (lambda (k)
                            (values))))))
   (test "i_4 corners and center"
      (assert-true (array-equal? (share-array
                                    i_4
                                    (shape 0 2 0 2)
                                    (lambda (j k)
                                       (values (* 3 j) (* 3 k))))
                      (share-array
                         i_4
                         (shape 0 2 0 2)
                         (lambda (j k)
                            (values (+ j 1) (+ k 1)))))))

   ; (test "i_4 transpose"
   ;    (assert-true (array-equal? i_4 (transpose i_4))))

   ; (test "threed123 transpose"
   ;    (assert-true  (array-equal? threed123
   ;                     (apply transpose threed312 rot231))))

   (test "board vs board-two"
      (assert-true (array-equal? board board-two)))

   ; (test "board-three vs transpose of board-two"
   ;    (assert-true (array-equal? board-three
   ;                (transpose board-two 3 0 1 2))))

   ; (test "board-two versus transpose of board-two"
   ;    (assert-true (array-equal? (share-array
   ;                      board-two
   ;                      (shape -1 9 0 2 -1 9 -1 9)
   ;                      (lambda (t w u v)
   ;                         (values t u v w)))
   ;                    (transpose board-two 0 3 1 2))))

   (test "board-nothing"
      (assert-true (array-equal? board-nothing (array (array-shape board-nothing)))))

   (test "tabulate-array! with vector"
      (assert-true
         (array-equal? (tabulate-array (shape 4 8 2 5 0 1) *)
            (tabulate-array! (shape 4 8 2 5 0 1)
               (lambda (v)
                  (* (vector-ref v 0)
                     (vector-ref v 1)
                     (vector-ref v 2)))
               (vector * * *)))))

   (test "tabulate-array! with array"
      (assert-true (array-equal? (tabulate-array (shape 4 8 2 5 0 1) *)
                      (let ((index (share-array (make-array (shape 0 2 0 3))
                                      (shape 0 3)
                                      (lambda (k) (values 1 k)))))
                         (tabulate-array! (shape 4 8 2 5 0 1)
                            (lambda (a)
                               (* (array-ref a 0)
                                  (array-ref a 1)
                                  (array-ref a 2)))
                            index))))
      )
   (test "array-sum"
      (assert-true (array-equal?
                      (array-map
                         +
                         (share-array (array (shape) 0) (shape 1 2 1 4) (lambda _ (values)))
                         (share-array (array (shape) 1) (shape 1 2 1 4) (lambda _ (values)))
                         (share-array (array (shape) 2) (shape 1 2 1 4) (lambda _ (values))))
                      (array (shape 1 2 1 4) 3 3 3))))

   (test "four-by-four vs tabulate-array"
      (assert-true (array-equal? four-by-four (tabulate-array (shape 0 4 0 4) *))))

   (test "four-by-four vs array-retabulate!"
      (assert-true (array-equal?
                      four-by-four
                      (let ((table (make-array (shape 0 4 0 4) 19101)))
                         (array-retabulate! table (array-shape table) *)
                         table))))

   (test "four-by-four vs array-retabulate! on parts"
      (assert-true (array-equal?
                      four-by-four
                      (let ((table (make-array (shape 0 4 0 4) 19101)))
                         (array-retabulate!
                            table
                            (shape 1 2 1 4)
                            (lambda (v)
                               (* (vector-ref v 0) (vector-ref v 1)))
                            (vector - -))
                         (array-retabulate!
                            table
                            (shape 2 4 0 4)
                            (lambda (a)
                               (* (array-ref a (vector 0)) (array-ref a (vector 1))))
                            (make-array (shape 0 2)))
                         (array-set! table 0 0 0)
                         (array-set! table (vector 0 1) 0)
                         (array-set! table (array (shape 0 2) 0 2) 0)
                         (shape-for-each
                            (shape 0 1 3 4)
                            (lambda (v)
                               (array-set! table v (vector-ref v 0)))
                            (vector - -))
                         (let ((arr (share-array
                                       table
                                       (shape 1 2 0 1 1 2 3 4 5 6 7 8 1 2 3 4 5 6 7 8)
                                       (lambda (r k . _)
                                          (values r k)))))
                            (array-retabulate! arr (array-shape arr) *))
                         table))))
   
   (test "shape-for-each without index object"
      (assert-true (let ((em '()))
                      (shape-for-each
                         (shape 0 2 -2 0 0 1)
                         (lambda (u v w)
                            (set! em (cons (list u v w) em))))
                      (equal? (reverse em) '((0 -2 0) (0 -1 0) (1 -2 0) (1 -1 0))))))


   (test "array-append"
      (assert-true  (array-equal? (array-append
                                     (array (shape 0 2 0 2) 'a 'b 'c 'd)
                                     (array (shape 0 1 0 2) 'e 'f))
                       (array (shape 0 3 0 2) 'a 'b 'c 'd 'e 'f)))
      
      (assert-true (array-equal? (array-append
                                    (array (shape 0 2 0 2) 'a 'b 'c 'd)
                                    (array (shape 0 2 0 1) 'e 'f) 1)
                      (array (shape 0 2 0 3) 'a 'b 'e 'c 'd 'f)))
      (assert-true (array-equal? (array-append
                                     (array (shape 0 2 0 2) 'a 'b 'c 'd)
                                     (array (shape 1 3 0 1) 'e 'f) 1)
                       (array (shape 0 2 0 3) 'a 'b 'e 'c 'd 'f)))

      )
   
   )


(define (main args)
   (let ((tr (instantiate::terminal-test-runner (suite srfi25-tests))))
      (if (test-runner-execute tr #t) 0 -1)))