(module testsrfi25
   (library srfi25 btest)
   (main main))

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
   )

(define (main args)
   (let ((tr (instantiate::terminal-test-runner (suite srfi25-tests))))
      (if (test-runner-execute tr #t) 0 -1))
   )