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

   (test "make-array works"
      (print (make-array (shape) *))
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
   
   )

(define (main args)
   (let ((tr (instantiate::terminal-test-runner (suite srfi25-tests))))
      (if (test-runner-execute tr #t) 0 -1))
   )