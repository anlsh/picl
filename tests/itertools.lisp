(in-package :picl/tests)
(f:in-suite picl-test-suite)

(f:def-test test/iterate-driver ()
  (f:is (equalp (iterate:iter
                  (iterate:for i in-it (picl:permutations '(1 2 3)))
                  (iterate:collect i))
                (list #(1 2 3) #(1 3 2) #(2 1 3) #(2 3 1) #(3 1 2) #(3 2 1)))))

(f:def-test test/list-iterator ()
  (let ((ls1 '(1 2 3 4)))
    (f:is (iter-makes ls1 ls1))
    (f:is (iter-makes nil nil))))

(f:def-test test/vector-iterator ()
  (f:is (iter-makes #(1 2 3 4 5)
                    '(1 2 3 4 5)))
  (f:is (iter-makes #() nil)))

(f:def-test test/empty-iterator ()
  (f:is (iter-makes nil nil)))

(f:def-test test/enumerate ()
  (f:is (iter-makes (enumerate '(:a :b :c))
                    '((0 :a) (1 :b) (2 :c)))))

(f:def-test test/range ()
  ;; Empty range checks
  (f:is (iter-makes nil (range 0 -2)))
  (f:is (iter-makes nil (range -2 0 -1)))
  (f:is (iter-makes nil (range -2 0 -1)))
  ;; Going upwards
  (f:is (iter-makes '(-2 -1) (range -2 0)))
  (f:is (iter-makes '(0 1 2 3 4) (range 0 5)))
  (f:is (iter-makes '(1 2 3 4) (range 1 5)))
  (f:is (iter-makes '(0 2 4 6 8) (range 0 10 2)))
  (f:is (iter-makes '(0 2 4 6 8) (range 0 9 2)))

  (f:is (iter-makes '(0 -1 -2 -3 -4) (range 0 -5 -1)))
  (f:is (iter-makes '(0 -2 -4) (range 0 -5 -2)))
  (f:is (iter-makes '(0 -2 -4) (range 0 -6 -2))))

(f:def-test test/take ()
  (f:is (equalp '(1 2 3) (take 12 '(1 2 3))))
  (let ((it (make-iterator '(1 2 3 4 5 6 7 8))))
    (f:is (iter-makes (take 8 it) (range 1 9)))
    (f:is (iter-makes nil (take 1 it)))))

(f:def-test test/repeat ()
  (let ((num-repeats 10)
        (repeat-el 32))
    (f:is (equalp (loop for _ below num-repeats collect repeat-el)
                  (take num-repeats (repeat repeat-el))))))

(f:def-test test/count ()
  (f:is (equalp (iter-to-list (range 0 10))
                (take 10 (icount 0 1))))
  (f:is (equalp (iter-to-list (range 0 8 2))
                (take 4 (icount 0 2)))))

(f:def-test test/cycle ()
  (f:is (equalp '(1 2 3 4 1 2 3 4)
                (take 8 (cycle '(1 2 3 4))))))

(f:def-test test/islice ()
  (f:is (iter-makes (range 10 20 2)
                    (islice (icount) 10 20 2)))
  (f:is (iter-makes (range 113 257 7)
                    (islice (icount) 113 257 7))))

(f:def-test test/imap ()
  (f:is (iter-makes (range 0 18 6)
                    (imap #'+ (icount) (range 0 6 2) (range 0 9 3)))))

(f:def-test test/compress ()
  (f:is (iter-makes '(1 2 3 4 5 6)
                    (compress '(1 2 3 4 5 6) (repeat t))))
  (f:is (iter-makes '(1 3 5)
                    (compress '(1 2 3 4 5) (cycle '(t nil)))))
  (f:is (iter-makes '(1 3 5)
                    (compress '(1 2 3 4 5 6) (cycle '(t nil))))))

(f:def-test test/chain ()
  (f:is (iter-makes '() (chain nil nil nil)))
  (f:is (iter-makes '() (chain nil)))
  (f:is (iter-makes '(1 2 3 4 5 6) (chain '(1 2 3) '(4 5 6))))
  (f:is (iter-makes '(1 2 3 4 5 6) (chain '() '(1 2 3) '() '(4 5 6) '()))))

(f:def-test test/zip ()
  (f:is (iter-makes (list #(1 2 nil))
                    (zip '(1) '(2 3 nil) '(nil 4))))
  (f:is (iter-makes nil (zip '() '(1 2 3 4))))
  (f:is (iter-makes (list #(1 3) #(2 4))
                    (zip '(1 2) '(3 4)))))

(f:def-test test/zip-longest ()
  (f:is (iter-makes (list #(1 2 nil) #(nil 3 4) #(nil nil nil))
                    (zip-longest nil '(1) '(2 3 nil) '(nil 4))))
  (f:is (iter-makes nil (zip-longest nil '() '())))
  (f:is (iter-makes (list #(1 :a) #(2 :a) #(3 :a))
                    (zip-longest :a '(1 2 3) nil)))
  (f:is (iter-makes (list #(1) #(2))
                    (zip-longest :a '(1 2)))))

(f:def-test test/dropwhile ()
  (f:is (iter-makes '(3 4 5 6)
                    (dropwhile (lambda (x) (< x 3))
                               '(1 2 3 4 5 6))))
  (f:is (iter-makes '(1 2 3 4 5 6)
                    (dropwhile (lambda (x) (declare (ignore x)) nil)
                               '(1 2 3 4 5 6))))
  (f:is (iter-makes '() (dropwhile (lambda (x) (declare (ignore x)) t)
                                   '(1 2 3 4 5 6)))))

(f:def-test test/takewhile ()
  (f:is (iter-makes '(1 2)
                    (takewhile (lambda (x) (< x 3))
                               '(1 2 3 4 5 6))))
  (f:is (iter-makes '(0 1 2 3 4 5 6)
                    (takewhile (lambda (x) (<= x 6))
                               (icount 0 1))))
  (f:is (iter-makes '() (takewhile (lambda (x) (declare (ignore x)) nil)
                                   (icount 0 1)))))

(f:def-test test/filter ()
  ;; Also serves to test filterfalse
  (f:is (iter-makes '(1 2 3 4 5 6)
                    (filter (lambda (x) (declare (ignore x)) t)
                            '(1 2 3 4 5 6))))
  (f:is (iter-makes '()
                    (filter (lambda (x) (declare (ignore x)) nil)
                            '(1 2 3 4 5 6))))
  (f:is (iter-makes '(1 3 5)
                    (filter (lambda (x) (oddp x)) '(-2 0 1 2 3 4 5 6 8))))
  (f:is (iter-makes '(-2 0 2 4 6 8)
                    (filterfalse (lambda (x) (oddp x)) '(-2 0 1 2 3 4 5 6 8)))))

(f:def-test test/starmap ()
  (f:is (iter-makes '() (starmap #'+ nil)))
  (f:is (iter-makes '(4 5 10 2)
                    (starmap #'+ '((1 2 1) (5) (1 2 3 4) (0 0 0 0 0 2 ))))))

(f:def-test test/tee ()
  (let* ((tees (tee (make-iterator '(1 2 3 4 5 6 7 8 9 10))))
         (t0 (aref tees 0))
         (t1 (aref tees 1)))
    (f:is (equalp '(1 2 3)
                  (take 3 t0)))
    (f:is (equalp '(1 2)
                  (take 2 t1)))
    (f:is (equalp '(3 4 5 6)
                  (take 4 t1)))
    (f:is (equalp '(4 5 6 7 8 9 10)
                  (take 7 t0)))
    (f:is (iter-makes nil (iter-to-list t0)))
    (f:is (iter-makes '(7 8 9 10)
                      (iter-to-list t1)))))
