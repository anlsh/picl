(in-package :picl-tests)

(f:def-suite picl-test-suite :description "Tests for Python Iterators in CL")
(f:in-suite picl-test-suite)

(defun iter-makes (ls iterlike)
  (equalp ls (iter-to-list (make-iterator iterlike))))

(f:def-test test/list-iterator ()
  (let ((ls1 '(1 2 3 4)))
    (f:is (iter-makes ls1 ls1))
    (f:is (iter-makes nil nil))))

(f:def-test test/empty-iterator ()
  (f:is (iter-makes nil nil)))

(f:def-test test/range ()
  ;; Empty range checks
  (f:is (iter-makes nil (range 0 -2)))
  (f:is (iter-makes nil (range -2 0 :delta -1)))
  (f:is (iter-makes nil (range -2 0 :delta -1)))
  ;; Going upwards
  (f:is (iter-makes '(-2 -1) (range -2 0)))
  (f:is (iter-makes '(0 1 2 3 4) (range 0 5)))
  (f:is (iter-makes '(1 2 3 4) (range 1 5)))
  (f:is (iter-makes '(0 2 4 6 8) (range 0 10 :delta 2)))
  (f:is (iter-makes '(0 2 4 6 8) (range 0 9 :delta 2)))

  (f:is (iter-makes '(0 -1 -2 -3 -4) (range 0 -5 :delta -1)))
  (f:is (iter-makes '(0 -2 -4) (range 0 -5 :delta -2)))
  (f:is (iter-makes '(0 -2 -4) (range 0 -6 :delta -2))))

(f:def-test test/take-n ()
  (f:is (equalp '(1 2 3) (take-n 12 '(1 2 3))))
  (let ((it (make-iterator '(1 2 3 4 5 6 7 8))))
    (f:is (iter-makes (take-n 8 it) (range 1 9)))
    (f:is (iter-makes nil (take-n 1 it)))))

(f:def-test test/repeat ()
  (let ((num-repeats 10)
        (repeat-el 32))
    (f:is (equalp (loop for _ below num-repeats collect repeat-el)
                  (take-n num-repeats (repeat repeat-el))))))

(f:def-test test/count ()
  (f:is (equalp (iter-to-list (range 0 10))
                (take-n 10 (icount 0 1))))
  (f:is (equalp (iter-to-list (range 0 8 :delta 2))
                (take-n 4 (icount 0 2)))))

(f:def-test test/cycle ()
  (f:is (equalp '(1 2 3 4 1 2 3 4)
                (take-n 8 (cycle '(1 2 3 4))))))
