(defpackage picl-tests
  (:use :cl :picl)
  (:local-nicknames (#:f #:fiveam) (#:gcl #:generic-cl) (#:alx #:alexandria))
  (:export #:picl-test-suite))

(in-package :picl-tests)
(f:def-suite picl-test-suite :description "Tests for Python Iterators in CL")

(defun iter-makes (i0 i1)
  (f:is (equalp (iter-to-list (make-iterator i0))
                (iter-to-list (make-iterator i1)))))
