(in-package :picl)

(defclass iterator () ())
(defclass copyable-iterator (iterator) ())

(defgeneric make-iterator (obj))
(defgeneric next (iterable))

(define-condition stop-iteration (simple-error) ())

(defgeneric it-to-list (it))
