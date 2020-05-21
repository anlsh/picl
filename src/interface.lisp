(in-package :picl)

(defclass iterator () ())

(defgeneric make-iterator (obj))
(defmethod make-iterator ((obj iterator))
  obj)

(defgeneric next (iterable))

(define-condition stop-iteration (simple-error) ())
