(in-package :picl)

(defclass iterator () ())

(defgeneric make-iterator (obj))
(defmethod make-iterator ((obj function))
  obj)

(defun next (iterator)
  (funcall iterator))

(define-condition stop-iteration (simple-error) ())
