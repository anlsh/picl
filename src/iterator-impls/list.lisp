(in-package :picl)

(defmethod make-iterator ((obj list))
  (lambda ()
    (if obj
        (values (prog1 (car obj) (setf obj (cdr obj))) t)
        (values nil nil))))
