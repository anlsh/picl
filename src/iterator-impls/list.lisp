(in-package :picl)

(defmethod make-iterator ((obj list))
  "Returns an iterator which traverses elements of a list in sequential order"
  (lambda ()
    (if obj
        (values (prog1 (car obj) (setf obj (cdr obj))) t)
        (values nil nil))))
