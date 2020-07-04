(in-package :picl)

(defmethod make-iterator ((obj vector))
  "Returns an iterator which traverses elements of a vector in sequential order"
  (let ((curr 0)
        (length (length obj)))
    (lambda ()
      (if (< curr length)
          (values (prog1 (aref obj curr) (incf curr)) t)
          (values nil nil)))))
