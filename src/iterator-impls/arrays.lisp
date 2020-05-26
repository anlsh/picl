(in-package :picl)

(defmethod make-iterator ((vec vector))
  (let ((curr 0)
        (length (length vec)))
    (lambda ()
      (if (< curr length)
          (values (prog1 (aref vec curr) (incf curr)) t)
          (values nil nil)))))
