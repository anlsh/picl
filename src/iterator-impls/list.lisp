(in-package :picl)

(defmethod make-iterator ((obj list))
  (lambda ()
    (if (null obj)
        (error 'stop-iteration)
        (prog1 (car obj) (setf obj (cdr obj))))))
