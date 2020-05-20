(dcl:defclass/std iterator-list (iterator)
  ((state)))

(defmethod make-iterator ((obj list))
  (make-instance 'iterator-list :state obj))

(defmethod next ((it iterator-list))
  (with-slots ((state state)) it
    (if (null state)
        (error 'stop-iteration)
        (prog1 (car state) (setf state (cdr state))))))
