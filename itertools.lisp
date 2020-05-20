;; Utilities
(defmethod it-to-list (iterator)
  (handler-case (cons (next iterator) (it-to-list iterator))
    (stop-iteration () nil)))

;; A DRY Macro for defining iterators
;; (defmacro def-simple-iterator (name &body (backing-slot-specs
;;                                            (cons-name cons-args slot-initforms)
;;                                            next-body))
;;   `(progn (dcl:defclass/std ,name ,backing-slot-specs)
;;           (defun ,cons-name ,cons-args
;;             (make-instance ,name ,@slot-initforms))
;;           ))

;; Count
(dcl:defclass/std iterator-count (iterator)
  ((start curr delta)))

(defun icount (start step)
  (make-instance 'interator-count :start start :curr start :step step))

(defmethod next ((it iterator-count))
  (with-slots (curr delta) it
    (prog1 curr (incf curr delta))))

;; Repeat
(dcl:defclass/std iterator-repeat (iterator)
  ((item max-repeats)
   (curr-repeats :std 0)))

(defun irepeat (item &optional max-repeats)
  (make-instance 'iterator-repeat :item item :max-repeats max-repeats))

(defmethod next ((it iterator-repeat))
  (with-slots (item (curr curr-repeats) (max max-repeats)) it
    (if max
        (if (< curr max)
            (progn (incf curr) item)
            (error 'stop-iteration))
        item)))

;; Cycle
(dcl:defclass/std iterator-cycle (iterator)
  ((base-iter done results tail)))

(defmethod icycle ((it iterator))
  (make-instance 'iterator-cycle :base-iter it))

(defmethod next ((it iterator-cycle))
  (with-slots ((base base-iter) done (results results) tail) it
    (if done
        (prog1 (car tail) (setf tail (or (cdr tail) results)))
        (handler-case
            (progn (push (next base) results) (car results))
          (stop-iteration ()
            (unless results (error 'stop-iteration))
            (setf base nil
                  done t
                  results (nreverse results)
                  tail results)
            (next it))))))

;; Accumulate
;; TODO This is missing the functionality for special handling of function arguments
;; also, it would be good to fold this library into generic-cl/itertools somehow

(dcl:defclass/std iterator-accumulate (iterator)
  ((base-iter curr-left func)))

(defmethod accumulate ((iterator iterator) fn &optional init)
  (make-instance 'iterator-accumulate :base-iter iterator :func fn :curr-left init))

(defmethod next ((it iterator-accumulate))
  (with-slots ((it base-iter) (curr curr-left) func) it
    (setf curr (funcall func curr (next it)))))

;; Chains

(dcl:defclass/std iterator-chain (iterator)
  ((remaining-iterators curr-it)))

(defmethod ichain-from-iter ((it-of-its iterator))
  (make-instance 'iterator-chain :remaining-iterators it-of-its))

(defun ichain (&rest args)
  (ichain-from-iter (make-iterator args)))

(defmethod next ((it iterator-chain))
  (with-slots (curr-it (rem remaining-iterators)) it
    (unless curr-it (setf curr-it (next rem)))
    (handler-case (next curr-it)
      (stop-iteration () (setf curr-it (next rem)) (next it)))))

;; Compress

(dcl:defclass/std iterator-compress (iterator)
  ((base-iter bool-iter)))

(defmethod compress ((base-iter iterator) (bool-iter iterator))
  (make-instance 'iterator-compress :base-iter base-iter :bool-iter bool-iter))

(defmethod next ((it iterator-compress))
  (with-slots (base-iter bool-iter) it
    (let ((curr-item (next base-iter)))
      (if (next bool-iter)
          curr-item
          (next it)))))
