(in-package :picl)

;; Utilities
(defun iter-to-list (iterator)
  (handler-case (cons (next iterator) (it-to-list iterator))
    (stop-iteration () nil)))

(defun empty-iterator ()
  (make-iterator nil))

;; Range
(dcl:defclass/std iterator-range (iterator)
  ((start stop delta curr)))

(defmethod range ((start integer) (stop integer) &key delta)
  (unless delta (setf delta 1))
  (make-instance 'iterator-range :start start :stop stop :delta delta :curr start))

(defmethod next ((it iterator-range))
  (with-slots (curr delta stop) it
    (if (or (and (> delta 0) (< curr stop))
            (and (< delta 0) (> curr stop)))
        (prog1 curr (incf curr delta))
        (error 'stop-iteration))))

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

(defun icycle (it)
  (make-instance 'iterator-cycle :base-iter (make-iterator it)))

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
;; Also this is halfway to being useless, CL has reduce

(dcl:defclass/std iterator-accumulate (iterator)
  ((base-iter curr-left func)))

(defun accumulate (it fn &optional init)
  (make-instance 'iterator-accumulate :base-iter (make-iterator it) :func fn :curr-left init))

(defmethod next ((it iterator-accumulate))
  (with-slots ((it base-iter) (curr curr-left) func) it
    (setf curr (funcall func curr (next it)))))

;; Chains

(dcl:defclass/std iterator-chain (iterator)
  ((itail curr-it)))

(defun ichain-from-iter (it-of-its)
  (make-instance 'iterator-chain :itail (make-iterator it-of-its)))

(defun ichain (&rest args)
  (ichain-from-iter args))

(defmethod next ((it iterator-chain))
  (with-slots (curr-it itail) it
    (unless curr-it (setf curr-it (make-iterator (next itail))))
    (handler-case (next curr-it)
      (stop-iteration () (setf curr-it (make-iterator (next itail))) (next it)))))

;; Compress

(dcl:defclass/std iterator-compress (iterator)
  ((base-iter bool-iter)))

(defun compress (base-iter bool-iter)
  (make-instance 'iterator-compress
                 :base-iter (make-iterator base-iter)
                 :bool-iter (make-iterator bool-iter)))

(defmethod next ((it iterator-compress))
  (with-slots (base-iter bool-iter) it
    (let ((curr-item (next base-iter)))
      (if (next bool-iter)
          curr-item
          (next it)))))

;; Dropwhile

(dcl:defclass/std iterator-dropwhile (iterator)
  ((base-iter pred been-false)))

(defun dropwhile (predicate iterator)
  (make-instance 'iterator-dropwhile
                 :base-iter (make-iterator iterator)
                 :pred predicate))

(defmethod next ((it iterator-dropwhile))
  (with-slots (base-iter pred been-false) it
    (if been-false
        (next base-iter)
        (let ((item (next base-iter)))
          (if (funcall pred item)
              (next it)
              (progn (setf been-false t)
                     item))))))

;; Filterfalse

(dcl:defclass/std iterator-filterfalse (iterator)
  ((base-iter pred)))

(defun filterfalse (predicate iterator)
  (make-instance 'iterator-filterfalse
                 :base-iter (make-iterator iterator)
                 :pred predicate))

(defmethod next ((it iterator-filterfalse))
  (with-slots (base-iter pred) it
    (let ((item (next base-iter)))
      (if (not (funcall pred item))
              (next it)
              item))))

;; TODO groupby

;; TODO islice

;; takewhile

(dcl:defclass/std iterator-takewhile (iterator)
  ((base-iter pred)))

(defun takewhile (predicate iterator)
  (make-instance 'iterator-takewhile
                 :base-iter (make-iterator iterator)
                 :pred predicate))

(defmethod next ((it iterator-takewhile))
  (with-slots (base-iter pred been-false) it
    (let ((item (next base-iter)))
      (if (funcall pred item)
          item
          (error 'stop-iteration)))))

;; zip-longest
