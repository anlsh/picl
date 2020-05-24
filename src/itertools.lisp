(in-package :picl)

;; A macro with which to define iterables: the idea being that I can switch what it
;; produces from raw function representation (which is what it currently implements)
;; to generic dispatch based (what is currently used throughout the code)
;;
;; Not sure how useful it's going to be tbh: it's anaphoric, and plus whenever a state
;; variables has the same name as a constructor argument one (probably the state variable.
;; since it's not user-facing) will have to be mangled
;;
;; I also don't want to make users use this, so the representation will probably have
;; to be nailed down before initial release anyways
(defmacro def-iter (name state-vars (constructor-name constructor-params &body cons-body)
                    &body next-body)
  (declare (ignore name))
  `(defun ,constructor-name ,constructor-params
     (let ,state-vars
       (macrolet ((init-state (&rest args)
                    `(progn ,@(loop for (aname aform) in args
                                    collect `(setf ,aname ,aform)))))
         (progn ,@cons-body
                (lambda () ,@next-body))))))

(def-iter range (+start +stop +delta +curr)
    (make-range (start stop delta)
      (unless delta (setf delta 1))
      (init-state (+start start) (+stop stop) (+delta delta) (+curr start)))
  (if (or (and (> +delta 0) (< +curr +stop))
          (and (< +delta 0) (> +curr +stop)))
        (prog1 +curr (incf +curr +delta))
        (error 'stop-iteration)))

;; Utilities
(defun iter-to-list (iterator)
  (handler-case (cons (next iterator) (iter-to-list iterator))
    (stop-iteration () nil)))

(defun iter-to-vec (iterator)
  (let ((ls (iter-to-list (make-iterator iterator))))
    (make-array (length ls) :initial-contents ls)))

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
  (make-instance 'iterator-count :start start :curr start :delta step))

(defmethod next ((it iterator-count))
  (with-slots (curr delta) it
    (prog1 curr (incf curr delta))))

;; Repeat
(dcl:defclass/std iterator-repeat (iterator)
  ((item max-repeats)
   (curr-repeats :std 0)))

(defun repeat (item &optional max-repeats)
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

(defun cycle (it)
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

(defun chain-from-iter (it-of-its)
  (make-instance 'iterator-chain :itail (make-iterator it-of-its)))

(defun chain (&rest args)
  (chain-from-iter args))

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

;; TODO starmap

;; TODO zip_longest

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

;; take-n (not in "core" itertools but very nice for testing)
(defun take-n (n iterable)
  (let ((iterator (make-iterator iterable)))
    (loop for i below n
          collecting (handler-case (next iterator)
                       (stop-iteration () (return a)))
            into a
          finally (return a))))

;; islice
(dcl:defclass/std iterator-islice (iterator)
  ((base-it start stop delta curr)))

(defun islice (iterlike start stop delta)
  (unless (and (>= start 0) (>= stop 0) (>= delta 0))
    (error (format nil "Args must all be positive~%")))
  (make-instance 'iterator-islice
                 :base-it (make-iterator iterlike)
                 :start start :stop stop :delta delta :curr 0))

(defmethod next ((it iterator-islice))
  (with-slots (base-it curr start stop delta) it
    (if (< curr start)
        (progn (loop for _ below start
                     do (incf curr) (next base-it))
               (if (< curr stop)
                   (progn (incf curr) (next base-it))
                   (error 'stop-iteration)))
        (let (el)
          (loop for i below delta
                when (>= curr stop) do (error 'stop-iteration)
                  do (incf curr)
                     (setf el (next base-it)))
          el))))

;; tee
(dcl:defclass/std iterator-tee (iterator)
  ((stopped q base-iter)))

(defun tee (base-iter &optional (n 2))
  (let ((base-iter (make-iterator base-iter))
        (q (cons nil nil)))
    (loop with tees = (make-array n)
          for i below n
          do (setf (aref tees i) (make-instance 'iterator-tee :stopped nil :q q
                                                              :base-iter base-iter))
          finally (return tees))))

(defmethod next ((it iterator-tee))
  (with-slots (stopped q tail base-iter) it
    (when (null (cdr q))
      (setf (cdr q) (cons (next base-iter) nil)))
    (setf q (cdr q)) (car q)))
