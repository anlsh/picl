(in-package :picl)

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

;; Product
;; TODO It's missing the functionality of "repeat" argument
(dcl:defclass/std iterator-product (iterator)
  ((item-vec state-vec lengths stopped)))

(defun product (&rest iters)
  (loop with iter-vec = (iter-to-vec iters)
        with item-vec = (make-array (length iter-vec))
        with length-vec = (make-array (length iter-vec))
        for i below (length item-vec)
        for iter across iter-vec
        do (setf (aref item-vec i) (iter-to-vec (aref iter-vec i)))
           (setf (aref length-vec i) (length (aref item-vec i)))
        minimizing (aref length-vec i) into min-len
        finally
           (if (zerop min-len)
               (empty-iterator)
               (return (make-instance 'iterator-product
                                      :item-vec item-vec
                                      :state-vec (make-array (length item-vec) :initial-element 0)
                                      :lengths length-vec)))))

(defmethod next ((it iterator-product))
  (with-slots (item-vec state-vec lengths stopped) it
    (if stopped
        (error 'stop-iteration)
        (prog1 (loop with product = (make-array (length state-vec))
                     for i below (length product)
                     for ii = (aref state-vec i)
                     do (setf (aref product i) (aref (aref item-vec i) ii))
                     finally (return product))
          (labels ((next-combo (i)
                     (if (< i 0)
                         (setf stopped t)
                         (progn
                           (incf (aref state-vec i))
                           (when (>= (aref state-vec i) (aref lengths i))
                             (setf (aref state-vec i) 0)
                             (next-combo (1- i)))))))
            (next-combo (1- (length state-vec))))))))

;; Permutations

(dcl:defclass/std iterator-permutations (iterator)
  ((state-vec iter-vec stopped)))

;; TODO Does not handle r-permutations
;; TODO Does not handle repeated elements in permutations
(defun permutations (iterlike)
  (let ((ivec (iter-to-vec iterlike)))
    (make-instance 'iterator-permutations
                   :state-vec (iter-to-vec (range 0 (length ivec)))
                   :iter-vec ivec)))

(defmethod next ((it iterator-permutations))
  (with-slots (state-vec iter-vec stopped) it
    (if stopped
        (error 'stop-iteration)
        (labels ((reverse-tail (i)
                   (loop for k below (floor (- (length state-vec) i) 2)
                         do (rotatef (aref state-vec (+ i k))
                                     (aref state-vec (- (1- (length state-vec)) k)))))
                 (next-lexic ()
                   (loop with i = nil
                         with j = nil
                         for k below (length state-vec)
                         do
                            (when (and (< k (1- (length state-vec)))
                                       (< (aref state-vec k) (aref state-vec (1+ k))))
                              (setf i k))
                            (when (and i (> (aref state-vec k) (aref state-vec i)))
                              (setf j k))
                         finally
                            (if (null i)
                                (setf stopped t)
                                (progn
                                  (rotatef (aref state-vec i) (aref state-vec j))
                                  (reverse-tail (1+ i)))))))
          (prog1
              (loop with perm-vec = (make-array (length state-vec))
                    for i below (length state-vec)
                    do (setf (aref perm-vec i) (aref iter-vec (aref state-vec i)))
                    finally (return perm-vec))
            (next-lexic))))))

;; Combinations
(dcl:defclass/std iterator-combinations (iterator)
  ((indices pool stopped r)))

(defun combinations (iterlike r)
  (let ((ivec (iter-to-vec iterlike)))
    (if (> r (length ivec))
        (empty-iterator)
        (make-instance 'iterator-combinations
                       :indices (iter-to-vec (range 0 r :delta 1))
                       :pool ivec
                       :r r))))

;; TODO Strictly speaking, this should return sets: not vectors
(defmethod next ((it iterator-combinations))
  (with-slots (indices pool stopped r) it
    (when stopped (error 'stop-iteration))
    (prog1 (loop with ret-vec = (make-array r)
                 for i below r
                 do (setf (aref ret-vec i) (aref pool (aref indices i)))
                 finally (return ret-vec))
      (loop for i from (1- r) downto 0
            when (/= (aref indices i) (+ i (length pool) (- r)))
              do (incf (aref indices i))
                 (loop for j from (1+ i) to (1- r)
                       do (setf (aref indices j) (1+ (aref indices (1- j)))))
                 (return)
            finally (setf stopped t)))))


(dcl:defclass/std iterator-combinations-with-rep (iterator)
  ((indices pool stopped r n)))

(defun combinations-with-rep (iterlike r)
  (let ((pool (iter-to-vec iterlike)))
    (if (not (and (> r 0) (> (length pool) 0)))
        (empty-iterator)
        (make-instance 'iterator-combinations-with-rep
                       :indices (iter-to-vec (repeat 0 r))
                       :pool pool
                       :r r
                       :n (length pool)))))

;; TODO Strictly speaking, this should return a multiset
(defmethod next ((it iterator-combinations-with-rep))
  (with-slots (indices pool stopped r n) it
    (when stopped (error 'stop-iteration))
    (prog1 (loop with ret-vec = (make-array r)
                 for i below r
                 do (setf (aref ret-vec i) (aref pool (aref indices i)))
                 finally (return ret-vec))
      (loop for i from (1- r) downto 0
            when (/= (aref indices i) (1- n))
              do
                 (loop for j from i below r
                       with el = (1+ (aref indices i))
                       do (setf (aref indices j) el))
                 (return)
            finally (setf stopped t)))))
