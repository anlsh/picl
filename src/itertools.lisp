(in-package :picl)

(def-iter iterator-range (curr stop step)

    (range (s0 &optional s1 (step 1))
      (init-state (curr (if s1 s0 0)) (stop (if s1 s1 s0)) step))

  (if (or (and (> step 0) (< curr stop))
          (and (< step 0) (> curr stop)))
      (prog1 curr (incf curr step))
      (error 'stop-iteration)))

(def-iter iterator-icount (curr step)
    (icount (start step)
      (init-state (curr start) step))
  (prog1 curr (incf curr step)))

(def-iter iterator-repeat (max curr item)
    (repeat (item &optional max-repeats)
      (init-state item (max max-repeats) (curr 0)))
  (if max
        (if (< curr max)
            (progn (incf curr) item)
            (error 'stop-iteration))
        item))

(def-iter iterator-cycle (base-iter stopped results tail)
    (cycle (iterlike)
      (init-state (base-iter (make-iterator iterlike))))
  (if stopped
        (prog1 (car tail) (setf tail (or (cdr tail) results)))
        (handler-case
            (progn (push (next base-iter) results) (car results))
          (stop-iteration ()
            (unless results (error 'stop-iteration))
            (setf base-iter nil
                  stopped t
                  results (nreverse results)
                  tail results)
            (self)))))


;; Accumulate
;; TODO This is missing the functionality for special handling of function arguments
;; also, it would be good to fold this library into generic-cl/itertools somehow
;; Also this is halfway to being useless, CL has reduce

;; Chains

(def-iter iterator-chain-from-iter (itail)
    (chain-from-iter (it-of-its)
      (init-state (itail (make-iterator it-of-its))))
  (unless itail (setf itail (make-iterator (next itail))))
  (handler-case (next itail)
    (stop-iteration ()
      (setf itail (make-iterator (next itail)))
      (self))))

(defun chain (&rest args)
  (chain-from-iter args))

;; Compress

(def-iter iterator-compress (base-iter bool-iter)
    (compress (base-iterlike bool-iterlike)
      (init-state (base-iter (make-iterator base-iterlike))
                  (bool-iter (make-iterator bool-iterlike))))
  (let ((curr-item (next base-iter)))
      (if (next bool-iter)
          curr-item
          (self))))

;; Dropwhile

(def-iter iterator-dropwhile (base-iter pred been-false)
    (dropwhile (predicate iterlike)
      (init-state (pred predicate) (base-iter (make-iterator iterlike))))
  (if been-false
        (next base-iter)
        (let ((item (next base-iter)))
          (if (funcall pred item)
              (self)
              (progn (setf been-false t)
                     item)))))

;; Filterfalse
(def-iter iterator-filterfalse (base-iter pred)
    (filterfalse (predicate iterlike)
      (init-state (pred predicate) (base-iter (make-iterator iterlike))))
  (let ((item (next base-iter)))
      (if (not (funcall pred item))
              (self)
              item)))

(def-iter iterator-starmap (base-iter fn)
    (starmap (iterlike fn)
      (init-state (base-iter (make-iterator iterlike)) fn))
  (apply fn (iter-to-list (next base-iter))))

;; TODO zip_longest

(def-iter _ (base-iter pred)
    (takewhile (predicate iterlike)
      (init-state (pred predicate) (base-iter (make-iterator iterlike))))
  (let ((item (next base-iter)))
    (if (funcall pred item)
        item
        (error 'stop-iteration))))

(def-iter _ (base-iter start stop delta curr)
    (islice (iterlike start stop delta)
            (unless (and (>= start 0) (>= stop 0) (> delta 0))
              (error (format nil "Args must all be positive~%")))
            (init-state (base-iter (make-iterator iterlike))
                        start stop delta
                        (curr 0)))
    (if (< curr start)
        (progn (loop for _ below start
                     do (incf curr)
                        (next base-iter))
               (if (< curr stop)
                   (progn (incf curr) (next base-iter))
                   (error 'stop-iteration)))
        (loop for i below delta
              with el
              when (>= curr stop) do (error 'stop-iteration)
                do (incf curr)
                   (setf el (next base-iter))
              finally (return el))))

(def-iter _ (q base-iter)
    (tee-item (iterlike q)
      (init-state (base-iter (make-iterator iterlike)) q))
  (when (null (cdr q))
    (setf (cdr q) (cons (next base-iter) nil)))
  (setf q (cdr q)) (car q))

(defun tee (iterlike &optional (n 2))
  (let ((base-iter (make-iterator iterlike))
        (q (cons nil nil)))
    (loop with tees = (make-array n)
          for i below n
          do (setf (aref tees i) (tee-item base-iter q))
          finally (return tees))))
