(in-package :picl)

(def-iter iterator-range (curr stop step)

    (range (s0 &optional s1 (step 1))
      (init-state (curr (if s1 s0 0)) (stop (if s1 s1 s0)) step))

  (if (or (and (> step 0) (< curr stop))
          (and (< step 0) (> curr stop)))
      (values (prog1 curr (incf curr step)) t)
      (values nil nil)))


(def-iter iterator-icount (curr step)

    (icount (&optional (start 0) (step 1))
      (init-state (curr start) step))

  (values (prog1 curr (incf curr step)) t))


(def-iter iterator-repeat (max curr item)

    (repeat (item &optional max-repeats)
      (init-state item (max max-repeats) (curr 0)))

  (if max
      (if (< curr max)
          (values (progn (incf curr) item) t)
          (values nil nil))
      (values item t)))


(def-iter iterator-cycle (base-iter stopped results tail)

    (cycle (iterlike)
      (init-state (base-iter (make-iterator iterlike))))

  (if stopped
      (values (prog1 (car tail) (setf tail (or (cdr tail) results)))
              t)
      (multiple-value-bind (next-item base-alive) (next base-iter)
        (if base-alive
            (values (progn (push next-item results) (car results)) t)
            (if results
                (progn (setf base-iter nil
                             stopped t
                             results (nreverse results)
                             tail results)
                       (self))
                (values nil nil))))))


(def-iter iterator-cycle (base-iter stopped results tail)

    (cycle (iterlike)
      (init-state (base-iter (make-iterator iterlike))))

  (if stopped
      (values (prog1 (car tail) (setf tail (or (cdr tail) results)))
              t)
      (multiple-value-bind (next-item base-alive) (next base-iter)
        (if base-alive
            (values (progn (push next-item results) (car results)) t)
            (if results
                (progn (setf base-iter nil
                             stopped t
                             results (nreverse results)
                             tail results)
                       (self))
                (values nil nil))))))

(def-iter iterator-zip (iterator-vec n)

    (zip-from-itl (itl-of-itls)
      (let* ((itl-of-itls (iter-to-vec itl-of-itls))
             (n (length itl-of-itls)))
        (loop for i below n
              do (setf (aref itl-of-itls i)
                       (make-iterator (aref itl-of-itls i))))
        (if (zerop n)
            (empty-iterator)
            (init-state (iterator-vec itl-of-itls) n))))

  (loop with alive = t
        for i below n
        with ret-vec = (make-array n)
        while alive
        do
           (multiple-value-bind (iter-item iter-alive) (next (aref iterator-vec i))
             (setf alive iter-alive)
             (setf (aref ret-vec i) iter-item))
        finally (return (if alive
                            (values ret-vec t)
                            (values nil nil)))))

(defun zip (&rest iterlikes)
  (zip-from-itl iterlikes))


(def-iter iterator-zip-longest (iterator-vec fill-item n num-active active-vec)

    (zip-longest-from-itl (itl-of-itls &optional fill-item)
      (let* ((itl-of-itls (iter-to-vec itl-of-itls))
             (num-active (length itl-of-itls)))
        (loop for i below num-active
              do (setf (aref itl-of-itls i)
                       (make-iterator (aref itl-of-itls i))))
        (if (zerop num-active)
            (empty-iterator)
            (init-state (iterator-vec itl-of-itls) (n num-active) num-active fill-item
                        ;; TODO Maybe this should be a vit vector?
                        (active-vec (make-array num-active :initial-element t))))))

  (loop for i below n
        with ret-vec = (make-array n)
        do (if (aref active-vec i)
               (multiple-value-bind (iter-item iter-alive) (next (aref iterator-vec i))
                 (if iter-alive
                     (setf (aref ret-vec i) iter-item)
                     (progn (decf num-active)
                            (setf (aref active-vec i) nil)
                            (setf (aref ret-vec i) fill-item))))
               (setf (aref ret-vec i) fill-item))
        finally (return (if (zerop num-active)
                            (values nil nil)
                            (values ret-vec t)))))

(defun zip-longest (fill-item &rest iterlikes)
  (zip-longest-from-itl iterlikes fill-item))


(def-iter iterator-chain-from-iter (curr-iter itail)

    (chain-from-iter (itl-of-itls)
      (let ((itl-of-itls (make-iterator itl-of-itls)))
        (multiple-value-bind (curr-iter its-alive) (next itl-of-itls)
          (if its-alive
              (init-state (curr-iter (make-iterator curr-iter))
                          (itail itl-of-itls))
              (empty-iterator)))))

  (multiple-value-bind (curr-item curr-alive) (next curr-iter)
    (if curr-alive
        (values curr-item t)
        (progn (multiple-value-bind (next-iter itail-alive) (next itail)
                 (if itail-alive
                     (progn (setf curr-iter (make-iterator next-iter))
                            (self))
                     (values nil nil)))))))

(defun chain (&rest args)
  (chain-from-iter args))


(def-iter iterator-compress (base-iter bool-iter)

    (compress (base-iterlike bool-iterlike)
      (init-state (base-iter (make-iterator base-iterlike))
                  (bool-iter (make-iterator bool-iterlike))))

  (multiple-value-bind (curr-item curr-alive) (next base-iter)
    (multiple-value-bind (bool-item bool-alive) (next bool-iter)
      (if (and curr-alive bool-alive)
          (if bool-item
              (values curr-item t)
              (self))
          (values nil nil)))))


(def-iter iterator-dropwhile (base-iter pred been-false)

    (dropwhile (predicate iterlike)
      (init-state (pred predicate) (base-iter (make-iterator iterlike))))

  (if been-false
      (next base-iter)
      (multiple-value-bind (item base-alive) (next base-iter)
        (if base-alive
            (if (funcall pred item)
                (self)
                (progn (setf been-false t)
                       (values item t)))
            (values nil nil)))))

(def-iter iterator-filter (base-iter pred)

    (filter (predicate iterlike)
      (init-state (pred predicate) (base-iter (make-iterator iterlike))))

  (multiple-value-bind (base-item base-alive) (next base-iter)
    (if base-alive
        (if (funcall pred base-item)
              (values base-item t)
              (self))
        (values nil nil))))

(defun filterfalse (predicate iterlike)
  (filter (lambda (x) (not (funcall predicate x))) iterlike))


(def-iter iterator-starmap (base-iter fn)

    (starmap (fn iterlike)
      (init-state (base-iter (make-iterator iterlike)) fn))

  (multiple-value-bind (base-item base-alive) (next base-iter)
    (if base-alive
        (values (apply fn (iter-to-list base-item)) t)
        (values nil nil))))


(def-iter iterator-takewhile (base-iter pred been-false)

    (takewhile (predicate iterlike)
      (init-state (pred predicate) (base-iter (make-iterator iterlike))))

  (if been-false
      (values nil nil)
      (multiple-value-bind (base-item base-alive) (next base-iter)
        (if base-alive
            (values base-item (funcall pred base-item))
            (values nil nil)))))


(def-iter iterator-islice (base-iter start stop delta curr)

    (islice (iterlike start stop delta)
      (unless (and (>= start 0) (>= stop 0) (> delta 0))
        (error (format nil "Args must all be positive~%")))
      (init-state (base-iter (make-iterator iterlike))
                  start stop delta
                  (curr 0)))

  (if (< curr start)
      (progn (loop for _ below start
                   for (__ base-alive) = (multiple-value-list (next base-iter))
                   when (not base-alive)
                     do (return-from self (values nil nil))
                   do (incf curr))
             (when (< curr stop)
               (incf curr)
               (next base-iter)))
      (loop with base-item
            with base-alive = t

            for i below delta
            do
               (when (or (not base-alive) (>= curr stop))
                 (return (values nil nil)))
               (multiple-value-setq (base-item base-alive)
                 (next base-iter))
               (incf curr)
            finally (return-from self (values base-item t)))))


(def-iter iterator-tee-item (q base-iter)

    (tee-item (iterlike q)
      (init-state (base-iter (make-iterator iterlike)) q))

  (if (null (cdr q))
      (multiple-value-bind (base-item base-alive) (next base-iter)
        (if base-alive
            (progn (setf (cdr q) (list base-item)))
            (return-from self (values nil nil)))))
  (setf q (cdr q)) (values (car q) t))

(defun tee (iterlike &optional (n 2))
  (let ((base-iter (make-iterator iterlike))
        (q (cons nil nil)))
    (loop with tees = (make-array n)
          for i below n
          do (setf (aref tees i) (tee-item base-iter q))
          finally (return tees))))

(defun imap (pred &rest iterlikes)
  (starmap pred (apply #'zip iterlikes)))
