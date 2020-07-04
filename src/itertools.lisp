(in-package :picl)


(def-iter iterator-count (curr step)
    (icount (&optional (start 0) (step 1))
      "Yields the elements `start, start + 1*step, start + 2*step, ...`
      ```
      (count 2 4)
      => 2, 6, 10, 14, ...
      ```"
      (init-state (curr start) step))

  (values (prog1 curr (incf curr step)) t))


(def-iter iterator-range (curr stop step)
    (range (s0 &optional s1 (step 1))
      "```
      (range 5)
       => 0, 1, 2, 3, 4
      (range 2 5)
      => 2, 3, 4
      (range -2)
      => 0, -1
      (range 1 7 2)
      => 1, 3, 5
      ```"
      (init-state (curr (if s1 s0 0)) (stop (if s1 s1 s0)) step))

  (if (or (and (> step 0) (< curr stop))
          (and (< step 0) (> curr stop)))
      (values (prog1 curr (incf curr step)) t)
      (values nil nil)))


(def-iter iterator-enumerate (iterator curr)
    (enumerate (iterlike &optional (curr 0))
      "Yield two-element lists of indices (beginning at curr) and their corresponding elements in
      `iterlike`
      ```
      (enumerate '(a b c d))
      => #(0 a) #(1 b) #(2 c) #(3 d)
      (enumerate '(a b c d) 3)
      => #(3 a) #(4 b) #(5 c) #(6 d)
      ```"
      (init-state (iterator (make-iterator iterlike)) curr))
  (multiple-value-bind (item alive) (next iterator)
    (if alive
        ;; TODO Consings is maybe no tthe best solution?
        (multiple-value-prog1
            (values (list curr item) t)
          (incf curr))
        (values nil nil))))

(def-iter iterator-repeat (max curr item)

    (repeat (item &optional max-repeats)
      "Yields its argument forever or `max-repeats` times, if given
      ```
      (repeat t)
      => t, t, ...
      (repeat t 4)
      => t, t, t, t
      ```"
      (init-state item (max max-repeats) (curr 0)))

  (if max
      (if (< curr max)
          (values (progn (incf curr) item) t)
          (values nil nil))
      (values item t)))


(def-iter iterator-cycle (base-iter stopped results tail)

    (cycle (iterlike)
      "Continually yields the elements of its argument in order, starting over when the end is
      reached

      If the base iterator is empty, the result of iterator-cycle will be too
      ```
      (cycle '(1 2 3 4))
      => 1, 2, 3, 4, 1, 2, 3, 4, ...
      (iter-to-list (cycle '()))
      => nil
      ```"
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

(defun chain (&rest iterlikes)
  "Yields the elements of the first iterable in `iterlike`, then the second, etc.
  ```
  (chain '(1 2 3) '(4 5 6) (count 7))
  => 1, 2, 3, 4, 5, 6, 7 ...
  ```"
  (chain-from-iter iterlikes))


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
  "Returns vectors consisting of the first elements from each iterable in `iterlike`, then the
  second, etc until one is consumed
  ```
  (zip '(1 2 3) '(a b c d))
  => #(1 a). #(2 b), #(3 c)
  ```"
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
  "Returns vectors consisting of the first elements from each iterable in `iterlike`, then the
  second, etc until *all* are consumed. Once a constituent iterable has been exhausted,
  `fill-value` is used to pad the vector in its place.
  ```
  (zip nil '(1 2 3) '(a b c d))
  => #(1 a). #(2 b), #(3 c) #(nil d)
  ```"
  (zip-longest-from-itl iterlikes fill-item))


(def-iter iterator-compress (base-iter bool-iter)

    (compress (base-iterlike bool-iterlike)
      "Yields elements of `base-iterlike` while the corresponding element in `bool-iterlike`
      is truthy. Stops when either of its arguments is consumed

      ```
      (iterator-compress (count 0) (t nil t nil t nil))
      => 0 2 4
      ```"
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
      "Drops all elements of `base-iter` until `pred` first returns false, then yields all further
      elements
      ```
`     (dropwhile (lambda (x) (< 3 x) (count 0))
      => 3, 4, 5, ...
      ```"
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
      "Yields elements of `iterlike` for which `predicate` returns true
      ```
`     (filter (lambda (x) (evenp x) (count 0))
      => 0, 2, 4, ...
      ```"
      (init-state (pred predicate) (base-iter (make-iterator iterlike))))

  (multiple-value-bind (base-item base-alive) (next base-iter)
    (if base-alive
        (if (funcall pred base-item)
              (values base-item t)
              (self))
        (values nil nil))))

(defun filterfalse (predicate iterlike)

  "Yields elements of `iterlike` for which `predicate` returns false
  ```
  (filterfalse (lambda (x) (evenp x) (count 0))
  => 1, 3, 5, ...
  ```"
  (filter (lambda (x) (not (funcall predicate x))) iterlike))


(def-iter iterator-starmap (base-iter fn)

    (starmap (fn iterable-of-iterables)
      "Applies `fn` to the first argument of `iterable-of-iterables`, then the second, etc
      ```
      (starmap #'+ '(1 2) '(3 4))
      => 3, 7
      ```"
      (init-state (base-iter (make-iterator iterable-of-iterables)) fn))

  (multiple-value-bind (base-item base-alive) (next base-iter)
    (if base-alive
        (values (apply fn (iter-to-list base-item)) t)
        (values nil nil))))

(defun imap (pred &rest iterlikes)
  "Applies `fn` to the first elements of each iterable in `iterlikes`, then to the seconds, etc
  ```
  (imap #'+ '(1 2) '(3 4))
  => 4, 6
  ```"
  (starmap pred (apply #'zip iterlikes)))

(def-iter iterator-takewhile (base-iter pred been-false)

    (takewhile (predicate iterlike)
      "Yields elements of `iterlike` for which `predicate` is truthy, terminating once it
      first returns nil
      ```
`     (takewhile (lambda (x) (< 3 x) (count 0))
      => 0, 1, 2
      ```"
      (init-state (pred predicate) (base-iter (make-iterator iterlike))))

  (if been-false
      (values nil nil)
      (multiple-value-bind (base-item base-alive) (next base-iter)
        (if base-alive
            (values base-item (funcall pred base-item))
            (values nil nil)))))


(def-iter iterator-islice (base-iter start stop delta curr)

    (islice (iterlike start stop delta)
      "Works like Python's
      [`islice`](https://docs.python.org/3.8/library/itertools.html#itertools.islice)"
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
  "Returns a vector of `n` independent copies of `iterliike`. `iterlike` itself should not be used
  after it has been passed to `tee`, otherwise its copies will not be properly updated

  If the base iterable is large be careful not to advance any copy too far ahead of the others, as
  elements which have not yet been consumed by all copies do need to be stored in memory.
  ```
  i0, i1 = (tee '(1 2 3 4))
  i0 => 1, 2, 3, 4
  i1 => 1, 2, 3, 4
  ```"
  (let ((base-iter (make-iterator iterlike))
        (q (cons nil nil)))
    (loop with tees = (make-array n)
          for i below n
          do (setf (aref tees i) (tee-item base-iter q))
          finally (return tees))))
