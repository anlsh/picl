(in-package :picl)
;; A macro with which to define iterables: the idea being that I can switch what it
;; produces from raw function representation (which is what it currently implements)
;; to generic dispatch based (what is currently used throughout the code)
;;
;; TODO The implementation sucks :|
(defmacro def-iter (name state-vars (constructor-name constructor-params &body constructor-body)
                    &body next-body)
  "An anaphoric convenience macro which can be re-implemented to change how iterators are
  structured. Exposes the local `init-state` macro within `constructor-body`

  If you don't want to-use this macro (which I totally understand) just write constructors for your
  iterators returning 0-argument closures representing the `next` function and you'll be fine. This
  macro just helped me get through a lot of waffling during development

  Parameters
  ----------
  `name` is a synbol naming the iterator's backing state. However it's actually
  ignored, since PICL doesn't currently expose iterators' backing state

  `state-vars` is a list of symbols naming the iterator's state variables.

  The `constructor-name/params/body` will be `defun`'d to create the user-facing constructor
  function. In `constructor-body`, the return value should be an iterator or a call to local
  `init-state` macro

  `next-body` The definition of the `next` function for this iterator, which will be run in a
   lexical environment consisting of the state vars

  Both `constructor-body` and `next-body` are defined in the lexical environment of the `def-iter`
  form, and as such can access any lexical bindings not shadowed by the `constructor-params` and
  `state-vars` respectively

  The local `init-state` macro.
  ----------------------------
  The arguments to `init-state` should be of the form `(statevar-symbol value-form)`or
  `statevar-symbol`. In the first case, the value of `value-form` will be used to initialize the
  state variable named by `symbol`. In the second case, `statevar-symbol` is also used as the
  corresponding `value-form`

  Example
  -------
  What follows is a sample implementation of the `count` iterator using this macro
  ```
  (def-iter ignored-backing-name (curr-val step)
    (count (start step)
      (init-state (curr-val start) step))
    (prog1 curr-val (incf curr-val step))))
  ````"

  (declare (ignore name))
  (alexandria:with-gensyms (next-fname)
    (let ((docstring "Function had no docstring, so this one was inserted"))
      (when (and (cdr constructor-body) (stringp (car constructor-body)))
        (setf docstring (car constructor-body)
              constructor-body (cdr constructor-body)))
      `(labels ((,next-fname (&key ,@state-vars)
                  (labels ((self () ,@next-body)) #'self)))
         (defun ,constructor-name ,constructor-params
           ,docstring
           (macrolet
               ((init-state (&rest argspecs)
                  (list 'apply '#',next-fname
                        (apply #'append
                               '(list )
                               (mapcar (lambda (aspec)
                                         (if (symbolp aspec)
                                             (list (alx:make-keyword aspec) aspec)
                                             (destructuring-bind (aname adef) aspec
                                               (list (alx:make-keyword aname) adef))))
                                       argspecs)))))
             ,@constructor-body))))))

;; Utilities
(defun iter-to-list (iterlike)
  "Reads `iterlike` into a list
  ```
  (iter-to-list (range 4))
  => (0 1 2 3)
  (iter-to-list (count 0))
  => Out of memory error!
  ```"
  (labels ((rec (iterator)
             (multiple-value-bind (base-item base-alive) (next iterator)
               (if base-alive
                   (cons base-item (iter-to-list iterator))
                   nil))))
    (rec (make-iterator iterlike))))

(defun iter-to-vec (iterlike)
  "Reads `iterlike` into a vector
  ```
  (iter-to-list (range 4))
  => #(0 1 2 3)
  (iter-to-list (count 0))
  => Out of memory error!
  ```"
  (let ((ls (iter-to-list iterlike)))
    (make-array (length ls) :initial-contents ls)))

(defun empty-iterator ()
  "Returns an empty iterator
  ```
  (iter-to-list (empty-iterator))
  => nil
  ```"
  (make-iterator nil))

(defun take (n iterlike)
  "Returns a list consisting of the first `n` (or fewer, if the iterator runs out) items of iterlike
  ```
  (take-n 5 (range 30))
  => (0 1 2 3 4)
  take-n 30 (range 4)
  => (0 1 2 3)
  ```"
  (loop with iterator = (make-iterator iterlike)
        for i below n
        for (base-item base-alive) = (multiple-value-list (next iterator))
        while base-alive
        collecting base-item into ls
        finally (return ls)))


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
