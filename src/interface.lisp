(in-package :picl)

(defgeneric make-iterator (iterlike)
  (:documentation
   "Creates an iterator from `iterlike`: an iterator is simply anything which can be passed
   as an argument to `next`"))

(defmethod make-iterator ((obj function))
  obj)

(defun next (iterator)
  "Produces two values, the payload and the alive-indicator

  While iterator is not yet exhausted, calling next will yield its next item and a
  truthy alive-indicator

  After (iterator has been exhausted all further calls should yield an @c(alive-indicator)
  of nil, and the payload should be ignored by the callee"
  (funcall iterator))


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
