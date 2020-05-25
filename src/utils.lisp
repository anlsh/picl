(in-package :picl)
;; A macro with which to define iterables: the idea being that I can switch what it
;; produces from raw function representation (which is what it currently implements)
;; to generic dispatch based (what is currently used throughout the code)
;;
;; TODO The implementation sucks :|
(defmacro def-iter (name state-vars (constructor-name constructor-params &body cons-body)
                      &body next-body)
    (declare (ignore name))
    (alexandria:with-gensyms (next-fname)
      `(labels ((,next-fname (&key ,@state-vars)
                  (labels ((self () ,@next-body)) #'self)))
         (defun ,constructor-name ,constructor-params
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
             ,@cons-body)))))

;; Utilities
(defun iter-to-list (iterlike)
  (labels ((rec (iterator)
             (multiple-value-bind (base-item base-alive) (next iterator)
               (if base-alive
                   (cons base-item (iter-to-list iterator))
                   nil))))
    (rec (make-iterator iterlike))))

(defun iter-to-vec (iterlike)
  (let ((ls (iter-to-list iterlike)))
    (make-array (length ls) :initial-contents ls)))

(defun empty-iterator ()
  (make-iterator nil))

;; take-n (not in "core" itertools but very nice for testing)
(defun take-n (n iterlike)
  (loop with iterator = (make-iterator iterlike)
        for i below n
        for (base-item base-alive) = (multiple-value-list (next iterator))
        while base-alive
        collecting base-item into ls
        finally (return ls)))
