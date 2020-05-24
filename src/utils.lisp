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
         ,@cons-body
         (labels ((self () ,@next-body))
           #'self)))))

;; Utilities
(defun iter-to-list (iterlike)
  (labels ((rec (iterator)
             (handler-case (cons (next iterator) (iter-to-list iterator))
               (stop-iteration () nil))))
    (rec (make-iterator iterlike))))

(defun iter-to-vec (iterlike)
  (let ((ls (iter-to-list iterlike)))
    (make-array (length ls) :initial-contents ls)))

(defun empty-iterator ()
  (make-iterator nil))

;; take-n (not in "core" itertools but very nice for testing)
(defun take-n (n iterlike)
  (let ((iterator (make-iterator iterlike)))
    (loop for i below n
          collecting (handler-case (next iterator)
                       (stop-iteration () (return a)))
            into a
          finally (return a))))
