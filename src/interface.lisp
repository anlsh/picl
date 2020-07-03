(in-package :picl)

(defclass iterator () ())

(defgeneric make-iterator (obj))
(defmethod make-iterator ((obj function))
  obj)

(defun next (iterator)
  (funcall iterator))

;; Driver for iterate. Maybe not the right place to put it, but it should be fine
;; This code is stolen from https://github.com/BnMcGn/snakes/blob/master/iterate.lisp
;; Example: (iterate:iter (iterate:for i in-it (picl:permutations '(1 2 3))) (print i))
(iterate:defmacro-driver (iterate:FOR var IN-IT iterlike)
  (alx:with-gensyms (g curr alive)
    (let ((kwd (if iterate:generate 'iterate:generate 'iterate:for)))
      `(progn
         (iterate:with ,g = (make-iterator ,iterlike))
         (,kwd ,var next (multiple-value-bind (,curr ,alive) (funcall ,#'next ,g)
                           (if ,alive ,curr (iterate:terminate))))))))
