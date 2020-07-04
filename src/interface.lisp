(in-package :picl)

(defgeneric make-iterator (iterlike)
  (:documentation
   "Creates an iterator from @cl:param(iterlike): an iterator is simply anything which can be passed
as an argument to NEXT"))

(defmethod make-iterator ((obj function))
  obj)

(defun next (iterator)
  "Produces two values, the payload and the alive-indicator


While @c(iterator) is not yet exhausted, calling next will yield its next item and a
truthy @c(alive-indicator)


After @c(iterator) has been exhausted all further calls should yield an @c(alive-indicator)
of @(nil), and the payload should be ignored by the callee
"
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
