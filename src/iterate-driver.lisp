(defpackage #:picl/iterate
  (:use #:cl))

(in-package :picl/iterate)

(iterate:defmacro-driver (iterate:FOR var IN-IT iterlike)
  ;; Example usage
  ;; (iterate:iter (iterate:for i in-it (picl-iterate:permutations '(1 2 3))) (print i))
  (alexandria:with-gensyms (g curr alive)
    (let ((kwd (if iterate:generate 'iterate:generate 'iterate:for)))
      `(progn
         (iterate:with ,g = (picl:make-iterator ,iterlike))
         (,kwd ,var next (multiple-value-bind (,curr ,alive) (funcall ,#'picl:next ,g)
                           (if ,alive ,curr (iterate:terminate))))))))
