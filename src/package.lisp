;;;; package.lisp

(defpackage #:picl
  (:use #:cl)
  (:local-nicknames (#:dcl #:defclass-std) (#:an #:anaphora))
  (:export
   ;; Interface
   #:next
   #:make-iterator
   #:stop-iteration

   ;; Itertools
   #:iter-to-list
   #:empty-iterator
   #:range
   #:icount
   #:islice
   #:chain
   #:cycle
   #:repeat
   #:take-n))
