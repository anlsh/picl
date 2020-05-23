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
   #:iter-to-vec
   #:empty-iterator
   #:range
   #:icount
   #:islice
   #:chain
   #:cycle
   #:repeat
   #:permutations
   #:combinations
   #:combinations-with-rep
   #:product
   #:tee
   #:take-n))
