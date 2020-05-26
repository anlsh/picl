;;;; package.lisp

(defpackage #:picl
  (:use #:cl)
  (:local-nicknames (#:dcl #:defclass-std) (#:alx #:alexandria))
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
   #:zip-longest
   #:chain
   #:compress
   #:cycle
   #:repeat
   #:dropwhile
   #:takewhile
   #:starmap
   #:filterfalse
   #:permutations
   #:combinations
   #:combinations-with-rep
   #:product
   #:tee
   #:take-n))
