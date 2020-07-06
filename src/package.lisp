;;;; package.lisp

(defpackage #:picl
  (:use #:cl)
  (:local-nicknames (#:alx #:alexandria) (#:dcl #:defclass-std))
  (:export
   ;; Interface
   #:make-iterator
   #:next

   ;; Itertools
   #:chain
   #:combinations
   #:combinations-with-rep
   #:compress
   #:cycle
   #:dropwhile
   #:empty-iterator
   #:enumerate
   #:empty-iterator
   #:filter
   #:filterfalse
   #:icount
   #:imap
   #:islice
   #:iter-to-list
   #:iter-to-vec
   #:nfold-product
   #:permutations
   #:product
   #:range
   #:repeat
   #:starmap
   #:take
   #:takewhile
   #:tee
   #:zip
   #:zip-longest))
