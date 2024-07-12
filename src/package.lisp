;;;; package.lisp

(defpackage #:picl
  (:use #:cl)
  (:local-nicknames (#:alx #:alexandria) (#:dcl #:defclass-std))
  (:shadow #:map #:count #:apply)
  (:export
   ;; Interface
   #:make-iterator
   #:next

   ;; Itertools
   #:chain
   #:chain-from-iter
   #:combinations
   #:combinations-with-rep
   #:compress
   #:count
   #:cycle
   #:dropwhile
   #:empty-iterator
   #:empty-iterator
   #:enumerate
   #:filter
   #:filterfalse
   #:islice
   #:iter-to-list
   #:iter-to-vec
   #:map
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
   #:zip-longest

   ;; Random other utils
   #:apply
   #:always
   #:never))
