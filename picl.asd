;;;; picl.asd

(asdf:defsystem #:picl
  :description "Python Iterators in Common Lisp"
  :author "Anish Moorthy <anlsh@protonmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :components ((:file "package")
               (:file "picl" :depends-on ("package"))))
