;;;; picl.asd

(asdf:defsystem #:picl
  :description "Python Iterators in Common Lisp"
  :author "Anish Moorthy <anlsh@protonmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (:defclass-std :anaphora)
  :components ((:file "package")
               (:file "interface" :depends-on ("package"))
               (:file "itertools" :depends-on ("interface"))
               (:module "iterator-impls/" :depends-on ("package")
                        :components
                        ((:file "list")))
               (:file "picl" :depends-on ("package"))))
