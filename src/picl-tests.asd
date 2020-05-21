;;;; picl-tests.asd

(asdf:defsystem #:picl-tests
  :description "Python Iterators in Common Lisp (Tests)"
  :author "Anish Moorthy <anlsh@protonmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (:picl :fiveam)
  :components
  ((:module "tests"
    :components ((:file "package")
                 (:file "tests" :depends-on ("package"))))))
