;;;; picl.asd

(asdf:defsystem #:picl
  :description "Python Itertools in Common Lisp"
  :author "Anish Moorthy <anlsh@protonmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:defclass-std)
  :components
  ((:module "src"
    :components ((:file "package")
                 (:file "interface" :depends-on ("package"))
                 (:file "itertools" :depends-on ("interface"))
                 (:file "combinatoric" :depends-on ("interface"))
                 (:module "iterator-impls/" :depends-on ("interface")
                  :components
                  ((:file "list")))
                 (:file "picl" :depends-on ("package"))))))

;; Tests
(asdf:defsystem #:picl-tests
  :description "Python Iterators in Common Lisp (Tests)"
  :author "Anish Moorthy <anlsh@protonmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:picl #:fiveam)
  :components
  ((:module "tests"
    :components ((:file "package")
                 (:file "tests" :depends-on ("package"))))))
