;;;; picl.asd

(asdf:defsystem #:picl
  :description "Python Itertools in Common Lisp"
  :author "Anish Moorthy <anlsh@protonmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:defclass-std #:alexandria)
  :components
  ((:module "src"
    :components ((:file "package")
                 (:file "interface" :depends-on ("package"))
                 (:file "utils" :depends-on ("interface"))
                 (:file "itertools" :depends-on ("utils"))
                 (:file "combinatoric" :depends-on ("utils"))
                 (:module "iterator-impls" :depends-on ("utils")
                  :components
                  ((:file "list")
                   (:file "vector")))
                 (:file "picl" :depends-on ("package"))))))


(asdf:defsystem #:picl/iterate
  :description "Iterate driver for PICL"
  :author "Anish Moorthy <anlsh@protonmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:picl #:alexandria #:iterate)
  :components
  ((:module "src"
    :components ((:file "iterate-driver")))))

;; Tests
(asdf:defsystem #:picl/tests
  :description "Python Iterators in Common Lisp (Tests)"
  :author "Anish Moorthy <anlsh@protonmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:picl #:picl/iterate #:fiveam #:generic-cl #:alexandria #:iterate)
  :components
  ((:module "tests"
    :components ((:file "package")
                 (:file "itertools" :depends-on ("package"))
                 (:file "combinatoric" :depends-on ("package"))))))
