;;;; picl.asd

(asdf:defsystem #:picl
  :description "Python Itertools in Common Lisp"
  :author "Anish Moorthy <anlsh@protonmail.com>"
  :homepage "https://anlsh.github.io/picl/"
  :license  "MIT"
  :version "1.0.1"
  :serial t
  :depends-on (#:defclass-std #:alexandria)
  :components
  ((:module "src"
    :components ((:file "package")
                 (:file "interface" :depends-on ("package"))
                 (:file "default-iterators" :depends-on ("interface"))
                 (:file "utils" :depends-on ("interface"))
                 (:file "itertools" :depends-on ("utils"))
                 (:file "combinatoric" :depends-on ("utils")))))
  :in-order-to ((test-op (test-op #:picl/tests))))


(asdf:defsystem #:picl/iterate
  :description "Iterate driver for PICL"
  :serial t
  :depends-on (#:picl #:alexandria #:iterate)
  :components
  ((:module "src"
    :components ((:file "iterate-driver")))))

;; Tests
(asdf:defsystem #:picl/tests
  :description "Tests for PICL"
  :serial t
  :depends-on (#:picl #:picl/iterate #:fiveam #:generic-cl #:alexandria #:iterate)
  :components
  ((:module "tests"
    :components ((:file "package")
                 (:file "test-itertools" :depends-on ("package"))
                 (:file "test-combinatoric" :depends-on ("package")))))
  :perform (test-op (op system)
                    (funcall (read-from-string "fiveam:run!")
                             (read-from-string "picl/tests:suite"))))
