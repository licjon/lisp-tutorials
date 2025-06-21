(asdf:defsystem "lisp-testing-tests"
  :description "Test system for lisp-testing"
  :author "Your Name"
  :license "MIT"
  :version "0.1.0"
  :depends-on (:lisp-testing
               :fiveam)  ; FiveAM testing framework
  :serial t
  :pathname "tests"
  :components ((:file "package")
               (:file "test-utils")
               (:file "main")
               (:file "arithmetic")
               (:file "statistics")
               (:file "special"))
  :perform (test-op (o c) (symbol-call :lisp-testing-tests :run-lisp-testing-tests))
  :in-order-to ((test-op (load-op :lisp-testing)))) 
