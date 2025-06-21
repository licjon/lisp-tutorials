(asdf:defsystem "lisp-testing"
  :description "A calculator library with tests for Common Lisp CI/CD tutorial"
  :author "Your Name"
  :license "MIT"
  :version "0.1.0"
  :depends-on ()  ; No external dependencies for the main library
  :serial t
  :components ((:module "src"
                :components ((:file "package")
                           (:file "arithmetic")
                           (:file "statistics")
                           (:file "special"))))) 
