(asdf:defsystem #:clos-web
  :description "CLOS Web Application Tutorial"
  :author "Your Name"
  :license "MIT"
  :version "0.1.0"
  :serial t
  :depends-on (#:hunchentoot
               #:cl-who
               #:cl-json
               #:alexandria)
  :components ((:file "package")
               (:module "src"
                :components ((:file "models")
                            (:file "web-server")
                            (:file "views")
                            (:file "utils"))))
  :in-order-to ((test-op (test-op #:clos-web-tests)))) 