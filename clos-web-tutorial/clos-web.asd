(asdf:defsystem #:clos-web
  :description "CLOS Web Application Tutorial"
  :author "Your Name"
  :license "MIT"
  :version "0.1.0"
  :depends-on (#:hunchentoot
               #:cl-who
               #:cl-json
               #:alexandria)
  :components ((:file "utils-package")
               (:file "src/utils" :depends-on ("utils-package"))
               (:file "package" :depends-on ("src/utils"))
               (:module "src"
                :depends-on ("package")
                :components ((:file "models")
                            (:file "views")
                            (:file "web-server"))))
  :in-order-to ((test-op (test-op #:clos-web-tests)))) 