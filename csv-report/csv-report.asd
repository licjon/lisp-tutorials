(asdf:defsystem "csv-report"
  :description "A simple CSV report generator written in Common Lisp."
  :author "Your Name"
  :license "MIT"
  :version "0.1.0"

  :depends-on ("cl-csv" "str") ; you can add more here as needed

  :serial t  ; load files in order
  :components ((:file "package")
               (:file "parser")
               (:file "formatter")
               (:file "main")))
