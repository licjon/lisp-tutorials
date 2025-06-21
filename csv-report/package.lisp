(defpackage :csv-report
  (:use :cl)
  (:export :start))  ; This will be the main entry point

(defpackage :csv-report/parser
  (:use :cl :csv-report :cl-csv)
  (:export :parse-csv :total-by-category :grand-total :test-parser))  ; Added :grand-total

(defpackage :csv-report/formatter
  (:use :cl :csv-report)
  (:export :format-summary :format-markdown-summary :save-report-to-file :test-formatter))  ; Export all formatter functions
