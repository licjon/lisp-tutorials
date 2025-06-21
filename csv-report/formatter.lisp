(in-package :csv-report/formatter)

(defun format-summary (parsed-data)
  "Format the parsed CSV data into a nice summary report.
   Takes the result of csv-report/parser:parse-csv as input."
  (let* ((category-totals (csv-report/parser:total-by-category parsed-data))
         (grand-total (csv-report/parser:grand-total category-totals)))
    
    ;; Create a multi-line formatted report
    (with-output-to-string (s)
      ;; Header
      (format s "Category Totals~%")
      (format s "===============~%~%")
      
      ;; Category breakdown
      (dolist (category-pair category-totals)
        (destructuring-bind (category . amount) category-pair
          (format s "~A: $~,2F~%" category amount)))
      
      ;; Total
      (format s "~%Total spent: $~,2F~%" grand-total))))

(defun format-markdown-summary (parsed-data)
  "Format the parsed CSV data into a Markdown-formatted report.
   This function demonstrates an alternate output format."
  (let* ((category-totals (csv-report/parser:total-by-category parsed-data))
         (grand-total (csv-report/parser:grand-total category-totals)))
    
    (with-output-to-string (s)
      ;; Header
      (format s "# Expense Report Summary~%~%")
      
      ;; Table header
      (format s "| Category | Amount |~%")
      (format s "|----------|--------|~%")
      
      ;; Category breakdown as table rows
      (dolist (category-pair category-totals)
        (destructuring-bind (category . amount) category-pair
          (format s "| ~A | $~,2F |~%" category amount)))
      
      ;; Total
      (format s "~%**Total spent:** $~,2F~%" grand-total))))

(defun save-report-to-file (parsed-data &optional (filename "expense-report.txt"))
  "Save a formatted report to a file.
   Demonstrates file output in addition to string formatting."
  (with-open-file (out filename :direction :output :if-exists :supersede)
    (write-string (format-summary parsed-data) out)
    filename))

;; REPL-friendly function for testing
(defun test-formatter (file-path)
  "Test function to parse a CSV file and display a formatted report."
  (let ((data (csv-report/parser:parse-csv file-path)))
    (if data
        (progn
          (format t "~&~A~%" (format-summary data))
          (format t "~&~%Report could also be saved to file with:~%")
          (format t "(csv-report/formatter:save-report-to-file data)~%~%")
          (format t "Or displayed as Markdown:~%~%~A~%" 
                  (format-markdown-summary data)))
        (format t "~&ERROR: Could not parse data from ~A~%" file-path)))) 