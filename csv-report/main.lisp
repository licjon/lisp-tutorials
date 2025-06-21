(in-package :csv-report)

;; Helper function to find files relative to the system location
(defun system-relative-path (system-name path)
  "Find a path relative to the ASDF system's location."
  (merge-pathnames path (asdf:system-source-directory system-name)))

(defun start (&optional (file nil))
  "Main entry point. Loads a CSV file and prints a formatted summary.
If file is NIL, use the default expenses.csv in the system directory.
Otherwise, use the specified file path."
  (let* ((default-csv (system-relative-path "csv-report" "expenses.csv"))
         (csv-path (or file default-csv))
         (parsed-data (csv-report/parser:parse-csv csv-path)))
    
    (if parsed-data
        (let ((summary (csv-report/formatter:format-summary parsed-data)))
          (format t "~a~%" summary))
        (format t "ERROR: Could not parse data from ~A~%" csv-path))))
