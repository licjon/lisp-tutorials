(in-package :csv-report/parser)

;; This function reads a CSV file and returns the data as a list of rows
(defun parse-csv (file-path)
  "Read a CSV file and return the data as a list of rows (minus header).
   Each row is represented as an association list with keys :date, :category, :amount, and :notes."
  (handler-case
      (let* ((all-rows (cl-csv:read-csv (probe-file file-path)))
             (data-rows (rest all-rows))) ; Skip header row
        
        ;; Convert each row to a more semantic format with association lists
        (mapcar #'row-to-alist data-rows))
      
    ;; Error handling for common scenarios
    (file-error (e)
      (format *error-output* "ERROR: Could not open file ~A: ~A~%" 
              file-path e)
      nil)
    (error (e)
      (format *error-output* "ERROR: Failed to parse CSV: ~A~%" e)
      nil)))

;; Helper function to convert a row of strings to a more useful representation
(defun row-to-alist (row)
  "Convert a raw CSV row (list of strings) to an association list with semantic keys."
  (destructuring-bind (date category amount notes) row
    (list (cons :date date)
          (cons :category category)
          (cons :amount (parse-float amount))
          (cons :notes notes))))

;; Parse a string as a floating-point number
(defun parse-float (string)
  "Parse a string as a floating-point number, gracefully handling formatting issues."
  (handler-case 
      (read-from-string string)
    (error ()
      (format *error-output* "WARNING: Could not parse amount: ~A~%" string)
      0.0)))

;; Main function to calculate totals by category
(defun total-by-category (parsed-data)
  "Process parsed CSV data and return category totals as an alist.
   The returned structure will be: ((\"Category1\" . amount1) (\"Category2\" . amount2) ...)"
  (let ((totals (make-hash-table :test #'equal)))
    
    ;; Sum up amounts for each category
    (dolist (row parsed-data)
      (let ((category (cdr (assoc :category row)))
            (amount (cdr (assoc :amount row))))
        (incf (gethash category totals 0.0) amount)))
    
    ;; Convert hash table to association list and sort by category name
    (sort (hash-table-to-alist totals) #'string< :key #'car)))

;; Helper function to convert hash table to association list
(defun hash-table-to-alist (hash-table)
  "Convert a hash table to an association list."
  (let ((result nil))
    (maphash (lambda (key value)
               (push (cons key value) result))
             hash-table)
    result))

;; Calculate the grand total across all categories
(defun grand-total (category-totals)
  "Calculate the sum of all category totals."
  (reduce #'+ category-totals :key #'cdr :initial-value 0.0))

;; REPL-friendly function for quick testing
(defun test-parser (file-path)
  "Test function to parse a CSV file and print the category totals."
  (let* ((data (parse-csv file-path))
         (totals (total-by-category data)))
    (format t "~&Parsed ~D rows from ~A~%" (length data) file-path)
    (format t "~&Category totals:~%")
    (dolist (category-total totals)
      (format t "  ~A: $~,2F~%" (car category-total) (cdr category-total)))
    (format t "~&Grand total: $~,2F~%" (grand-total totals))
    
    ;; Return the data for further processing
    (values data totals))) 