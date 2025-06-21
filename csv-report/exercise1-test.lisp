(defpackage :exercise1-test
  (:use :cl)
  (:export :run-tests))

(in-package :exercise1-test)

;; Sample data for testing
(defparameter *sample-data*
  (list 
   (list (cons :date "2025-04-01") (cons :category "Groceries") (cons :amount 58.32) (cons :notes "Farmers market"))
   (list (cons :date "2025-04-03") (cons :category "Utilities") (cons :amount 120.45) (cons :notes "Electricity bill"))
   (list (cons :date "2025-04-04") (cons :category "Coffee") (cons :amount 4.50) (cons :notes "Latte"))
   (list (cons :date "2025-04-07") (cons :category "Rent") (cons :amount 1500.00) (cons :notes "Monthly rent"))
   (list (cons :date "2025-04-10") (cons :category "Groceries") (cons :amount 42.90) (cons :notes "Grocery store"))))

;; Test function for filter-by-category
(defun test-filter-by-category ()
  (format t "~%Testing filter-by-category function...~%")
  
  ;; Check if the function exists
  (unless (fboundp 'csv-report/parser:filter-by-category)
    (format t "FAIL: filter-by-category function not found in csv-report/parser package~%")
    (return-from test-filter-by-category nil))
  
  ;; Test filtering groceries
  (let ((groceries (csv-report/parser:filter-by-category *sample-data* "Groceries")))
    (unless (= (length groceries) 2)
      (format t "FAIL: Expected 2 Groceries items, but got ~A~%" (length groceries))
      (return-from test-filter-by-category nil))
    
    (unless (every (lambda (item) (string= (cdr (assoc :category item)) "Groceries")) groceries)
      (format t "FAIL: Not all items in filtered list are Groceries~%")
      (return-from test-filter-by-category nil))
    
    (let ((amounts (mapcar (lambda (item) (cdr (assoc :amount item))) groceries)))
      (unless (and (member 58.32 amounts) (member 42.90 amounts))
        (format t "FAIL: Expected specific Groceries amounts not found~%")
        (return-from test-filter-by-category nil))))
  
  ;; Test filtering utilities
  (let ((utilities (csv-report/parser:filter-by-category *sample-data* "Utilities")))
    (unless (= (length utilities) 1)
      (format t "FAIL: Expected 1 Utilities item, but got ~A~%" (length utilities))
      (return-from test-filter-by-category nil))
    
    (unless (= (cdr (assoc :amount (first utilities))) 120.45)
      (format t "FAIL: Utilities amount incorrect~%")
      (return-from test-filter-by-category nil)))
  
  ;; Test filtering non-existent category
  (let ((nonexistent (csv-report/parser:filter-by-category *sample-data* "Entertainment")))
    (unless (= (length nonexistent) 0)
      (format t "FAIL: Expected 0 Entertainment items, but got ~A~%" (length nonexistent))
      (return-from test-filter-by-category nil)))
  
  (format t "PASS: filter-by-category tests successful~%")
  t)

;; Test function for start-filtered
(defun test-start-filtered ()
  (format t "~%Testing start-filtered function...~%")
  
  ;; Check if the function exists
  (unless (fboundp 'csv-report:start-filtered)
    (format t "FAIL: start-filtered function not found in csv-report package~%")
    (return-from test-start-filtered nil))
  
  ;; Check function signature (this is a simple check that doesn't verify much)
  (let ((lambda-list (sb-introspect:function-lambda-list #'csv-report:start-filtered)))
    ;; Instead of counting raw length, check that it has the right structure
    (unless (and (>= (length lambda-list) 2)
                (eq (second lambda-list) '&optional))
      (format t "FAIL: start-filtered should take a category parameter and an optional file parameter~%")
      (return-from test-start-filtered nil)))
  
  (format t "PASS: start-filtered function exists with correct signature~%")
  t)

;; Main test runner
(defun run-tests ()
  (format t "~%Running Exercise 1 tests...~%")
  
  ;; Make sure the system is loaded
  (unless (find-package :csv-report)
    (handler-case
        (asdf:load-system :csv-report)
      (error (e)
        (format t "ERROR: Could not load csv-report system: ~A~%" e)
        (return-from run-tests nil))))
  
  (let ((filter-test (test-filter-by-category))
        (start-test (test-start-filtered)))
    (if (and filter-test start-test)
        (format t "~%All tests PASSED! Your solution looks good.~%")
        (format t "~%Some tests FAILED. Please check the errors above and try again.~%")))) 