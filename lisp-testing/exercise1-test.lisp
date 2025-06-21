(defpackage :exercise1-test
  (:use :cl)
  (:export :run-tests))

(in-package :exercise1-test)

;; Utility for float comparison
(defun float-equal (a b &optional (epsilon 0.0001))
  "Compare two floating point numbers within epsilon."
  (< (abs (- a b)) epsilon))

;; Check if a function exists
(defun function-exists-p (package-name symbol-name)
  "Check if a function exists in the specified package."
  (let* ((package (find-package package-name))
         (symbol (and package (find-symbol symbol-name package))))
    (and symbol (fboundp symbol))))

;; Test the sin-degrees function
(defun test-sin-degrees ()
  (format t "~%Testing sin-degrees function...~%")
  
  ;; Check if the function exists
  (unless (function-exists-p :lisp-testing "SIN-DEGREES")
    (format t "FAIL: sin-degrees function not found in lisp-testing package~%")
    (return-from test-sin-degrees nil))
  
  ;; Common angles for sine
  (let ((test-cases '((0 0.0) (30 0.5) (90 1.0) (180 0.0) (270 -1.0) (360 0.0))))
    (dolist (test test-cases)
      (let ((angle (first test))
            (expected (second test))
            (result (funcall (symbol-function 
                               (find-symbol "SIN-DEGREES" :lisp-testing)) 
                             (first test))))
        (unless (float-equal result expected)
          (format t "FAIL: sin-degrees(~A) returned ~A, expected ~A~%" 
                  angle result expected)
          (return-from test-sin-degrees nil)))))
  
  (format t "PASS: sin-degrees tests successful~%")
  t)

;; Test the cos-degrees function
(defun test-cos-degrees ()
  (format t "~%Testing cos-degrees function...~%")
  
  ;; Check if the function exists
  (unless (function-exists-p :lisp-testing "COS-DEGREES")
    (format t "FAIL: cos-degrees function not found in lisp-testing package~%")
    (return-from test-cos-degrees nil))
  
  ;; Common angles for cosine
  (let ((test-cases '((0 1.0) (60 0.5) (90 0.0) (180 -1.0) (270 0.0) (360 1.0))))
    (dolist (test test-cases)
      (let ((angle (first test))
            (expected (second test))
            (result (funcall (symbol-function 
                               (find-symbol "COS-DEGREES" :lisp-testing)) 
                             (first test))))
        (unless (float-equal result expected)
          (format t "FAIL: cos-degrees(~A) returned ~A, expected ~A~%" 
                  angle result expected)
          (return-from test-cos-degrees nil)))))
  
  (format t "PASS: cos-degrees tests successful~%")
  t)

;; Test the tan-degrees function
(defun test-tan-degrees ()
  (format t "~%Testing tan-degrees function...~%")
  
  ;; Check if the function exists
  (unless (function-exists-p :lisp-testing "TAN-DEGREES")
    (format t "FAIL: tan-degrees function not found in lisp-testing package~%")
    (return-from test-tan-degrees nil))
  
  ;; Common angles for tangent
  (let ((test-cases '((0 0.0) (45 1.0) (135 -1.0) (180 0.0) (225 1.0))))
    (dolist (test test-cases)
      (let ((angle (first test))
            (expected (second test))
            (result (funcall (symbol-function 
                               (find-symbol "TAN-DEGREES" :lisp-testing)) 
                             (first test))))
        (unless (float-equal result expected)
          (format t "FAIL: tan-degrees(~A) returned ~A, expected ~A~%" 
                  angle result expected)
          (return-from test-tan-degrees nil)))))
  
  ;; Special case for tan(90) which should signal error or return a very large number
  (let ((result (handler-case
                    (funcall (symbol-function 
                               (find-symbol "TAN-DEGREES" :lisp-testing)) 90)
                  (error () :error-signaled))))
    (unless (or (eq result :error-signaled)
                (> (abs result) 1000000))
      (format t "FAIL: tan-degrees(90) should approach infinity or signal an error~%")
      (return-from test-tan-degrees nil)))
  
  (format t "PASS: tan-degrees tests successful~%")
  t)

;; Check if the required files exist
(defun check-files ()
  (format t "~%Checking required files...~%")
  
  (let ((required-files '(
                         "src/trigonometry.lisp"
                         "tests/trigonometry.lisp")))
    
    (dolist (file required-files)
      (unless (probe-file file)
        (format t "FAIL: Required file ~A not found~%" file)
        (return-from check-files nil))))
  
  (format t "PASS: All required files exist~%")
  t)

;; Main test runner
(defun run-tests ()
  (format t "~%Running Exercise 1 tests...~%")
  
  ;; Make sure the system is loaded
  (unless (find-package :lisp-testing)
    (handler-case
        (asdf:load-system :lisp-testing)
      (error (e)
        (format t "ERROR: Could not load lisp-testing system: ~A~%" e)
        (return-from run-tests nil))))
  
  (let ((files-check (check-files))
        (sin-test (test-sin-degrees))
        (cos-test (test-cos-degrees))
        (tan-test (test-tan-degrees)))
    
    (format t "~%Test Summary:~%")
    (format t "Files check: ~A~%" (if files-check "PASS" "FAIL"))
    (format t "sin-degrees: ~A~%" (if sin-test "PASS" "FAIL"))
    (format t "cos-degrees: ~A~%" (if cos-test "PASS" "FAIL"))
    (format t "tan-degrees: ~A~%" (if tan-test "PASS" "FAIL"))
    
    (if (and files-check sin-test cos-test tan-test)
        (format t "~%All tests PASSED! Your solution looks good.~%")
        (format t "~%Some tests FAILED. Please check the errors above and try again.~%")))) 