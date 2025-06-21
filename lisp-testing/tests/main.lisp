(in-package :lisp-testing-tests)

;; Define the main test suite that includes all tests
(def-suite all-tests
  :description "Master test suite for lisp-testing")

;; Define individual test suites for each module
(def-suite arithmetic-tests
  :description "Tests for arithmetic operations"
  :in all-tests)

(def-suite statistics-tests
  :description "Tests for statistical functions"
  :in all-tests)

(def-suite special-tests
  :description "Tests for special mathematical functions"
  :in all-tests)

;; Function to run all tests - renamed to avoid conflict with FiveAM
(defun run-lisp-testing-tests ()
  "Run all tests for the lisp-testing library."
  (run! 'all-tests))

;; Functions to run individual test suites
(defun run-arithmetic-tests ()
  "Run tests for arithmetic operations."
  (run! 'arithmetic-tests))

(defun run-statistics-tests ()
  "Run tests for statistical functions."
  (run! 'statistics-tests))

(defun run-special-tests ()
  "Run tests for special mathematical functions."
  (run! 'special-tests)) 