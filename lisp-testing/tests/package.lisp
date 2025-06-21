(defpackage :lisp-testing-tests
  (:use :cl :fiveam)
  (:documentation "Test package for the lisp-testing calculator library")
  (:export 
   ;; Main test functions
   :run-lisp-testing-tests
   :run-arithmetic-tests
   :run-statistics-tests
   :run-special-tests
   
   ;; Test suites
   :all-tests
   :arithmetic-tests
   :statistics-tests
   :special-tests
   
   ;; Test utilities
   :float=)) 
