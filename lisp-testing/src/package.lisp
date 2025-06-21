(defpackage :lisp-testing
  (:use :cl)
  (:documentation "Main package for the lisp-testing calculator library")
  (:export
   ;; Arithmetic operations
   :add
   :subtract
   :multiply
   :divide
   
   ;; Statistical functions
   :mean
   :median
   :standard-deviation
   :variance
   
   ;; Special functions
   :factorial
   :fibonacci
   :power
   
   ;; Utility functions
   :compose
   :with-calculator))

(defpackage :lisp-testing/arithmetic
  (:use :cl :lisp-testing)
  (:documentation "Arithmetic operations for the calculator")
  (:export
   :add
   :subtract
   :multiply
   :divide))

(defpackage :lisp-testing/statistics
  (:use :cl :lisp-testing)
  (:documentation "Statistical functions for the calculator")
  (:export
   :mean
   :median
   :standard-deviation
   :variance))

(defpackage :lisp-testing/special
  (:use :cl :lisp-testing)
  (:documentation "Special mathematical functions for the calculator")
  (:export
   :factorial
   :fibonacci
   :power)) 
