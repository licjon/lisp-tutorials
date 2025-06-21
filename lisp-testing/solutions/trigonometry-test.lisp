(in-package :lisp-testing-tests)

;; Define a new test suite for trigonometry
(def-suite trigonometry-tests
  :description "Tests for trigonometric functions"
  :in all-tests)

(in-suite trigonometry-tests)

;; Helper function for comparing floats within a tolerance
(defun approx= (value expected &optional (epsilon 0.0001))
  "Compare two floating point numbers within epsilon."
  (< (abs (- value expected)) epsilon))

;; Tests for sin-degrees
(test sin-degrees-test
  "Test sine function with degrees"
  ;; Common angles for sine
  (is (approx= (lisp-testing:sin-degrees 0) 0.0))
  (is (approx= (lisp-testing:sin-degrees 30) 0.5))
  (is (approx= (lisp-testing:sin-degrees 90) 1.0))
  (is (approx= (lisp-testing:sin-degrees 180) 0.0))
  (is (approx= (lisp-testing:sin-degrees 270) -1.0))
  (is (approx= (lisp-testing:sin-degrees 360) 0.0))
  
  ;; Test negative angles
  (is (approx= (lisp-testing:sin-degrees -30) -0.5))
  
  ;; Test larger angles (periodic function)
  (is (approx= (lisp-testing:sin-degrees 390) 0.5)))

;; Tests for cos-degrees
(test cos-degrees-test
  "Test cosine function with degrees"
  ;; Common angles for cosine
  (is (approx= (lisp-testing:cos-degrees 0) 1.0))
  (is (approx= (lisp-testing:cos-degrees 60) 0.5))
  (is (approx= (lisp-testing:cos-degrees 90) 0.0))
  (is (approx= (lisp-testing:cos-degrees 180) -1.0))
  (is (approx= (lisp-testing:cos-degrees 270) 0.0))
  (is (approx= (lisp-testing:cos-degrees 360) 1.0))
  
  ;; Test negative angles
  (is (approx= (lisp-testing:cos-degrees -60) 0.5))
  
  ;; Test larger angles (periodic function)
  (is (approx= (lisp-testing:cos-degrees 420) 0.5)))

;; Tests for tan-degrees
(test tan-degrees-test
  "Test tangent function with degrees"
  ;; Common angles for tangent
  (is (approx= (lisp-testing:tan-degrees 0) 0.0))
  (is (approx= (lisp-testing:tan-degrees 45) 1.0))
  (is (approx= (lisp-testing:tan-degrees 135) -1.0))
  (is (approx= (lisp-testing:tan-degrees 180) 0.0))
  (is (approx= (lisp-testing:tan-degrees 225) 1.0))
  
  ;; Test negative angles
  (is (approx= (lisp-testing:tan-degrees -45) -1.0))
  
  ;; Test approaching infinity (should be a very large number)
  (is (> (abs (lisp-testing:tan-degrees 89.99)) 1000)))

;; Add a helper function to run just the trigonometry tests
(defun run-trigonometry-tests ()
  "Run tests for trigonometric functions."
  (run! 'trigonometry-tests)) 
