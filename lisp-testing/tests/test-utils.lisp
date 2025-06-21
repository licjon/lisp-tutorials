(in-package :lisp-testing-tests)

(defun float= (a b &optional (epsilon 1.0e-6))
  "Compare two floating point numbers for equality within a small epsilon.
   This is useful for testing floating point calculations where exact equality
   is not expected due to rounding errors."
  (<= (abs (- a b)) epsilon)) 