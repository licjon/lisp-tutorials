(in-package :lisp-testing-tests)

(in-suite arithmetic-tests)

(test add-two-positive-numbers
  "Test that adding two positive numbers works correctly."
  (is (= 5 (lisp-testing:add 2 3)))
  (is (= 10 (lisp-testing:add 7 3))))

(test add-with-negative-numbers
  "Test addition with negative numbers."
  (is (= -1 (lisp-testing:add 2 -3)))
  (is (= 0 (lisp-testing:add -7 7))))

(test add-with-zero
  "Test addition with zero."
  (is (= 5 (lisp-testing:add 5 0)))
  (is (= -3 (lisp-testing:add 0 -3)))
  (is (= 0 (lisp-testing:add 0 0))))

(test subtract-positive-numbers
  "Test subtraction with positive numbers."
  (is (= 2 (lisp-testing:subtract 5 3)))
  (is (= -2 (lisp-testing:subtract 3 5))))

(test subtract-with-negative-numbers
  "Test subtraction with negative numbers."
  (is (= 5 (lisp-testing:subtract 2 -3)))
  (is (= -10 (lisp-testing:subtract -7 3))))

(test multiply-positive-numbers
  "Test multiplication with positive numbers."
  (is (= 15 (lisp-testing:multiply 5 3)))
  (is (= 0 (lisp-testing:multiply 0 5))))

(test multiply-with-negative-numbers
  "Test multiplication with negative numbers."
  (is (= -15 (lisp-testing:multiply 5 -3)))
  (is (= 15 (lisp-testing:multiply -5 -3))))

(test divide-positive-numbers
  "Test division with positive numbers."
  (is (= 4 (lisp-testing:divide 12 3)))
  (is (= 2.5 (lisp-testing:divide 5 2))))

(test divide-with-negative-numbers
  "Test division with negative numbers."
  (is (= -4 (lisp-testing:divide -12 3)))
  (is (= 4 (lisp-testing:divide -12 -3))))

(test divide-by-zero
  "Test that division by zero signals an error."
  (signals division-by-zero (lisp-testing:divide 5 0))) 
