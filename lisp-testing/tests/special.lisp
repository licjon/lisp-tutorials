(in-package :lisp-testing-tests)

(in-suite special-tests)

(test factorial-of-zero
  "Test that factorial of 0 is 1."
  (is (= 1 (lisp-testing:factorial 0))))

(test factorial-of-small-numbers
  "Test factorial calculation for small numbers."
  (is (= 1 (lisp-testing:factorial 1)))
  (is (= 2 (lisp-testing:factorial 2)))
  (is (= 6 (lisp-testing:factorial 3)))
  (is (= 24 (lisp-testing:factorial 4)))
  (is (= 120 (lisp-testing:factorial 5))))

(test factorial-of-negative-number
  "Test that factorial of negative number signals an error."
  (signals error (lisp-testing:factorial -1)))

(test fibonacci-sequence
  "Test calculation of Fibonacci sequence elements."
  (is (= 0 (lisp-testing:fibonacci 0)))
  (is (= 1 (lisp-testing:fibonacci 1)))
  (is (= 1 (lisp-testing:fibonacci 2)))
  (is (= 2 (lisp-testing:fibonacci 3)))
  (is (= 3 (lisp-testing:fibonacci 4)))
  (is (= 5 (lisp-testing:fibonacci 5)))
  (is (= 8 (lisp-testing:fibonacci 6))))

(test fibonacci-of-negative-number
  "Test that Fibonacci of negative number signals an error."
  (signals error (lisp-testing:fibonacci -1)))

(test power-with-integer-exponents
  "Test power calculation with integer exponents."
  (is (= 1 (lisp-testing:power 5 0)))
  (is (= 5 (lisp-testing:power 5 1)))
  (is (= 25 (lisp-testing:power 5 2)))
  (is (= 1/25 (lisp-testing:power 5 -2))))

(test power-with-non-integer-exponents
  "Test power calculation with non-integer exponents."
  (is (float= 5.0 (lisp-testing:power 5 1.0) 0.001))
  (is (float= 25.0 (lisp-testing:power 5 2.0) 0.001))
  (is (float= (sqrt 5) (lisp-testing:power 5 0.5) 0.001)))

(test power-with-negative-base
  "Test power calculation with negative base."
  (is (= 25 (lisp-testing:power -5 2)))
  (is (= -125 (lisp-testing:power -5 3)))
  (signals error (lisp-testing:power -5 0.5))) 