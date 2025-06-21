(in-package :lisp-testing-tests)

(in-suite statistics-tests)

(test mean-calculation
  "Test calculation of arithmetic mean."
  (is (= 3 (lisp-testing:mean '(1 2 3 4 5))))
  (is (= 2.5 (lisp-testing:mean '(1 2 3 4))))
  (is (= 10 (lisp-testing:mean '(10))))
  (is (= -2 (lisp-testing:mean '(-5 -2 -1 0)))))

(test mean-with-empty-list
  "Test that calculating mean of empty list signals an error."
  (signals error (lisp-testing:mean '())))

(test median-odd-length
  "Test median calculation with odd-length lists."
  (is (= 3 (lisp-testing:median '(1 3 5))))
  (is (= 3 (lisp-testing:median '(5 3 1))))
  (is (= 0 (lisp-testing:median '(-5 0 5)))))

(test median-even-length
  "Test median calculation with even-length lists."
  (is (= 2.5 (lisp-testing:median '(1 2 3 4))))
  (is (= 2.5 (lisp-testing:median '(4 1 3 2))))
  (is (= -1.5 (lisp-testing:median '(-5 -3 0 2)))))

(test median-single-element
  "Test median calculation with a single element."
  (is (= 42 (lisp-testing:median '(42)))))

(test median-with-empty-list
  "Test that calculating median of empty list signals an error."
  (signals error (lisp-testing:median '())))

(test variance-calculation
  "Test variance calculation."
  (is (= 0 (lisp-testing:variance '(5))))
  (is (= 2.5 (lisp-testing:variance '(1 2 3 4 5))))
  (is (= 0 (lisp-testing:variance '(3 3 3 3)))))

(test variance-with-empty-list
  "Test that calculating variance of empty list signals an error."
  (signals error (lisp-testing:variance '())))

(test standard-deviation-calculation
  "Test standard deviation calculation."
  (is (= 0 (lisp-testing:standard-deviation '(5))))
  (is (= 0 (lisp-testing:standard-deviation '(3 3 3 3))))
  (is (float= 1.58 (lisp-testing:standard-deviation '(1 2 3 4 5)) 0.01)))

(test standard-deviation-with-empty-list
  "Test that calculating standard deviation of empty list signals an error."
  (signals error (lisp-testing:standard-deviation '())))
