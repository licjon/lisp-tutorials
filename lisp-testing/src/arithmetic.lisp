(in-package :lisp-testing/arithmetic)

(defun add (a b)
  "Add two numbers together."
  (+ a b))

(defun subtract (a b)
  "Subtract b from a."
  (- a b))

(defun multiply (a b)
  "Multiply two numbers together."
  (* a b))

(defun divide (a b)
  "Divide a by b. Signals an error if b is zero."
  (when (zerop b)
    (error 'division-by-zero 
           :operation 'divide
           :operands (list a b)))
  (/ a b)) 