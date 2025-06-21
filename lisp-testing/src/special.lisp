(in-package :lisp-testing/special)

(defun factorial (n)
  "Calculate the factorial of a non-negative integer."
  (cond
    ((< n 0) (error "Factorial is only defined for non-negative integers"))
    ((= n 0) 1)
    (t (* n (factorial (1- n))))))

(defun fibonacci (n)
  "Calculate the nth Fibonacci number (0-based)."
  (cond
    ((< n 0) (error "Fibonacci is only defined for non-negative integers"))
    ((= n 0) 0)
    ((= n 1) 1)
    (t (+ (fibonacci (- n 1))
          (fibonacci (- n 2))))))

(defun power (base exponent)
  "Calculate base raised to exponent power.
   For integer exponents, use integer arithmetic.
   For non-integer exponents, use floating-point arithmetic."
  (cond
    ((integerp exponent) 
     (cond
       ((minusp exponent) (/ 1 (power base (- exponent))))
       ((zerop exponent) 1)
       (t (* base (power base (1- exponent))))))
    ((minusp base) 
     (error "Cannot raise negative base to non-integer power"))
    (t (exp (* exponent (log base)))))) 