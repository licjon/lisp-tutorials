(in-package :lisp-testing/statistics)

(defun mean (numbers)
  "Calculate the arithmetic mean of a list of numbers."
  (when (null numbers)
    (error "Cannot calculate mean of an empty list"))
  (/ (reduce #'+ numbers) (length numbers)))

(defun median (numbers)
  "Calculate the median value of a list of numbers."
  (when (null numbers)
    (error "Cannot calculate median of an empty list"))
  
  (let* ((sorted (sort (copy-seq numbers) #'<))
         (len (length sorted))
         (mid (floor len 2)))
    (if (oddp len)
        (nth mid sorted)
        (/ (+ (nth (1- mid) sorted) 
              (nth mid sorted)) 
           2))))

(defun variance (numbers)
  "Calculate the variance of a list of numbers."
  (when (null numbers)
    (error "Cannot calculate variance of an empty list"))
  
  (when (= 1 (length numbers))
    (return-from variance 0))
  
  (let* ((m (mean numbers))
         (sum-of-squares (reduce #'+ (mapcar (lambda (x) (expt (- x m) 2)) numbers))))
    (/ sum-of-squares (1- (length numbers)))))

(defun standard-deviation (numbers)
  "Calculate the standard deviation of a list of numbers."
  (sqrt (variance numbers))) 