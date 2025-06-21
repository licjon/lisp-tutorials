(in-package :lisp-testing/trigonometry)

;; Constants
(defconstant +degrees-to-radians+ (/ pi 180))

(defun sin-degrees (angle-in-degrees)
  "Calculate the sine of an angle specified in degrees."
  (sin (* angle-in-degrees +degrees-to-radians+)))

(defun cos-degrees (angle-in-degrees)
  "Calculate the cosine of an angle specified in degrees."
  (cos (* angle-in-degrees +degrees-to-radians+)))

(defun tan-degrees (angle-in-degrees)
  "Calculate the tangent of an angle specified in degrees.
   Note: Will signal an error or return a very large number when angle is close to 90, 270, etc."
  (tan (* angle-in-degrees +degrees-to-radians+))) 