(in-package #:clos-web.utils)

;;; ========================================
;;; UTILITY FUNCTIONS
;;; ========================================

(defun generate-id ()
  "Generate a unique ID for tasks"
  (format nil "~A" (get-universal-time)))

(defun get-universal-time ()
  "Get current universal time"
  (get-universal-time))

(defun format-currency (amount)
  "Format a number as currency"
  (format nil "$~,2F" amount))

(defun format-date (universal-time)
  "Format universal time as a readable date"
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time universal-time)
    (format nil "~4,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D"
            year month date hour minute second)))

(defun parse-date (date-string)
  "Parse a date string in YYYY-MM-DD format to universal time"
  (let ((parts (uiop:split-string date-string :separator '(#\-))))
    (when (= (length parts) 3)
      (let ((year (parse-integer (first parts)))
            (month (parse-integer (second parts)))
            (day (parse-integer (third parts))))
        (encode-universal-time 0 0 0 day month year)))))

(defun validate-email (email)
  "Basic email validation"
  (and (stringp email)
       (> (length email) 3)
       (find #\@ email)))

(defun sanitize-html (string)
  "Basic HTML sanitization"
  (when string
    (let ((result string))
      (setf result (cl-ppcre:regex-replace-all "&" result "&amp;"))
      (setf result (cl-ppcre:regex-replace-all "<" result "&lt;"))
      (setf result (cl-ppcre:regex-replace-all ">" result "&gt;"))
      (setf result (cl-ppcre:regex-replace-all "\"" result "&quot;"))
      result)))

(defun truncate-string (string length)
  "Truncate a string to specified length"
  (if (and string (> (length string) length))
      (concatenate 'string (subseq string 0 length) "...")
      string)) 