;;; ========================================
;;; CLOS WEB TUTORIAL - SOLUTIONS
;;; ========================================
;;; 
;;; This file contains completed solutions for the CLOS exercises.
;;; Use this as a reference if you get stuck, but try to solve
;;; the exercises yourself first!
;;; ========================================

(in-package #:clos-web.models)

;;; ========================================
;;; EXERCISE 1: Basic Task Class
;;; ========================================

(defclass task ()
  ((id :initform (generate-id) :accessor task-id)
   (title :initarg :title :accessor task-title)
   (description :initarg :description :accessor task-description)
   (status :initform :pending :accessor task-status)
   (created-at :initform (get-universal-time) :accessor task-created-at))
  (:documentation "Base class for all task types"))

;;; ========================================
;;; EXERCISE 3: Generic Functions
;;; ========================================

(defgeneric task-display-name (task)
  (:documentation "Return a formatted name for display"))

(defgeneric task-css-class (task)
  (:documentation "Return CSS class for styling"))

(defgeneric validate-task (task)
  (:documentation "Validate task data"))

(defgeneric save-task (task)
  (:method-combination :around)
  (:documentation "Save task with logging and validation"))

;;; ========================================
;;; EXERCISE 4: Method Implementations
;;; ========================================

(defmethod task-display-name ((task task))
  (task-title task))

(defmethod task-css-class ((task task))
  "task")

(defmethod validate-task ((task task))
  (and (task-title task)
       (> (length (task-title task)) 0)))

;;; ========================================
;;; EXERCISE 5: Task Inheritance
;;; ========================================

(defclass personal-task (task)
  ((priority :initform :low :accessor task-priority))
  (:documentation "Personal tasks with priority levels"))

(defclass work-task (task)
  ((deadline :initarg :deadline :accessor task-deadline)
   (assigned-to :initarg :assigned-to :accessor task-assigned-to))
  (:documentation "Work tasks with deadlines and assignments"))

(defclass urgent-task (task)
  ((priority :initform :high :accessor task-priority)
   (escalation-time :initform 3600 :accessor task-escalation-time))
  (:documentation "Urgent tasks with high priority and escalation"))

;;; ========================================
;;; EXERCISE 6: Polymorphic Behavior
;;; ========================================

(defmethod task-display-name ((task personal-task))
  (format nil "~A (~A)" (task-title task) (task-priority task)))

(defmethod task-display-name ((task work-task))
  (if (slot-boundp task 'deadline)
      (format nil "~A (Due: ~A)" (task-title task) 
              (format-date (task-deadline task)))
      (task-title task)))

(defmethod task-display-name ((task urgent-task))
  (format nil "URGENT: ~A" (task-title task)))

(defmethod task-css-class ((task personal-task))
  "personal-task")

(defmethod task-css-class ((task work-task))
  "work-task")

(defmethod task-css-class ((task urgent-task))
  "urgent-task")

(defmethod validate-task ((task personal-task))
  (and (call-next-method)
       (member (task-priority task) '(:low :medium :high))))

(defmethod validate-task ((task work-task))
  (and (call-next-method)
       (or (not (slot-boundp task 'deadline))
           (task-deadline task))))

(defmethod validate-task ((task urgent-task))
  (and (call-next-method)
       (task-escalation-time task)))

;;; ========================================
;;; EXERCISE 7: Method Combinations
;;; ========================================

(defmethod save-task :around ((task task))
  (format t "Saving task: ~A~%" (task-title task))
  (call-next-method)
  (format t "Task saved successfully~%"))

(defmethod save-task :before ((task task))
  (unless (validate-task task)
    (error "Invalid task: ~A" (task-title task))))

(defmethod save-task ((task task))
  (format t "Base save method for task: ~A~%" (task-title task)))

;;; ========================================
;;; EXERCISE 8: Task Registry
;;; ========================================

(defclass task-registry ()
  ((tasks :initform (make-hash-table :test 'equal) :allocation :class))
  (:documentation "Singleton registry for all tasks"))

(defvar *task-registry* (make-instance 'task-registry))

(defmethod add-task ((registry task-registry) task)
  (setf (gethash (task-id task) (slot-value registry 'tasks)) task)
  task)

(defmethod remove-task ((registry task-registry) task-id)
  (remhash task-id (slot-value registry 'tasks)))

(defmethod get-task ((registry task-registry) task-id)
  (gethash task-id (slot-value registry 'tasks)))

(defmethod get-all-tasks ((registry task-registry))
  (loop for task being the hash-values of (slot-value registry 'tasks)
        collect task))

;;; ========================================
;;; EXERCISE 9: JSON Serialization
;;; ========================================

(defmethod task-to-json ((task task))
  (cl-json:encode-json-to-string
   (list :id (task-id task)
         :title (task-title task)
         :description (task-description task)
         :status (task-status task)
         :created-at (task-created-at task)
         :type (class-name (class-of task)))))

(defun json-to-task (json-string)
  (let ((data (cl-json:decode-json-from-string json-string)))
    (make-instance 'task
                   :title (cdr (assoc :title data))
                   :description (cdr (assoc :description data))
                   :status (cdr (assoc :status data)))))

;;; ========================================
;;; EXERCISE 10: HTML Generation
;;; ========================================

(defmethod task-to-html ((task task))
  (cl-who:with-html-output-to-string (*standard-output* nil)
    (:div :class (task-css-class task)
          (:h3 (task-display-name task))
          (:p (task-description task))
          (:div :class "task-meta"
                (:span :class "status" (string-downcase (task-status task)))
                (:span :class "created" (format-date (task-created-at task)))))))

;;; ========================================
;;; UPDATED TEST FUNCTIONS
;;; ========================================

(defun test-basic-task ()
  "Test creating and accessing a basic task"
  (format t "~%=== Testing Basic Task ===~%")
  (let ((task (make-instance 'task 
                             :title "Learn CLOS"
                             :description "Study Common Lisp Object System")))
    (format t "Task: ~A~%" (task-title task))
    (format t "Status: ~A~%" (task-status task))
    (format t "ID: ~A~%" (task-id task))
    (format t "Created: ~A~%" (format-date (task-created-at task)))))

(defun test-task-inheritance ()
  "Test task inheritance and polymorphic behavior"
  (format t "~%=== Testing Task Inheritance ===~%")
  
  ;; Test personal task
  (let ((personal (make-instance 'personal-task 
                                 :title "Buy groceries"
                                 :description "Milk, bread, eggs")))
    (format t "Personal task: ~A~%" (task-display-name personal))
    (format t "CSS class: ~A~%" (task-css-class personal))
    (format t "Priority: ~A~%" (task-priority personal)))
  
  ;; Test work task
  (let ((work (make-instance 'work-task 
                             :title "Complete report"
                             :description "Q4 financial report"
                             :deadline (+ (get-universal-time) (* 7 24 3600)))))
    (format t "Work task: ~A~%" (task-display-name work))
    (format t "CSS class: ~A~%" (task-css-class work))
    (format t "Deadline: ~A~%" (format-date (task-deadline work))))
  
  ;; Test urgent task
  (let ((urgent (make-instance 'urgent-task 
                               :title "Fix critical bug"
                               :description "Server down issue")))
    (format t "Urgent task: ~A~%" (task-display-name urgent))
    (format t "CSS class: ~A~%" (task-css-class urgent))
    (format t "Priority: ~A~%" (task-priority urgent))))

(defun test-task-registry ()
  "Test task registry functionality"
  (format t "~%=== Testing Task Registry ===~%")
  
  ;; Add some tasks
  (let ((task1 (make-instance 'task :title "Task 1"))
        (task2 (make-instance 'personal-task :title "Task 2"))
        (task3 (make-instance 'work-task :title "Task 3")))
    
    (add-task *task-registry* task1)
    (add-task *task-registry* task2)
    (add-task *task-registry* task3)
    
    (format t "All tasks:~%")
    (dolist (task (get-all-tasks *task-registry*))
      (format t "  - ~A~%" (task-display-name task)))
    
    ;; Test JSON serialization
    (format t "~%JSON for task 1:~%")
    (format t "~A~%" (task-to-json task1))
    
    ;; Test HTML generation
    (format t "~%HTML for task 2:~%")
    (format t "~A~%" (task-to-html task2))))

(defun run-all-tests ()
  "Run all CLOS tests"
  (test-basic-task)
  (test-task-inheritance)
  (test-task-registry)
  (format t "~%=== All Tests Complete ===~%")) 