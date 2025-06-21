(in-package #:clos-web.views)

;;; ========================================
;;; HTML VIEW GENERATION
;;; ========================================

;;; ========================================
;;; EXERCISE 16: Basic HTML Generation
;;; ========================================
;;; 
;;; Learn to generate HTML using CL-WHO:
;;; - Use with-html-output-to-string
;;; - Create basic HTML structure
;;; - Generate dynamic content
;;;
;;; TODO: Implement basic HTML generation
;;; HINT: Use CL-WHO macros like :html, :head, :body

;;; ========================================
;;; EXERCISE 17: Task List View
;;; ========================================
;;; 
;;; Create a view to display a list of tasks:
;;; - Generate HTML table or list of tasks
;;; - Include task details (title, status, etc.)
;;; - Add links for edit/delete actions
;;;
;;; TODO: Implement task list view
;;; HINT: Use dolist to iterate over tasks

;;; ========================================
;;; EXERCISE 18: Task Form View
;;; ========================================
;;; 
;;; Create forms for creating and editing tasks:
;;; - Input fields for task properties
;;; - Dropdown for task type selection
;;; - Submit buttons with proper actions
;;;
;;; TODO: Implement task form view
;;; HINT: Use :form with :method and :action attributes

;;; ========================================
;;; EXERCISE 19: Task Detail View
;;; ========================================
;;; 
;;; Create a detailed view for individual tasks:
;;; - Show all task properties
;;; - Display task type-specific information
;;; - Include action buttons
;;;
;;; TODO: Implement task detail view
;;; HINT: Use task-to-html from the models

;;; ========================================
;;; EXERCISE 20: Layout and Styling
;;; ========================================
;;; 
;;; Create a consistent layout with styling:
;;; - Common header and footer
;;; - CSS classes for styling
;;; - Responsive design elements
;;;
;;; TODO: Implement layout and styling
;;; HINT: Use external CSS file and consistent structure

;;; ========================================
;;; HELPER FUNCTIONS (Provided)
;;; ========================================

(defun render-layout (title content)
  "Render a complete HTML page with layout"
  (with-html-output-to-string (*standard-output* nil :prologue t)
    (:html
     (:head
      (:title title)
      (:meta :charset "utf-8")
      (:meta :name "viewport" :content "width=device-width, initial-scale=1")
      (:link :rel "stylesheet" :type "text/css" :href "/static/css/style.css"))
     (:body
      (:header
       (:h1 "Task Manager")
       (:nav
        (:a :href "/" "Home")
        (:a :href "/task/new" "New Task")))
      (:main content)
      (:footer
       (:p "Task Manager - Built with CLOS and Common Lisp"))))))

(defun render-task-list (tasks)
  "Render a list of tasks as HTML"
  (with-html-output-to-string (*standard-output* nil)
    (:div :class "task-list"
          (:h2 "Tasks")
          (:table :class "task-table"
                  (:thead
                   (:tr
                    (:th "Title")
                    (:th "Type")
                    (:th "Status")
                    (:th "Created")
                    (:th "Actions")))
                  (:tbody
                   (dolist (task tasks)
                     (render-task-row task)))))))

(defun render-task-row (task)
  "Render a single task as a table row"
  (with-html-output-to-string (*standard-output* nil)
    (:tr :class (task-css-class task)
         (:td (task-title task))
         (:td (class-name (class-of task)))
         (:td (string-downcase (task-status task)))
         (:td (format-date (task-created-at task)))
         (:td
          (:a :href (format nil "/task/~A" (task-id task)) "View")
          (:a :href (format nil "/task/~A/edit" (task-id task)) "Edit")
          (:form :method "post" :action (format nil "/task/~A/delete" (task-id task))
                 :style "display: inline;"
                 (:button :type "submit" :onclick "return confirm('Delete this task?')" "Delete")))))))

(defun render-task-form (task &optional (action "/task/create"))
  "Render a form for creating or editing a task"
  (with-html-output-to-string (*standard-output* nil)
    (:form :method "post" :action action :class "task-form"
           (:div :class "form-group"
                 (:label :for "title" "Title:")
                 (:input :type "text" :id "title" :name "title" 
                         :value (when task (task-title task)) :required t))
           (:div :class "form-group"
                 (:label :for "description" "Description:")
                 (:textarea :id "description" :name "description" :rows "3"
                           (when task (task-description task))))
           (:div :class "form-group"
                 (:label :for "task-type" "Task Type:")
                 (:select :id "task-type" :name "task-type"
                          (:option :value "personal" "Personal")
                          (:option :value "work" "Work")
                          (:option :value "urgent" "Urgent")))
           (:div :class "form-group"
                 (:label :for "status" "Status:")
                 (:select :id "status" :name "status"
                          (:option :value "pending" "Pending")
                          (:option :value "in-progress" "In Progress")
                          (:option :value "completed" "Completed")))
           (:div :class "form-actions"
                 (:button :type "submit" "Save Task")
                 (:a :href "/" "Cancel")))))

(defun render-task-detail (task)
  "Render detailed view of a single task"
  (with-html-output-to-string (*standard-output* nil)
    (:div :class "task-detail"
          (:h2 (task-display-name task))
          (:div :class "task-info"
                (:p (:strong "Description: ") (task-description task))
                (:p (:strong "Status: ") (string-downcase (task-status task)))
                (:p (:strong "Created: ") (format-date (task-created-at task)))
                (when (slot-boundp task 'priority)
                  (:p (:strong "Priority: ") (string-downcase (task-priority task))))
                (when (slot-boundp task 'deadline)
                  (:p (:strong "Deadline: ") (format-date (task-deadline task))))
                (when (slot-boundp task 'assigned-to)
                  (:p (:strong "Assigned to: ") (task-assigned-to task))))
          (:div :class "task-actions"
                (:a :href (format nil "/task/~A/edit" (task-id task)) :class "button" "Edit")
                (:a :href "/" :class "button" "Back to List")))))

;;; ========================================
;;; TESTING FUNCTIONS (For Development)
;;; ========================================

(defun test-html-generation ()
  "Test HTML generation functions"
  (format t "~%=== Testing HTML Generation ===~%")
  ;; TODO: Add HTML generation test code here
  )

(defun test-task-views ()
  "Test task view functions"
  (format t "~%=== Testing Task Views ===~%")
  ;; TODO: Add task view test code here
  )

(defun run-view-tests ()
  "Run all view tests"
  (test-html-generation)
  (test-task-views)
  (format t "~%=== View Tests Complete ===~%")) 