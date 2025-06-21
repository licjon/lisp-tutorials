(in-package #:clos-web.web)

;;; ========================================
;;; WEB SERVER SETUP
;;; ========================================

(defvar *acceptor* nil
  "The Hunchentoot acceptor instance")

(defvar *port* 8080
  "Default port for the web server")

;;; ========================================
;;; EXERCISE 11: Web Server Setup
;;; ========================================
;;; 
;;; Set up a basic Hunchentoot web server:
;;; - Create an acceptor on port 8080
;;; - Define basic routes for task management
;;; - Handle HTTP requests and responses
;;;
;;; TODO: Implement the web server setup
;;; HINT: Use hunchentoot:start and hunchentoot:stop

;;; ========================================
;;; EXERCISE 12: Route Definitions
;;; ========================================
;;; 
;;; Define routes for the task management application:
;;; - GET / - Show task list
;;; - GET /task/new - Show new task form
;;; - POST /task/create - Create new task
;;; - GET /task/:id - Show task details
;;; - POST /task/:id/update - Update task
;;; - POST /task/:id/delete - Delete task
;;;
;;; TODO: Implement these routes
;;; HINT: Use hunchentoot:define-easy-handler

;;; ========================================
;;; EXERCISE 13: Request Handling
;;; ========================================
;;; 
;;; Handle different types of HTTP requests:
;;; - Parse form data from POST requests
;;; - Extract parameters from URL
;;; - Return appropriate HTTP responses
;;;
;;; TODO: Implement request handling
;;; HINT: Use hunchentoot:post-parameters and hunchentoot:get-parameters

;;; ========================================
;;; EXERCISE 14: Session Management
;;; ========================================
;;; 
;;; Add session management for user state:
;;; - Start sessions for users
;;; - Store user preferences
;;; - Handle session timeouts
;;;
;;; TODO: Implement session management
;;; HINT: Use hunchentoot:start-session and hunchentoot:session-value

;;; ========================================
;;; EXERCISE 15: Error Handling
;;; ========================================
;;; 
;;; Add proper error handling for web requests:
;;; - Handle 404 errors for missing tasks
;;; - Validate form input
;;; - Show user-friendly error messages
;;;
;;; TODO: Implement error handling
;;; HINT: Use hunchentoot:abort-request-handler and condition handling

;;; ========================================
;;; HELPER FUNCTIONS (Provided)
;;; ========================================

(defun start-server (&optional (port *port*))
  "Start the web server on the specified port"
  (when *acceptor*
    (stop-server))
  (setf *acceptor* (hunchentoot:start (make-instance 'hunchentoot:acceptor
                                                     :port port)))
  (format t "~%Server started on http://localhost:~D~%" port))

(defun stop-server ()
  "Stop the web server"
  (when *acceptor*
    (hunchentoot:stop *acceptor*)
    (setf *acceptor* nil)
    (format t "~%Server stopped~%")))

(defun get-parameter (name)
  "Get a parameter from the current request"
  (or (hunchentoot:get-parameter name)
      (hunchentoot:post-parameter name)))

(defun redirect (url)
  "Redirect to a URL"
  (hunchentoot:redirect url))

(defun set-content-type (type)
  "Set the content type for the response"
  (setf (hunchentoot:content-type*) type))

;;; ========================================
;;; TESTING FUNCTIONS (For Development)
;;; ========================================

(defun test-server-startup ()
  "Test server startup and shutdown"
  (format t "~%=== Testing Server Startup ===~%")
  (start-server)
  (sleep 2)
  (stop-server))

(defun test-basic-routes ()
  "Test basic route functionality"
  (format t "~%=== Testing Basic Routes ===~%")
  ;; TODO: Add route testing code here
  )

(defun run-web-tests ()
  "Run all web server tests"
  (test-server-startup)
  (test-basic-routes)
  (format t "~%=== Web Tests Complete ===~%")) 