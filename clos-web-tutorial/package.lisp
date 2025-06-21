(defpackage #:clos-web
  (:use #:cl)
  (:nicknames #:cw)
  (:export #:start-server
           #:stop-server
           #:create-task
           #:get-all-tasks
           #:delete-task
           #:update-task))

(defpackage #:clos-web.models
  (:use #:cl)
  (:nicknames #:cwm)
  (:export #:task
           #:personal-task
           #:work-task
           #:urgent-task
           #:task-title
           #:task-description
           #:task-status
           #:task-created-at
           #:task-priority
           #:task-deadline
           #:task-assigned-to
           #:task-escalation-time
           #:task-display-name
           #:task-css-class
           #:validate-task
           #:save-task
           #:task-to-html))

(defpackage #:clos-web.web
  (:use #:cl)
  (:nicknames #:cww)
  (:export #:start-server
           #:stop-server
           #:*acceptor*))

(defpackage #:clos-web.views
  (:use #:cl)
  (:nicknames #:cwv)
  (:export #:render-task-list
           #:render-task-form
           #:render-task-item
           #:render-layout))

(defpackage #:clos-web.utils
  (:use #:cl)
  (:nicknames #:cwu)
  (:export #:generate-id
           #:get-universal-time
           #:format-currency
           #:format-date)) 