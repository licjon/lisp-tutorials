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
           #:current-timestamp
           #:format-currency
           #:format-date)) 