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
  (:import-from #:clos-web.utils #:format-date)
  (:nicknames #:cwv)
  (:export #:render-task-list
           #:render-task-form
           #:render-task-item
           #:render-layout))
