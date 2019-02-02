
(in-package #:gantlet)

(defclass gantlet-pane (application-pane table-pane)
  ((task :initform nil :accessor pane-task)))

(defun set-pane-task (pane task)
  (setf (pane-task pane) task)
  (let ((task-view (make-instance 'task-view
                       :task task
                       :start (start task)
                       :end (end task))))
    (setf (stream-default-view pane)
          task-view)
    (let ((resource-list (find-pane-named (pane-frame pane) 'resource-list)))
      (let ((resources (task-resources task)))
        (setf (climi::visible-items resource-list) (length resources))
        (setf (clime:list-pane-items resource-list :invoke-callback nil)
              (mapcar #'name resources))))
    (let ((unscheduled-task-list (find-pane-named (pane-frame pane) 'unscheduled-task-list)))
      (let ((unscheduled-tasks
             (remove-if (lambda (x) (and (numberp (task-progress x))
                                         (>= (task-progress x) 1)))
                        (unscheduled-tasks task))))
        (setf (climi::visible-items unscheduled-task-list) (length unscheduled-tasks))
        (setf (clime:list-pane-items unscheduled-task-list :invoke-callback nil)
              (mapcar #'name unscheduled-tasks))))
    (labels ((critical (task)
               (labels ((%critical (task)
                          (if (task-critical task)
                              (return-from critical t)
                              (loop for child across (task-children task)
                                 do
                                   (%critical child)))))
                 (%critical task)))
             (set-expanded-state (view task)
               (unless (some #'identity
                             (loop for child across (task-children task)
                                do
                                  (set-expanded-state view child)
                                collect
                                  (critical child)))
                 (with-accessors ((hide-task-children-hash-table
                                   task-view-hide-task-children-hash-table))
                     task-view
                   (setf (gethash task hide-task-children-hash-table) t)))))
      (set-expanded-state task-view task))
    (window-refresh pane)
    (pane-needs-redisplay pane)
    (repaint-sheet pane +everywhere+)))

(defmethod shared-initialize :after ((pane gantlet-pane) slot-names &key)
  (let ((task (pane-task pane)))
    (when task
      (set-pane-task pane task))))

(defun redraw (frame pane)
  (setf (pane-needs-redisplay pane) t)
  (clim:redisplay-frame-pane frame pane))

;; this is currently called by the redisplay command but this and the
;; command can probably both go away.
(defun redisplay-app (frame pane)
  (clim:redisplay-frame-pane frame pane))

