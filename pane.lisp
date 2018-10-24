
(in-package #:gantlet)

(defclass gantlet-pane (application-pane table-pane)
  ((task :initform nil :accessor pane-task)))

(defun set-pane-task (pane task)
  (setf (pane-task pane) task)
  (setf (stream-default-view pane)
        (make-instance 'task-view
                       :task task
                       :start (start task)
                       :end (end task)))
  (let ((resource-list (find-pane-named (pane-frame pane) 'resource-list)))
    (let ((resources (task-resources task)))
      (setf (climi::visible-items resource-list) (length resources))
      (setf (clime:list-pane-items resource-list :invoke-callback nil) (mapcar #'name resources))))
  (window-refresh pane)
  (pane-needs-redisplay pane)
  (repaint-sheet pane +everywhere+))

(defmethod shared-initialize :after ((pane gantlet-pane) slot-names &key)
  (let ((task (pane-task pane)))
    (when task
      (set-pane-task pane task))))

(defun redraw (frame pane)
  (window-clear pane)
  (gantlet-display frame pane)
  (clim:redisplay-frame-pane frame pane))

(defun redisplay-app (frame pane)
  (clim:redisplay-frame-pane frame pane))

