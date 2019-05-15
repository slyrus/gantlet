
(in-package #:gantlet)

(defclass gantlet-chart-pane (application-pane table-pane)
  ((task :initarg :task :initform nil :accessor pane-task)))

(defclass task-chart-view (view)
  ((task :initarg :task :accessor task-view-task)
   (zoom-x-level :initform 1.0d0 :accessor zoom-x-level :initarg :zoom-x)
   (zoom-y-level :initform 1.0d0 :accessor zoom-y-level :initarg :zoom-y)
   (start :initarg :start :accessor task-view-start)
   (end :initarg :end :accessor task-view-end)
   (hide-completed-tasks :initform nil :initarg :hide-completed-tasks :accessor task-view-hide-completed-tasks)
   (hide-past-tasks :initform t :initarg :hide-past-tasks :accessor task-view-hide-past-tasks)
   (hide-cost :initform t :initarg :hide-cost :accessor task-view-hide-cost)
   (hide-non-critical-tasks :initform nil :initarg :hide-non-critical-tasks :accessor task-view-hide-non-critical-tasks)
   (x-offset :initarg :x-offset :accessor task-view-x-offset :initform 5)
   (y-offset :initarg :y-offset :accessor task-view-y-offset :initform 0)
   (task-counter :initarg :task-counter :accessor task-view-task-counter :initform 0)
   (show-task-info-hash-table :accessor task-view-show-task-info-hash-table :initform (make-hash-table))
   (hide-task-children-hash-table :accessor task-view-hide-task-children-hash-table :initform (make-hash-table))))

(defgeneric set-pane-task (pane task))

(defmethod set-pane-task ((pane gantlet-chart-pane) task)
  (setf (pane-task pane) task)
  (let ((task-view (make-instance 'task-chart-view
                       :task task
                       :start (start task)
                       :end (end task))))
    (setf (stream-default-view pane)
          task-view)
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

(defmethod shared-initialize :after ((pane gantlet-chart-pane) slot-names &key)
  (let ((task (pane-task pane)))
    (when task
      (set-pane-task pane task))))

