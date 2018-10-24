
(in-package #:gantlet)

(defclass task-view (view)
  ((task :initarg :task :accessor task-view-task)
   (zoom-x-level :initform 1.0d0 :accessor zoom-x-level)
   (zoom-y-level :initform 1.0d0 :accessor zoom-y-level)
   (start :initarg :start :accessor task-view-start)
   (end :initarg :end :accessor task-view-end)
   (hide-completed-tasks :initform t :initarg :hide-completed-tasks :accessor task-view-hide-completed-tasks)
   (hide-non-critical-tasks :initform nil :initarg :hide-non-critical-tasks :accessor task-view-hide-non-critical-tasks)
   (x-offset :initarg :x-offset :accessor task-view-x-offset :initform 5)
   (y-offset :initarg :y-offset :accessor task-view-y-offset :initform 0)
   (task-counter :initarg :task-counter :accessor task-view-task-counter :initform 0)
   (show-task-info-hash-table :accessor task-view-show-task-info-hash-table :initform (make-hash-table))
   (hide-task-children-hash-table :accessor task-view-hide-task-children-hash-table :initform (make-hash-table))))

