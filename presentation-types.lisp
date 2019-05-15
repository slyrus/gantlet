
(in-package #:gantlet)

;;
;;
;; Classes (for presenations) and Presentation Types
(defclass task-output-record (standard-presentation)
  ((task :initarg :task :accessor task)))

;;
;; task-output-record - nothing exciting happens here now
(define-presentation-type task-output-record ()
  :options ((level 0)))

;;
;; top-level-task -- use this handle the backgrounds, etc...
(defclass top-level-task (standard-presentation)
  ())

(define-presentation-type top-level-task ()
  :inherit-from 'task-output-record)

(defmethod output-record-refined-position-test ((record top-level-task) x y)
  nil)

;;
;; mute-presentation - children of this tree are not highlighted
(defclass mute-presentation (standard-presentation)
  ())

(define-presentation-type mute-presentation ())

(defmethod highlight-output-record-tree ((record mute-presentation) stream state)
  nil)


;; task-group presentation
(define-presentation-type task-group ())

