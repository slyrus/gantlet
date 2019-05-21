
(in-package #:gantlet)

;; hide task children
(define-gesture-name table-hide-task-children-gesture :pointer-button (:left))

(define-presentation-to-command-translator hide-table-task-children-translator
    (table-task com-hide-task-children gantlet-app
          :gesture table-hide-task-children-gesture
          :menu nil
          :tester ((object presentation event)
                   (declare (ignore presentation event))
                   (taskp object))
          :documentation "Hide children for this task.")
    (object)
  (list object))

;; show task info
(define-gesture-name table-show-task-info-gesture :pointer-button (:left :control))

(define-presentation-to-command-translator show-table-task-info-translator
    (table-task com-show-task-info gantlet-app
          :gesture table-show-task-info-gesture
          :menu nil
          :tester ((object presentation event context-type)
                   (declare (ignore presentation event context-type))
                   #+(or) (print context-type *debug-io*)
                   (taskp object))
          :documentation "Show info for this task.")
    (object)
  (list object))

