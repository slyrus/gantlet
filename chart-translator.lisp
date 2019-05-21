
(in-package #:gantlet)

;; show task info
(define-gesture-name chart-show-task-info-gesture :pointer-button (:left))

(define-presentation-to-command-translator show-chart-task-info-translator
    (chart-task com-show-task-info gantlet-app
          :gesture chart-show-task-info-gesture
          :menu nil
          :tester ((object presentation event context-type)
                   (declare (ignore presentation event context-type))
                   #+(or) (print context-type *debug-io*)
                   (taskp object))
          :documentation "Show info for this task.")
    (object)
  (list object))

;; hide task children
(define-gesture-name chart-hide-task-children-gesture :pointer-button (:left :control))

(define-presentation-to-command-translator hide-chart-task-children-translator
    (chart-task com-hide-task-children gantlet-app
          :gesture chart-hide-task-children-gesture
          :menu nil
          :tester ((object presentation event)
                   (declare (ignore presentation event))
                   (taskp object))
          :documentation "Hide children for this task.")
    (object)
  (list object))

