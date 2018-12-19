
(in-package #:gantlet)

(defparameter *task-border-colors-rgb*
  (list (make-rgb-color 0.5 0.5 1.0)
        (make-rgb-color 0.5 1.0 0.5)
        (make-rgb-color 1.0 0.5 0.5)
        (make-rgb-color 0.5 0.75 0.5)
        (make-rgb-color 0.5 0.75 0.75)
        (make-rgb-color 0.5 0.5 0.75)))

(defparameter *task-border-colors*
  (list (make-rgb-color 0.6 0.5 0.6)
        (make-rgb-color 0.5 0.5 0.5)))

(defparameter *task-background-colors*
  (list (make-rgb-color 0.03 0.03 0.03)
        (make-rgb-color 0.06 0.06 0.06)))

(defparameter *critical-task-color*
  (make-rgb-color 1 0.0 0.0))

(defparameter *complete-task-color*
  (make-rgb-color 0.5 0.5 1))

(defparameter *critical-task-border-color*
  (make-rgb-color 1 0.0 0.0))

(defun lighten-color (color lightness)
  (multiple-value-bind (r g b)
      (color-rgb color)
    (make-rgb-color
     (+ r (* (- 1 r) lightness))
     (+ g (* (- 1 g) lightness))
     (+ b (* (- 1 b) lightness)))))

(defparameter *task-colors*
  (list (make-rgb-color 0.0 0.4 0.0))
  #+nil
  (mapcar (lambda (color)
            (lighten-color color 0.6))
          *task-border-colors*))

(defparameter *task-text-color* +white+)

(defparameter *timeline-text-color* +white+)

(defparameter *app-pane-background-color* +black+)

