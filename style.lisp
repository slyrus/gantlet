
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

(defun invert-color (color)
  (multiple-value-bind (r g b)
      (color-rgb color)
    (make-rgb-color (- 1 r) (- 1 g) (- 1 b))))

(defparameter *dark-task-background-colors*
  (list (make-rgb-color 0.03 0.03 0.03)
        (make-rgb-color 0.06 0.06 0.06)))

(defparameter *light-task-background-colors*
  (mapcar #'invert-color *dark-task-background-colors*))

(defparameter *task-background-colors* *light-task-background-colors*)

(defparameter *critical-task-color*
  (make-rgb-color 1 0.9 0.9))

(defparameter *complete-task-color*
  (make-rgb-color 0.4 0.4 1))

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
  (list (make-rgb-color 0.85 0.9 0.85))
  #+nil
  (mapcar (lambda (color)
            (lighten-color color 0.6))
          *task-border-colors*))

(defparameter *task-text-color* +black+)

(defparameter *timeline-text-color* +black+)

(defparameter *app-pane-background-color* +white+)

