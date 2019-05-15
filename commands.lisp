
(in-package #:gantlet)

(defun redraw (frame pane)
  (setf (pane-needs-redisplay pane) t)
  (clim:redisplay-frame-pane frame pane))

;; this is currently called by the redisplay command but this and the
;; command can probably both go away.
(defun redisplay-app (frame pane)
  (clim:redisplay-frame-pane frame pane))

