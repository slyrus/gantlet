
(in-package #:gantlet)

(defun date-string (date)
  (local-time:format-timestring nil date :format local-time:+iso-8601-date-format+))

(defun mod-elt (sequence index)
  (elt sequence (mod index (length sequence))))

