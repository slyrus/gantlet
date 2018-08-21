
(asdf:defsystem #:gantlet
  :depends-on (#:mcclim #:gantt #:gantt-example)
  :serial t
  :components
  ((:file "gantlet")))
