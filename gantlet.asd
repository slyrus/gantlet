
(asdf:defsystem #:gantlet
  :depends-on (#:mcclim #:gantt #:cl-l10n)
  :serial t
  :components
  ((:file "gantlet")))
