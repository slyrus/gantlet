
(asdf:defsystem #:gantlet
  :depends-on (#:mcclim #:gantt #:cl-l10n)
  :serial t
  :components
  ((:file "package")
   (:file "style")
   (:file "view")
   (:file "pane")
   (:file "presentation")
   (:file "gantlet")))
