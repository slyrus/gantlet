
(asdf:defsystem #:gantlet
  :depends-on (#:mcclim #:gantt #:cl-l10n)
  :serial t
  :components
  ((:file "package")
   (:file "style")
   (:file "view")
   (:file "pane")
   (:file "table-pane")
   (:file "presentation")
   (:file "table-presentation")
   (:file "gantlet")))
