
(asdf:defsystem #:gantlet
  :depends-on (#:mcclim #:gantt #:cl-l10n)
  :serial t
  :components
  ((:file "package")
   (:file "style")
   (:file "utilities")
   (:file "presentation-types")
   (:file "chart-pane")
   (:file "chart-presentation")
   (:file "table-pane")
   (:file "table-presentation")
   (:file "commands")
   (:file "gantlet")
   (:file "chart-translator")
   (:file "table-translator")))
