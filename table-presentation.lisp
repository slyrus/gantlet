
(in-package #:gantlet)

;;
;; task-presentation - nothing exciting happens here now
(define-presentation-type table-task ()
  :options ((level 0)))

;;
;; top-level-task -- use this handle the backgrounds, etc...
(defclass table-top-level-task ()
  ())

(define-presentation-type table-top-level-task ()
  :inherit-from 'table-task)

#+nil
(defmethod output-record-refined-position-test ((record table-top-level-task) x y)
  nil)

;; task-group presentation
(define-presentation-type table-task-group ())

(define-presentation-method present (task (type table-task) pane
                                          (task-view task-table-view) &key)
  ;; level is automagically passed in as a presentation-type
  ;; "option". CLIM black magic at work.
  (with-output-as-presentation
      (t task 'task :single-box t)
    (with-accessors ((start task-view-start)
                     (end task-view-end)
                     (show-task-info-hash-table task-view-show-task-info-hash-table)
                     (hide-task-children-hash-table task-view-hide-task-children-hash-table)
                     (y-offset task-view-y-offset)
                     (x-offset task-view-x-offset)
                     (task-counter task-view-task-counter)
                     (hide-completed-tasks task-view-hide-completed-tasks)
                     (hide-past-tasks task-view-hide-past-tasks)
                     (hide-non-critical-tasks task-view-hide-non-critical-tasks))
        task-view
      (let* ((pane-task-length (local-time:timestamp-difference end start))
             (pane-width (rectangle-width (sheet-region pane)))
             (pane-unit (/ pane-task-length pane-width))
             (task-height 6)
             (task-padding 2)
             (bottom-margin 2)
             (x-zoom (zoom-x-level task-view))
             (y-zoom (zoom-y-level task-view))
             (no-dates (and (null (start task))
                            (null (end task)))))
        (let* ((task-start (cond ((start task))
                                 ((gantt:first-child-task-start task)
                                  (gantt:first-child-task-start task))
                                 (t start)))
               (progress (task-progress task))
               (task-end (cond ((end task)
                                (end task))
                               #+nil ((and progress (>= progress 1.0))
                                      task-start)
                               ((gantt:last-child-task-end task)
                                (gantt:last-child-task-end task))
                               (no-dates
                                task-start)
                               (t end)))
               (expanded (gethash task show-task-info-hash-table))
               (hide-task-children (gethash task hide-task-children-hash-table)))
          (let ((xstart (/ (local-time:timestamp-difference task-start start) pane-unit))
                (xend (/ (local-time:timestamp-difference task-end start) pane-unit))
                (task-color (if (and (task-critical task)
                                     *critical-task-color*)
                                *critical-task-color*
                                (mod-elt *task-colors* task-counter)))
                (task-border-color (if (and (task-critical task)
                                            *critical-task-border-color*
                                            (or (null progress)
                                                (< progress 1.0)))
                                       *critical-task-border-color*
                                       (mod-elt *task-border-colors* task-counter))))
            (let ((presentation
                   (with-output-as-presentation (t task 'task :single-box t)
                     (with-accessors ((name name))
                         task
                       (formatting-row (pane)
                         (formatting-cell (pane :align-x :left)
                           ;; This indenting output form doesn't work as expected.
                           #+nil
                           (indenting-output (pane (* 10 level)))
                           (let* ((family :fix)
                                  (face :roman)
                                  (size :normal)
                                  (style (make-text-style family face size)))
                             (with-text-style (pane style)
                               (format pane "~A"
                                       (coerce (make-array (* 1 level) :initial-element #\Space) 'string)))
                             (if (plusp (length (gantt:task-children task)))
                                 (if hide-task-children
                                     (format pane "~A" " >")
                                     (format pane "~A" " V"))
                                 (format pane "~A" "|"))))
                         (formatting-cell (pane :align-x :left)
                           (format pane "~A" name))
                         (formatting-cell (pane :align-x :left)
                           (format pane "~A" (date-string task-start)))
                         (formatting-cell (pane :align-x :left)
                           (format pane "~A" (date-string task-end))))
                       (unless hide-task-children
                         (loop for child across (gantt:task-children task)
                            do (with-accessors ((child-start start)
                                                (child-end end)
                                                (child-progress task-progress)
                                                (child-critical task-critical))
                                   child
                                 (unless (or
                                          (and hide-non-critical-tasks
                                               (not (or child-critical
                                                        (find-task t child :key #'gantt:task-critical))))
                                          (and hide-completed-tasks
                                               child-progress
                                               (>= child-progress 1.0))
                                          (and hide-past-tasks
                                               (and child-end
                                                    (local-time:timestamp< child-end start)))
                                          (and (null child-start)
                                               (null child-end)))
                                   (present child `((table-task) :level ,(1+ level))
                                            :stream pane)))))))))
              presentation)))))))

(define-presentation-method present (task (type table-top-level-task) pane
                                          (task-view task-table-view) &key)
  (let* ((family :sans-serif)
         (face :bold)
         (size :large)
         (style (make-text-style family face size)))
    (formatting-table (pane)
      (formatting-row (pane)
        (formatting-cell (pane :align-x :right)
          (declare (ignore pane)))
        (formatting-cell (pane :align-x :left)
          (with-text-style (pane style)
            (format pane "Task")))
        (formatting-cell (pane :align-x :left)
          (with-text-style (pane style)
            (format pane "Start")))
        (formatting-cell (pane :align-x :left)
          (with-text-style (pane style)
            (format pane "End"))))
      (with-accessors ((start task-view-start)
                       (end task-view-end))
          task-view
        (loop for child across (gantt:task-children task)
           for task-group-counter from 0
           do
             (with-accessors ((child-start start)
                              (child-end end))
                 child
               (unless (and (null child-start)
                            (null child-end))

                 (with-output-as-presentation
                     (t task 'task :single-box t)
                   (present child '((table-task) :level 0)
                            :stream pane)))))))))

