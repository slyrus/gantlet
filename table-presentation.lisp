
(in-package #:gantlet)

#+nil
(defun present-task (task pane task-view &key (level 0)))

(define-presentation-method present (task (type task-output-record) pane
                                          (task-view task-table-view) &key)
  ;; level is automagically passed in as a presentation-type
  ;; "option". CLIM black magic at work.
  (with-output-as-presentation
      (t task 'task :record-type 'task-output-record)
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
                   (with-output-as-presentation (t task 'task-group)
                     (with-accessors ((name name))
                         task
                       (formatting-row (pane)
                         (formatting-column (pane)
                           (formatting-cell (pane :align-x :left)
                             ;; This indenting output form doesn't work as expected.
                             #+nil (indenting-output (pane (* 10 level)))
                             (format pane "~A~A"
                                     (coerce (make-array (* 2 level) :initial-element #\Space) 'string)
                                     name)))
                         (formatting-column (pane)
                           (formatting-cell (pane :align-x :left)
                             (format pane "~A" (date-string task-start))))
                         (formatting-column (pane)
                           (formatting-cell (pane :align-x :left)
                             (format pane "~A" (date-string task-end)))))
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
                                 (present child `((task-output-record) :level ,(1+ level))))))))))
              presentation)))))))

(define-presentation-method present (task (type top-level-task) pane
                                          (task-view task-table-view) &key)
  (with-accessors ((task-view-task-counter task-view-task-counter)
                   (task-view-x-offset task-view-x-offset)
                   (task-view-y-offset task-view-y-offset)
                   (task-view-hide-cost task-view-hide-cost)
                   (x-zoom zoom-x-level))
      task-view
    (setf task-view-task-counter 0
          task-view-x-offset 5
          task-view-y-offset 18)
    (let* ((str (name task))
           (family :sans-serif)
           (face :bold)
           (size :huge)
           (style (make-text-style family face size)))
      (formatting-table (pane)
        (with-accessors ((start task-view-start)
                       (end task-view-end))
          task-view
        (let*  ((pane-width (rectangle-width (sheet-region pane)))
                (end-coord (* pane-width x-zoom)))
          (loop for child across (gantt:task-children task)
             for task-group-counter from 0
             do
               (with-accessors ((child-start start)
                                (child-end end))
                   child
                 (unless (and (null child-start)
                              (null child-end))

                   (with-output-as-presentation
                       (t task 'task-group)
                     (present child '((task-output-record) :level 0))))))))))))

