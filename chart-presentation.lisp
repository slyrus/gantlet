
(in-package #:gantlet)

(defclass chart-task () ())

;;
;; task-presentation - nothing exciting happens here now
(define-presentation-type chart-task ())

(defclass chart-task-presentation (standard-presentation) ())

;;
;; top-level-task -- use this handle the backgrounds, etc...
(defclass chart-top-level-task ()
  ())

(define-presentation-type chart-top-level-task ()
  :inherit-from 'chart-task)

#+nil
(defmethod output-record-refined-position-test ((record chart-top-level-task) x y)
  nil)

;;
;; mute-presentation - children of this tree are not highlighted
(defclass mute-presentation (standard-presentation)
  ())

(define-presentation-type mute-presentation ())

(defmethod highlight-output-record-tree ((record mute-presentation) stream state)
  nil)


;; task-group presentation
(define-presentation-type chart-task-group ())

#+nil
(define-presentation-method highlight-presentation
    ((type chart-task) (record chart-task-presentation) stream state))


;;
;; actual drawing routines
(defun draw-task (task pane x1 y1 x2 y2
                  &key fill-color
                       border-color
                       (expanded t))
  (let* ((text-left-margin 6)
         (text-top-margin 2)
         (text-size *default-text-size*)
         (family nil)
         (face :roman)
         (style (make-text-style family face text-size)))
    ;; first we need to compute text sizes, then we can draw the
    ;; boxes, and then, finally, the strings that go in the boxes.

    (with-accessors ((start-date start)
                     (end-date end)
                     (notes task-notes)
                     (progress task-progress)
                     (name name)
                     (task-cost task-cost)
                     (cost cost)
                     (task-type task-type))
        task
      (let (text-list)
        (flet ((add-text (str)
                 (multiple-value-bind (width height final-x final-y baseline)
	             (text-size pane str :text-style style)
                   (declare (ignore final-x final-y baseline))
                   (push (list str width (+ height)) text-list))))
          (add-text name)
          (when expanded
            (when start-date
              (add-text (format nil "Start: ~A" (date-string start-date))))
            (when end-date
              (add-text (format nil "End: ~A" (date-string end-date))))
            (when task-cost
              (add-text (format nil "Task Cost: ~A" (cl-l10n:format-number/currency nil task-cost 'ldml:usd))))
            (when cost
              (add-text (format nil "Total Cost: ~A" (cl-l10n:format-number/currency nil cost 'ldml:usd))))
            (when notes
              (loop for note in notes
                 do
                   (add-text (format nil "~A" note))))
            (let ((resources (task-resources task)))
              (loop for resource in resources
                 do
                   (add-text (format nil "Resource: ~A" (name resource))))))
          (let ((height (apply #'+ (mapcar #'third text-list)))
                (max-width (apply #'max (mapcar #'second text-list))))
            (incf y2 height)
            (with-output-as-presentation
                (t
                 task
                 'chart-task :record-type 'chart-task-presentation)
              (climi::invoke-surrounding-output-with-border
	       pane
               (lambda (pane)
                 (if (equal gantt:task-type :task)
                     (draw-rectangle* pane x1 y1 x2 y2 :ink fill-color)
                     ;; FIXME transparent ink blows up the PDF backend
                     ;; for now, so just use fill-color.
                     (draw-rectangle* pane x1 y1 x2 y2 :ink fill-color)))
               :shape :rectangle
               :filled nil
               :ink border-color
               :outline-ink border-color
               :line-thickness 3
               :padding-left 0
               :padding-right -2
               :padding-top 0
               :padding-bottom -2)
              (with-output-as-presentation
                  (pane nil 'mute-presentation :record-type 'mute-presentation)
                (when progress
                  (draw-rectangle* pane
                                   x1
                                   (+ y1 1)
                                   (+ (* (- x2 x1) progress) x1)
                                   (+ y1 5)
                                   :ink *complete-task-color* :filled t)))
              (let ((text-x-offset (if (> max-width (- x2 x1 7))
                                       (+ x2 text-left-margin 2)
                                       (+ x1 text-left-margin)))
                    (text-color (if (> max-width (- x2 x1 7))
                                    *task-text-color*
                                    *task-text-color*))
                    (y-offset y1))
                (flet ((draw-text-line (text-spec)
                         (destructuring-bind (text width height)
                             text-spec
                           (declare (ignore width))
                           (draw-text* pane
                                       text
                                       text-x-offset
                                       (if expanded
                                           (+ y-offset text-top-margin)
                                           (+ y-offset text-top-margin (/ (- y2 y1) 2)))
                                       :align-y (if expanded :top :center)
                                       :ink text-color
                                       :text-style style)
                           (incf y-offset height))))
                  (let ((lines (reverse text-list)))
                    (when (car lines)
                      (draw-text-line (car lines))
                      (with-output-as-presentation
                          (pane nil 'mute-presentation :record-type 'mute-presentation)
                        (loop for text-spec in (cdr lines)
                           do (draw-text-line text-spec))))))))))))))

;;
;; display strategy
;; 
;; 1. set up/present a task view (really the top level task/view)
;;
;; 2. setup/present the task details and (recursively) do the same for
;; child tasks

(define-presentation-method present (task (type chart-task) pane
                                          (task-view task-chart-view) &key)
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
                 (with-output-as-presentation
                     (t
                      task
                      'chart-task-group)
                   (with-bounding-rectangle* (x1 y1 x2 y2)
                       (draw-task task
                                  pane
                                  (+ x-offset (* x-zoom (max 0 xstart)))
                                  y-offset
                                  (+ x-offset (* x-zoom (min xend pane-width)))
                                  (+ y-offset (* y-zoom task-height) bottom-margin)
                                  :fill-color task-color
                                  :border-color task-border-color
                                  :expanded expanded)
                     (declare (ignore x1 x2))
                     (incf y-offset (+ (- y2 y1) task-padding))
                     (incf task-counter))
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
                               (present child 'chart-task :record-type 'chart-task-presentation :stream pane))))))))
            presentation))))))

;;
;; this is unused?!?
(defun draw-text-lines (pane x y lines &key (style *default-text-style*)
                                            (text-top-margin 2)
                                            (ink +black+)
                                            (align-y :top))
  (let ((text-list)
        (y-offset y))
    (flet ((add-text (str)
             (multiple-value-bind (width height final-x final-y baseline)
	         (text-size pane str :text-style style)
               (declare (ignore final-x final-y baseline))
               (push (list str width (+ text-top-margin height)) text-list)))
           (draw-text-line (text-spec)
             (destructuring-bind (text width height)
                 text-spec
               (declare (ignore width))
               (draw-text* pane text
                           x y-offset
                           :align-y align-y
                           :ink ink
                           :text-style style)
               (incf y-offset height))))
      (mapcar #'add-text lines)
      (mapcar #'draw-text-line (reverse text-list)))))

(defun draw-timeline-years (pane task-view style)
  (with-accessors ((start task-view-start)
                   (end task-view-end)
                   (y task-view-y-offset)
                   (x-zoom zoom-x-level))
      task-view
    (let* ((pane-task-length (local-time:timestamp-difference end start))
           (pane-width (rectangle-width (sheet-region pane)))
           (pane-unit (/ pane-task-length pane-width)))
      (multiple-value-bind (dyear)
          (time-interval::timestamp-decoded-difference end start)
        (let* ((nyears (+ dyear 2))
               (year-starts
                (cons start
                      (loop for i from 1 to nyears
                         collect
                           (local-time:with-decoded-timestamp
                               (:year start-year)
                               start
                             (local-time:adjust-timestamp
                                 (local-time:encode-timestamp 0 0 0 0 1 1 start-year)
                               (:offset :year i))))))
               (year-coords
                (mapcar (lambda (t1)
                          (/ (local-time:timestamp-difference
                              t1
                              (elt year-starts 0))
                             pane-unit))
                        year-starts)))
          (loop for i to nyears
             do
               (local-time:with-decoded-timestamp (:year year)
                   (elt year-starts i)
                 (when (< i nyears)
                   (when (> (- (elt year-coords (1+ i))
                               (elt year-coords i))
                            25)
                     (draw-text* pane
                                 (format nil "~A" year)
                                 (+ (* x-zoom (elt year-coords i)) 4)
                                 (+ y 15)
                                 :align-y :center
                                 :text-style style
                                 :ink *timeline-text-color*))
                   (draw-rectangle* pane
                                    (* x-zoom (elt year-coords i))
                                    y
                                    (* x-zoom (elt year-coords (1+ i)))
                                    (+ y 30)
                                    :ink +red+
                                    :filled nil)))))))))

(defun draw-timeline-quarters (pane task-view style)
  (with-accessors ((start task-view-start)
                   (end task-view-end)
                   (y task-view-y-offset)
                   (x-zoom zoom-x-level))
      task-view
    (let* ((pane-task-length (local-time:timestamp-difference end start))
           (pane-width (rectangle-width (sheet-region pane)))
           (pane-unit (/ pane-task-length pane-width)))
      (multiple-value-bind (dyear dmonth)
          (time-interval::timestamp-decoded-difference end start)
        (let* ((dquarter (ceiling dmonth 3))
               (nquarters (+ (* dyear 4) dquarter 3))
               (quarter-starts
                (cons start
                      (loop for i from 1 below nquarters
                         collect
                           (local-time:with-decoded-timestamp
                               (:year start-year :month start-month)
                               start
                             (local-time:adjust-timestamp
                                 (local-time:encode-timestamp 0 0 0 0 1
                                                              (1+ (* (floor (1- start-month) 3) 3))
                                                              start-year)
                               (:offset :month (* i 3)))))))
               (quarter-coords
                (mapcar (lambda (t1)
                          (/ (local-time:timestamp-difference
                              t1
                              (elt quarter-starts 0))
                             pane-unit))
                        quarter-starts)))
          (loop for i below nquarters
             do
               (local-time:with-decoded-timestamp (:year year :month month)
                   (elt quarter-starts i)
                 (when (< i (1- nquarters))
                   (when (> (- (elt quarter-coords (1+ i))
                               (elt quarter-coords i))
                            25)
                     (draw-text* pane
                                 (format nil "Q~A ~A"
                                         (1+ (floor (1- month) 3))
                                         year)
                                 (+ (* x-zoom (elt quarter-coords i)) 4)
                                 (+ y 15)
                                 :align-y :center
                                 :text-style style
                                 :ink *timeline-text-color*))
                   (draw-rectangle* pane
                                    (* x-zoom (elt quarter-coords i))
                                    y
                                    (* x-zoom (elt quarter-coords (1+ i)))
                                    (+ y 30)
                                    :ink +red+
                                    :filled nil)))))))))

(defun draw-timeline-months (pane task-view style)
  (with-accessors ((start task-view-start)
                   (end task-view-end)
                   (y task-view-y-offset)
                   (x-zoom zoom-x-level))
      task-view
    (let* ((pane-task-length (local-time:timestamp-difference end start))
           (pane-width (rectangle-width (sheet-region pane)))
           (pane-unit (/ pane-task-length pane-width)))
      (multiple-value-bind (dyear dmonth)
          (time-interval::timestamp-decoded-difference end start)
        (let* ((nmonths (+ (* dyear 12) dmonth 2))
               (month-starts
                (cons start
                      (loop for i from 1 below nmonths
                         collect
                           (local-time:with-decoded-timestamp
                               (:year start-year :month start-month)
                               start
                             (local-time:adjust-timestamp
                                 (local-time:encode-timestamp 0 0 0 0 1 start-month start-year)
                               (:offset :month i))))))
               (month-coords
                (mapcar (lambda (t1)
                          (/ (local-time:timestamp-difference
                              t1
                              (elt month-starts 0))
                             pane-unit))
                        month-starts)))
          (loop for i below nmonths
             do
               (local-time:with-decoded-timestamp (:year year :month month)
                   (elt month-starts i)
                 (when (< i (1- nmonths))
                   (if (> (- (elt month-coords (1+ i))
                               (elt month-coords i))
                            25)
                       (draw-text* pane
                                   (format nil "~A ~A"
                                           (aref local-time:+short-month-names+ month)
                                           year)
                                   (+ (* x-zoom (elt month-coords i)) 4)
                                   (+ y 15)
                                   :align-y :center
                                   :text-style style
                                   :ink *timeline-text-color*))
                   (draw-rectangle* pane
                                    (* x-zoom (elt month-coords i))
                                    y
                                    (* x-zoom (elt month-coords (1+ i)))
                                    (+ y 30)
                                    :ink +red+
                                    :filled nil)))))))))

(defun draw-timeline (pane task-view)
  (with-accessors ((start task-view-start)
                   (end task-view-end)
                   (y task-view-y-offset)
                   (x-zoom zoom-x-level))
      task-view
    (let* ((pane-task-length (local-time:timestamp-difference end start))
           (pane-width (rectangle-width (sheet-region pane)))
           (pane-unit (/ pane-task-length pane-width))
           (family :sans-serif)
           (face :bold)
           (size *default-text-size*)
           (timeline-style (make-text-style family face size)))
      (let* ((zoom-task-days (/ (/ pane-task-length local-time:+seconds-per-day+) x-zoom))
             (zoom-task-weeks (/ zoom-task-days local-time:+days-per-week+))
             (zoom-task-rough-months (/ zoom-task-days 30))
             (zoom-task-rough-years (/ zoom-task-days 365))
             (zoom-unit (/ pane-unit x-zoom local-time:+seconds-per-day+)))
        (declare (ignore zoom-task-weeks zoom-task-rough-months zoom-task-rough-years))
        (cond ((> zoom-unit 0.50)
               (draw-timeline-years pane task-view timeline-style))
              ((> zoom-unit 0.25)
               (draw-timeline-quarters pane task-view timeline-style))
              (t
               ;; (> zoom-unit 0.05)
               (draw-timeline-months pane task-view timeline-style))
              ;; by week or day?
              #+(or)
              (t
               (draw-rectangle* pane
                                0 (task-view-y-offset task-view)
                                100 (+ y 40)
                                :ink +green+ :filled t)))))))

(defun draw-today-highlight (pane task-view &key top bottom)
  (with-accessors ((start task-view-start)
                   (end task-view-end))
      task-view
    (let*  ((pane-task-length (local-time:timestamp-difference end start))
            (pane-width (rectangle-width (sheet-region pane)))
            (pane-height (rectangle-height (sheet-region pane)))
            (pane-unit (/ pane-task-length pane-width))
            (x-zoom (zoom-x-level task-view))
            (start-to-today (local-time:timestamp-difference (local-time:today) start))
            (today-coord (* x-zoom (/ start-to-today pane-unit)))
            (start-to-tomorrow (local-time:timestamp-difference
                                (local-time:adjust-timestamp
                                    (local-time:today)
                                  (:offset :day 1))
                                start))
            (tomorrow-coord (* x-zoom (/ start-to-tomorrow pane-unit))))
      (declare (ignore pane-height))
      (with-bounding-rectangle* (x1 y1 x2 y2)
          (stream-current-output-record pane)
        (declare (ignore x1 x2))
        (draw-rectangle* pane
                         today-coord
                         (or top y1)
                         (max (+ today-coord 4) tomorrow-coord)
                         (or bottom y2)
                         :ink +red+
                         :filled nil)))))

(define-presentation-method present (task (type chart-top-level-task) pane
                                          (task-view task-chart-view) &key)
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
      (multiple-value-bind (width height)
          (text-size pane str :text-style style)
        (declare (ignore width))
        (with-bounding-rectangle* (x1 y1 x2 y2)
            pane
          (declare (ignore y1 y2))
          (draw-text* pane str
                      (+ x1 (/ (- x2 x1) 2)) 4
                      :align-x :center
                      :align-y :top :text-style style
                      :ink *task-text-color*))
        (incf task-view-y-offset height)))
    (when (not task-view-hide-cost)
      (let ((cost (cost task)))
        (when cost
          (let* ((str (format nil "Remaining Cost: ~A"
                              (cl-l10n:format-number/currency nil cost 'ldml:usd)))
                 (family :sans-serif)
                 (face :roman)
                 (size :normal)
                 (style (make-text-style family face size)))
            (multiple-value-bind (width height)
                (text-size pane str :text-style style)
              (declare (ignore width))
              (with-bounding-rectangle* (x1 y1 x2 y2)
                  pane
                (declare (ignore y1 y2))
                (draw-text* pane str
                            (+ x1 (/ (- x2 x1) 2))
                            task-view-y-offset
                            :align-x :center
                            :align-y :top :text-style style
                            :ink *task-text-color*))
              (incf task-view-y-offset (+ 12 height))))))
      (let ((total-cost (cost task :include-finished t)))
        (when (and (not task-view-hide-cost) total-cost)
          (let* ((str (format nil "Total Cost: ~A"
                              (cl-l10n:format-number/currency nil total-cost 'ldml:usd)))
                 (family :sans-serif)
                 (face :roman)
                 (size :normal)
                 (style (make-text-style family face size)))
            (multiple-value-bind (width height)
                (text-size pane str :text-style style)
              (declare (ignore width))
              (with-bounding-rectangle* (x1 y1 x2 y2)
                  pane
                (declare (ignore y1 y2))
                (draw-text* pane str
                            (+ x1 (/ (- x2 x1) 2))
                            task-view-y-offset
                            :align-x :center
                            :align-y :top
                            :text-style style
                            :ink *task-text-color*))
              (incf task-view-y-offset (+ 12 height)))))))
    ;; draw timeline on top
    (draw-timeline pane task-view)
    (incf task-view-y-offset 60)

    (let ((today-top task-view-y-offset))
      ;; now draw the child tasks
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
                   (let ((task-record
                          (with-output-to-output-record (pane)
                            (present child 'chart-task :record-type 'chart-task-presentation :stream pane))))
                     (incf task-view-y-offset 24)
                     (let ((background-record
                            (with-output-to-output-record (pane)
                              (let ((row-color (mod-elt *task-background-colors* task-group-counter))
                                    (task-padding 2))
                                (with-bounding-rectangle* (x1 y1 x2 y2)
                                    task-record
                                  (draw-rectangle* pane
                                                   0 y1
                                                   (max (+ end-coord 18) (+ x2 3)) (+ y2 task-padding)
                                                   :ink row-color)
                                  (draw-rectangle* pane x1 (- y1 3) (+ x2 3) (+ y2 3)
                                                    :ink +gray50+
                                                    :filled nil
                                                    :line-thickness 2))))))
                       (add-output-record task-record background-record)
                       (stream-add-output-record pane background-record))))))))

      ;; draw a red rectangle around the current date
      (draw-today-highlight pane task-view :top (- today-top 4) :bottom (- task-view-y-offset 8)))

    ;; draw timeline on bottom
    (incf task-view-y-offset 4)
    (draw-timeline pane task-view)))
