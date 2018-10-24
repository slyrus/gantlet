
(in-package #:gantlet)

(defclass task-output-record (standard-presentation)
  ((task :initarg :task :accessor task)))

(defparameter *task-border-colors-rgb*
  (list (make-rgb-color 0.5 0.5 1.0)
        (make-rgb-color 0.5 1.0 0.5)
        (make-rgb-color 1.0 0.5 0.5)
        (make-rgb-color 0.5 0.75 0.5)
        (make-rgb-color 0.5 0.75 0.75)
        (make-rgb-color 0.5 0.5 0.75)))

(defparameter *task-border-colors*
  (list (make-rgb-color 0.6 0.6 0.6)
        (make-rgb-color 0.5 0.5 0.5)))

(defparameter *task-background-colors*
  (list (make-rgb-color 0.93 0.93 0.93)
        (make-rgb-color 0.96 0.96 0.96)))

(defparameter *critical-task-color*
  nil
  #+(or) (make-rgb-color 1 0.5 0.5))

(defparameter *complete-task-color*
  (make-rgb-color 0.5 0.5 1))

(defparameter *critical-task-border-color*
  (make-rgb-color 1 0.0 0.0))

(defun lighten-color (color lightness)
  (multiple-value-bind (r g b)
      (color-rgb color)
    (make-rgb-color
     (+ r (* (- 1 r) lightness))
     (+ g (* (- 1 g) lightness))
     (+ b (* (- 1 b) lightness)))))

(defparameter *task-colors*
  (mapcar (lambda (color)
            (lighten-color color 0.6))
          *task-border-colors*))

(defun date-string (date)
  (local-time:format-timestring nil date :format local-time:+iso-8601-date-format+))

;;
;;
;; Presentations

;;
;; task-output-record - nothing exciting happens here now
(define-presentation-type task-output-record ())

;;
;; top-level-task -- use this handle the backgrounds, etc...
(defclass top-level-task (standard-presentation)
  ())

(define-presentation-type top-level-task ()
  :inherit-from 'task)

(defmethod output-record-refined-position-test ((record top-level-task) x y)
  nil)

;;
;; mute-presentation - children of this tree are not highlighted
(defclass mute-presentation (standard-presentation)
  ())

(define-presentation-type mute-presentation ())

(defmethod highlight-output-record-tree ((record mute-presentation) stream state)
  nil)


;; task-group presentation
(define-presentation-type task-group ())

;;
;; actual drawing routines
(defun draw-task (task pane x1 y1 x2 y2
                  &key fill-color
                       border-color
                       (expanded t))
  (let* ((text-left-margin 6)
         (text-top-margin 2)
         (text-size :large)
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
                     (cost cost))
        task
      (let (text-list)
        (flet ((add-text (str)
                 (multiple-value-bind (width height final-x final-y baseline)
	             (text-size pane str :text-style style)
                   (declare (ignore final-x final-y baseline))
                   (push (list str width (+ text-top-margin height)) text-list))))
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
                 'task
                 :record-type 'task-output-record)
              (climi::invoke-surrounding-output-with-border
	       pane
               (lambda (pane)
                 (draw-rectangle* pane x1 y1 x2 y2 :ink fill-color))
               :shape :rectangle
               :filled t
               :ink border-color
               :outline-ink border-color
               :line-thickness 3
               :padding-left 2
               :padding-right 0
               :padding-top 1
               :padding-bottom 0)
              (with-output-as-presentation
                  (pane nil 'mute-presentation :record-type 'mute-presentation)
                (when progress
                  (draw-rectangle* pane
                                   x1
                                   y1
                                   (+ (* (- x2 x1) progress) x1)
                                   (+ y1 5)
                                   :ink *complete-task-color* :filled t)))
              (let ((text-x-offset (if (> max-width (- x2 x1 7))
                                       (+ x2 text-left-margin 2)
                                       (+ x1 text-left-margin)))
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
                                           (+ y-offset (/ (- y2 y1) 2)))
                                       :align-y (if expanded :top :center)
                                       :ink +black+
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

(defun mod-elt (sequence index)
  (elt sequence (mod index (length sequence))))

(define-presentation-method present (task (type task) pane
                                          (task-view task-view) &key)
  (with-accessors ((start task-view-start)
                   (end task-view-end)
                   (show-task-info-hash-table task-view-show-task-info-hash-table)
                   (hide-task-children-hash-table task-view-hide-task-children-hash-table)
                   (y-offset task-view-y-offset)
                   (x-offset task-view-x-offset)
                   (task-counter task-view-task-counter)
                   (hide-completed-tasks task-view-hide-completed-tasks)
                   (hide-non-critical-tasks task-view-hide-non-critical-tasks))
          task-view
    (let* ((pane-task-length (local-time:timestamp-difference end start))
           (pane-width (rectangle-width (sheet-region pane)))
           (pane-unit (/ pane-task-length pane-width))
           (task-height 6)
           (task-padding 1)
           (bottom-margin 1)
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
                      'task-group)
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
                        do (let ((child-end (end child))
                                 (child-progress (task-progress child))
                                 (child-critical (gantt:task-critical child)))
                             (unless (or
                                      (and hide-non-critical-tasks
                                           (not (or child-critical
                                                    (find-task t child :key #'gantt:task-critical))))
                                      (and hide-completed-tasks
                                              child-progress
                                              (>= child-progress 1.0)))
                               (unless (and child-end
                                            (local-time:timestamp< child-end start)))
                               (present child 'task))))))))
            presentation))))))

(defun draw-timeline (pane task-view)
  (with-accessors ((start task-view-start)
                   (end task-view-end)
                   (y task-view-y-offset))
      task-view
    (let* ((pane-task-length (local-time:timestamp-difference end start))
           (pane-width (rectangle-width (sheet-region pane)))
           (pane-unit (/ pane-task-length pane-width))
           (x-zoom (zoom-x-level task-view))
           (family :sans-serif)
           (face :bold)
           (size :large)
           (timeline-style (make-text-style family face size)))
      (let* ((days (/ pane-task-length local-time:+seconds-per-day+))
             (weeks (/ days local-time:+days-per-week+))
             (rough-months (/ days 30))
             (rough-years (/ days 365)))
        (cond (;; here we should show things by year and maybe quarter
               (> rough-years 3))
              ((> rough-months 24)
               (draw-rectangle* pane
                                0 (task-view-y-offset task-view)
                                100 (+ y 40)
                                :ink +red+ :filled t))
              (;; here we'll show things by month
               (> weeks 8)
               (multiple-value-bind (dyear dmonth)
                   (time-interval::timestamp-decoded-difference end start)
                 (let ((nmonths (+ (* dyear 12) dmonth 1)))
                   (let* ((month-starts
                           (loop for i below nmonths
                              collect
                                (local-time:with-decoded-timestamp
                                    (:year start-year :month start-month)
                                    start
                                  (local-time:adjust-timestamp
                                      (local-time:encode-timestamp 0 0 0 0 1 start-month start-year)
                                    (:offset :month (1+ i))))))
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
                              (draw-text* pane
                                          (format nil "~A ~A"
                                                  (aref local-time:+short-month-names+ month)
                                                  year)
                                          (+ (* x-zoom (elt month-coords i)) 4)
                                          (+ y 15)
                                          :align-y :top
                                          :text-style timeline-style)
                              (draw-rectangle* pane
                                               (* x-zoom (elt month-coords i))
                                               y
                                               (* x-zoom (elt month-coords (1+ i)))
                                               (+ y 30)
                                               :ink +red+
                                               :filled nil))))))))
              ;; by day?
              (t ))))))

(defun draw-today-highlight (pane task-view)
  (with-accessors ((start task-view-start)
                   (end task-view-end))
      task-view
    (let*  ((pane-task-length (local-time:timestamp-difference end start))
            (pane-width (rectangle-width (sheet-region pane)))
            (pane-height (rectangle-height (sheet-region pane)))
            (pane-unit (/ pane-task-length pane-width))
            (x-zoom (zoom-x-level task-view))
            (start-to-today (local-time:timestamp-difference (local-time:today) start))
            (today-coord (/ start-to-today pane-unit)))
      (draw-rectangle* pane
                       (* x-zoom today-coord)
                       0
                       (+ (* x-zoom today-coord) 4)
                       pane-height
                       :ink +red+
                       :filled nil))))

(define-presentation-method present (task (type top-level-task) pane
                                          (task-view task-view) &key)
  (setf (task-view-task-counter task-view) 0)
  (setf (task-view-x-offset task-view) 5)
  (setf (task-view-y-offset task-view) 26)

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
                    (+ x1 (/ (- x2 x1) 2)) 10
                    :align-x :center
                    :align-y :top :text-style style))
      (incf (task-view-y-offset task-view) height)))
  
  (with-accessors ((cost cost))
      task
    (with-accessors ((task-view-y-offset task-view-y-offset))
      task-view
      (let* ((str (format nil "Remaining Cost: ~A" (cl-l10n:format-number/currency nil cost 'ldml:usd)))
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
                        :align-y :top :text-style style))
          (incf (task-view-y-offset task-view) (+ 12 height))))))

  (with-accessors ((task-view-y-offset task-view-y-offset))
      task-view
    (let* ((str (format nil "Total Cost: ~A" (cl-l10n:format-number/currency nil (cost task :include-finished t) 'ldml:usd)))
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
                      :align-y :top :text-style style))
        (incf (task-view-y-offset task-view) (+ 12 height)))))

  ;; draw timeline
    
  (draw-timeline pane task-view)

  (incf (task-view-y-offset task-view) 65)

  (loop for child across (gantt:task-children task)
     for task-group-counter from 0
     do (let ((task-record
               (with-output-to-output-record (pane)
                 (present child 'task))))
          (let ((background-record
                 (with-output-to-output-record (pane)
                   (let ((row-color (mod-elt *task-background-colors* task-group-counter))
                         (task-padding 4))
                     (with-bounding-rectangle* (x1 y1 x2 y2)
                         task-record
                       (with-bounding-rectangle* (px1 py1 px2 py2)
                           pane
                         (declare (ignore py1 py2))
                         (draw-rectangle* pane px1 y1 px2 (+ y2 task-padding) :ink row-color))
                       #+(or)
                       (draw-rounded-rectangle* pane x1 y1 (+ x2 3) (+ y2 3)
                                                :ink (lighten-color row-color 0.4)
                                                :filled t)
                       (draw-rounded-rectangle* pane x1 y1 (+ x2 3) (+ y2 3)
                                                :ink +gray50+
                                                :filled nil
                                                :line-thickness 2))))))
            (add-output-record task-record background-record)
            (stream-add-output-record pane background-record))))

  (draw-timeline pane task-view)

  (draw-today-highlight pane task-view))
