
(defpackage #:gantlet
  (:use #:clim #:clim-lisp #:clim-extensions #:gantt))

(in-package #:gantlet)

(defclass task-view (view)
  ((task :initarg :task :accessor task-view-task)
   (zoom-x-level :initform 1.0d0 :accessor zoom-x-level)
   (zoom-y-level :initform 1.0d0 :accessor zoom-y-level)
   (start :initarg :start :accessor task-view-start)
   (end :initarg :end :accessor task-view-end)
   (hide-completed-tasks :initform t :initarg :hide-completed-tasks :accessor task-view-hide-completed-tasks)
   (hide-non-critical-tasks :initform nil :initarg :hide-non-critical-tasks :accessor task-view-hide-non-critical-tasks)
   (x-offset :initarg :x-offset :accessor task-view-x-offset :initform 5)
   (y-offset :initarg :y-offset :accessor task-view-y-offset :initform 0)
   (task-counter :initarg :task-counter :accessor task-view-task-counter :initform 0)
   (show-task-info-hash-table :accessor task-view-show-task-info-hash-table :initform (make-hash-table))
   (hide-task-children-hash-table :accessor task-view-hide-task-children-hash-table :initform (make-hash-table))))

(defclass gantlet-pane (application-pane table-pane)
  ((task :initform nil :accessor pane-task)))

(defun set-pane-task (pane task)
  (setf (pane-task pane) task)
  (setf (stream-default-view pane)
        (make-instance 'task-view
                       :task task
                       :start (start task)
                       :end (end task)))
  (let ((resource-list (find-pane-named (pane-frame pane) 'resource-list)))
    (let ((resources (task-resources task)))
      (setf (climi::visible-items resource-list) (length resources))
      (setf (clime:list-pane-items resource-list :invoke-callback nil) (mapcar #'name resources))))
  (window-refresh pane)
  (pane-needs-redisplay pane)
  (repaint-sheet pane +everywhere+))

(defmethod shared-initialize :after ((pane gantlet-pane) slot-names &key)
  (let ((task (pane-task pane)))
    (when task
      (set-pane-task pane task))))

(defun redraw (frame pane)
  (window-clear pane)
  (gantlet-display frame pane)
  (clim:redisplay-frame-pane frame pane))

(defun redisplay-app (frame pane)
  (clim:redisplay-frame-pane frame pane))

;; the zoom callbacks do not (yet) work as I would like. Problems:
;;
;; 1. When you zoom in the scroll bars are reset to 0, 0
;;
;; 2. There's a lot of flickering.
(defun zoom-x-callback (gadget scale)
  (declare (optimize (debug 3)))
  (let ((frame (pane-frame gadget)))
    (let ((pane (find-pane-named frame 'gantlet)))
      (let ((view (stream-default-view pane)))
        (when view
          (setf (zoom-x-level view) scale)))
      (setf (pane-needs-redisplay pane) t)

      ;; 1. find current viewport center

      (when (pane-viewport pane)
        (with-bounding-rectangle* (old-x1 old-y1 old-x2 old-y2)
            (pane-viewport-region pane)
          (declare (ignore old-x2 old-y1 old-y2))

          (redraw *application-frame* pane)
          (setf (pane-needs-redisplay pane) t)

          (with-bounding-rectangle* (new-x1 new-y1 new-x2 new-y2)
              (pane-viewport-region pane)
            (declare (ignore new-y2))
            (let ((new-x-pos (+ old-x1 (/ (- new-x2 new-x1) 2))))

              ;; 2. set the viewport center to the previous viewport center
              (scroll-extent pane new-x-pos new-y1)))))

      (redraw *application-frame* pane)
      #+nil (setf (pane-needs-redisplay pane) t))))

(defun zoom-y-callback (gadget scale)
  (let ((frame (pane-frame gadget)))
    (let ((pane (find-pane-named frame 'gantlet)))
      (let ((view (stream-default-view pane)))
        (when view
          (setf (zoom-y-level view) scale)))
      (setf (pane-needs-redisplay pane) t)
      (redraw *application-frame* pane)
      (repaint-sheet pane +everywhere+))))

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
         (text-top-margin 6)
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
               :shape :rounded
               :filled t
               :ink border-color
               :outline-ink border-color
               :line-thickness 3
               :padding-left 2
               :padding-right 1
               :padding-top 2
               :padding-bottom 1)
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
           (task-height 10)
           (task-padding 4)
           (bottom-margin 6)
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

  (draw-today-highlight pane task-view))

(defun gantlet-display (frame pane)
  (declare (ignore frame))
  (let* ((task (pane-task pane)))
    (when task
      (present task 'top-level-task))))

(define-application-frame gantlet-app ()
  ()
  (:menu-bar menubar-command-table)
  (:panes
   (gantlet gantlet-pane
            :display-function 'gantlet-display
            :display-time :command
            :height 600)
   (resource-list
    (make-pane 'list-pane
	       :value 'clim:region-intersection
	       :name-key (lambda (x) (format nil "~(~A~)" x))))
   (zoom-x :slider
           :min-value 0.1
           :max-value 10
           :decimal-places 2
           :value 1.0d0
           :show-value-p t
           :orientation :horizontal
           :drag-callback 'zoom-x-callback
           :value-changed-callback 'zoom-x-callback
           :min-height 24 :max-height 24)
   (zoom-y :slider
           :min-value 0.1
           :max-value 10
           :decimal-places 2
           :value 1.0d0
           :show-value-p t
           :orientation :vertical
           :drag-callback 'zoom-y-callback
           :value-changed-callback 'zoom-y-callback
           :min-width 24 :max-width 24)
   (int :interactor
        :height 200
        :max-height 200
        :width 1200))
  (:layouts
   (default (vertically ()
              (horizontally ()
                (5/6 (vertically ()
                        (horizontally ()
                          (scrolling ()
                            gantlet)
                          (labelling (:label "Zoom Y")
                            zoom-y))
                        (labelling (:label "Zoom X")
                          zoom-x)))
                (1/6 (labelling (:label "Resources")
                       resource-list)))
              int))))

(define-gantlet-app-command (com-quit :name t :keystroke (#\q :meta)) ()
  (frame-exit *application-frame*))

(defgeneric taskp (object)
  (:method ((object task))
    t))

;; show task info
(define-gantlet-app-command (com-show-task-info :name t :menu t
                                                :keystroke (#\i :meta))
    ((task task))
  (let* ((gantlet-pane (find-pane-named *application-frame* 'gantlet))
         (task-view (stream-default-view gantlet-pane)))
    (with-accessors ((show-task-info-hash-table task-view-show-task-info-hash-table))
        task-view
      (let ((expanded (gethash task show-task-info-hash-table)))
        (setf (gethash task show-task-info-hash-table) (not expanded))
        (redraw *application-frame* gantlet-pane)
        #+nil
        (notify-user *application-frame*
	             (format nil "~A."
		             (name task))
	             :title (format nil "Information on ~A" (name task))
	             :text-style '(:serif :roman 15))))))

(define-gesture-name show-task-info-gesture :pointer-button (:left))

(define-presentation-to-command-translator show-task-info-translator
    (task com-show-task-info gantlet-app
          :gesture show-task-info-gesture
          :menu nil
          :tester ((object presentation event context-type)
                   (declare (ignore presentation event context-type))
                   #+(or) (print context-type *debug-io*)
                   (taskp object))
          :documentation "Show info for this task.")
    (object)
  (list object))

;; hide task children
(define-gesture-name hide-task-children-gesture :pointer-button (:left :control))

(define-gantlet-app-command (com-hide-task-children :name t :menu t
                                                    :keystroke (#\h :meta))
    ((task task))
  (let* ((gantlet-pane (find-pane-named *application-frame* 'gantlet))
         (task-view (stream-default-view gantlet-pane)))
    (with-accessors ((hide-task-children-hash-table task-view-hide-task-children-hash-table))
        task-view
      (let ((expanded (gethash task hide-task-children-hash-table)))
        (setf (gethash task hide-task-children-hash-table) (not expanded))
        (redraw *application-frame* gantlet-pane)))))

(define-presentation-to-command-translator hide-task-children-translator
    (task com-hide-task-children gantlet-app
          :gesture hide-task-children-gesture
          :menu nil
          :tester ((object presentation event)
                   (declare (ignore presentation event))
                   (taskp object))
          :documentation "Hide children for this task.")
    (object)
  (list object))

(define-gantlet-app-command (com-redraw :name t) ()
  (let ((gantlet-pane (find-pane-named *application-frame* 'gantlet)))
    (when gantlet-pane
      (redraw *application-frame* gantlet-pane)
      #+nil
      (let ((region (or (pane-viewport-region gantlet-pane)
                        (sheet-region gantlet-pane))))
        (when region (handle-repaint gantlet-pane region))))))

(define-gantlet-app-command (com-hide-completed-tasks :name t) ()
  (let* ((gantlet-pane (find-pane-named *application-frame* 'gantlet))
         (task-view (stream-default-view gantlet-pane)))
    (with-accessors ((hide-completed-tasks task-view-hide-completed-tasks))
        task-view
      (setf hide-completed-tasks t)
      (redraw *application-frame* gantlet-pane))))

(define-gantlet-app-command (com-show-completed-tasks :name t) ()
  (let* ((gantlet-pane (find-pane-named *application-frame* 'gantlet))
         (task-view (stream-default-view gantlet-pane)))
    (with-accessors ((hide-completed-tasks task-view-hide-completed-tasks))
        task-view
      (setf hide-completed-tasks nil)
      (redraw *application-frame* gantlet-pane))))

(define-gantlet-app-command (com-hide-non-critical-tasks :name t) ()
  (let* ((gantlet-pane (find-pane-named *application-frame* 'gantlet))
         (task-view (stream-default-view gantlet-pane)))
    (with-accessors ((hide-non-critical-tasks task-view-hide-non-critical-tasks))
        task-view
      (setf hide-non-critical-tasks t)
      (redraw *application-frame* gantlet-pane))))

(define-gantlet-app-command (com-show-non-critical-tasks :name t) ()
  (let* ((gantlet-pane (find-pane-named *application-frame* 'gantlet))
         (task-view (stream-default-view gantlet-pane)))
    (with-accessors ((hide-non-critical-tasks task-view-hide-non-critical-tasks))
        task-view
      (setf hide-non-critical-tasks nil)
      (redraw *application-frame* gantlet-pane))))

(defmethod true-viewport-region ((pane gantlet-pane))
  (untransform-region (sheet-native-transformation pane)
                      (sheet-native-region pane)))

(define-gantlet-app-command (com-redisplay :name t) ()
  (let ((gantlet-pane (find-pane-named *application-frame* 'gantlet)))
    (when gantlet-pane
      (redisplay-app *application-frame* gantlet-pane)
      (let ((view-bounds (true-viewport-region gantlet-pane)))
        (with-bounding-rectangle* (x1 y1 x2 y2)
            view-bounds
          (draw-rectangle* gantlet-pane
                           (+ x1 5) (+ y1 5)
                           (- x2 5) (- y2 5)
                           :ink +red+
                           :filled nil))))))

(defun write-task-to-pdf-file (task file
                               &key
                                 (device-type '(1600 1000))
                                 view)
  (with-open-file (file-stream file :direction :output
                               :if-exists :supersede
                               :element-type '(unsigned-byte 8))
    (clim-pdf:with-output-to-pdf-stream
        (stream file-stream
                :header-comments '(:title (name task))
                :scale-to-fit t
                :device-type device-type)
      (setf (stream-default-view stream)
            (or view (make-instance 'task-view
                                    :task task
                                    :start (start task)
                                    :end (end task))))
      (let ((*standard-output* stream))
        (present task 'top-level-task)))))

(define-gantlet-app-command (com-write-task-to-pdf :name t)
    ((pdf-pathname pathname
                   :default *default-pathname-defaults* :insert-default t))
  (let* ((gantlet-pane (find-pane-named *application-frame* 'gantlet)))
    (with-accessors ((task pane-task)
                     (task-view stream-default-view))
        gantlet-pane
      (apply #'write-task-to-pdf-file task pdf-pathname
             :device-type (list (bounding-rectangle-width gantlet-pane)
                                (bounding-rectangle-height gantlet-pane))
             (when task-view
               `(:view ,task-view))))))

(make-command-table 'file-command-table
		    :errorp nil
		    :menu '(("Write to PDF" :command com-write-task-to-pdf)
                            ("Quit" :command com-quit)))

(make-command-table 'gantt-command-table
		    :errorp nil
		    :menu '(("Redraw" :command com-redraw)
                            ("Redisplay" :command com-redisplay)))

(make-command-table 'menubar-command-table
		    :errorp nil
		    :menu '(("File" :menu file-command-table)
                            ("Gantt" :menu gantt-command-table)))

(defvar *gantlet-application*)

(defun gantlet-main (&key (height 1200) (width 1600))
  (let ((frame (setf *gantlet-application* (make-application-frame 'gantlet-app
                                                                   :height height
                                                                   :width width))))
    (values frame
            (bt:make-thread
             (lambda ()
               (run-frame-top-level frame))))))

(defmethod clim:redisplay-frame-panes :after
    ((frame gantlet-app) &key force-p)
  (declare (ignore force-p))
  (let ((pane (clim:find-pane-named clim:*application-frame* 'gantlet)))
    (clim:replay (clim:stream-output-history pane) pane)
    #+nil
    (clim:with-output-recording-options (pane :record nil :draw t)
      (let ((view-bounds (true-viewport-region pane)))
        (with-bounding-rectangle* (x1 y1 x2 y2)
            view-bounds
          (draw-rectangle* pane
                           (+ x1 10) (+ y1 10)
                           (- x2 10) (- y2 10)
                           :ink +green+
                           :line-thickness 5
                           :filled nil)))
      (clim:draw-rectangle* pane 0 0 1000 1000
                            :filled t
                            :ink clim:+background-ink+))))

