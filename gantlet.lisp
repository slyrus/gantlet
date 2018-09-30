
(defpackage #:gantlet
  (:use #:clim #:clim-lisp #:clim-extensions #:gantt))

(in-package #:gantlet)

(defclass task-view (view)
  ((task :initarg :task :accessor task-view-task)
   (start :initarg :start :accessor task-view-start)
   (end :initarg :end :accessor task-view-end)
   (expanded-task-hash-table :accessor task-view-exapanded-task-hash-table :initform (make-hash-table))))

(defclass gantlet-pane (application-pane table-pane)
  ((task :initform nil :accessor pane-task)
   (task-view :initform :nil :accessor pane-task-view)
   (zoom-x-level :initform 1.0d0 :accessor zoom-x-level)
   (zoom-y-level :initform 1.0d0 :accessor zoom-y-level)))

(defun set-pane-task (pane task)
  (setf (pane-task pane) task)
  (let ((task-view (make-instance 'task-view
                                  :task task
                                  :start (start task)
                                  :end (end task))))
    (setf (pane-task-view pane) task-view
          (stream-default-view pane) task-view))
  (let ((resource-list (find-pane-named (pane-frame pane) 'resource-list)))
    (let ((resources (task-resources task)))
      (setf (climi::visible-items resource-list) (length resources))
      (setf (climi::list-pane-items resource-list :invoke-callback nil) (mapcar #'gantt::name resources))))
  (window-refresh pane)
  (pane-needs-redisplay pane)

  (repaint-sheet pane +everywhere+))

(defmethod shared-initialize :after ((pane gantlet-pane) slot-names &key)
  (let ((task (pane-task pane)))
    (when task
      (set-pane-task pane task))))

(defun redraw (frame pane)
  (window-clear pane)
  (display-gantlet frame pane)
  (clim:redisplay-frame-pane frame pane))

(defun redisplay (frame pane)
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
      (setf (zoom-x-level pane) scale)
      (setf (pane-needs-redisplay pane) t)

      ;; 1. find current viewport center
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
            (scroll-extent pane new-x-pos new-y1))))

      (redraw *application-frame* pane)
      (setf (pane-needs-redisplay pane) t))))

(defun zoom-y-callback (gadget scale)
  (let ((frame (pane-frame gadget)))
    (let ((pane (find-pane-named frame 'gantlet)))
      (setf (zoom-y-level pane) scale)
      (setf (pane-needs-redisplay pane) t)
      (redraw *application-frame* pane)
      #+nil (repaint-sheet pane +everywhere+))))

(define-application-frame gantlet-app ()
  ()
  (:menu-bar menubar-command-table)
  (:panes
   (gantlet gantlet-pane
            :display-function 'display-gantlet
            #| :default-view +gantt-chart-view+ |#
            :display-time :command)
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
        :width 800))
  (:layouts
   (default (vertically ()
              (horizontally ()
                (4/5 (vertically ()
                        (horizontally ()
                          (scrolling ()
                            gantlet)
                          (labelling (:label "Zoom Y")
                            zoom-y))
                        (labelling (:label "Zoom X")
                          zoom-x)))
                (1/5 (labelling (:label "Resources")
                       resource-list)))
              int))))

(defparameter *task-border-colors*
  (list (make-rgb-color 0.5 0.5 1.0)
        (make-rgb-color 0.5 1.0 0.5)
        (make-rgb-color 1.0 0.5 0.5)
        (make-rgb-color 0.5 0.75 0.5)
        (make-rgb-color 0.5 0.75 0.75)
        (make-rgb-color 0.5 0.5 0.75)))

(defparameter *task-colors*
  (mapcar (lambda (color)
            (multiple-value-bind (r g b)
                (color-rgb color)
              (let ((lightness 0.6))
                (make-rgb-color
                 (+ r (* (- 1 r) lightness))
                 (+ g (* (- 1 g) lightness))
                 (+ b (* (- 1 b) lightness))))))
          *task-border-colors*))

(defclass task-glyph ()
  ((task :initarg :task :accessor task-glyph-task)
   (task-view :initarg :task-view :accessor task-glyph-task-view)
   (x1 :initarg :x1 :accessor task-glyph-x1)
   (y1 :initarg :y1 :accessor task-glyph-y1)
   (x2 :initarg :x2 :accessor task-glyph-x2)
   (y2 :initarg :y2 :accessor task-glyph-y2)
   (fill-color :initarg :fill-color :accessor task-glyph-fill-color)
   (border-color :initarg :border-color :accessor task-glyph-border-color)
   (expanded :initarg :expanded :initform nil :accessor task-glyph-expanded)))

(defun date-string (date)
  (local-time:format-timestring nil date :format local-time:+iso-8601-date-format+))

(define-presentation-method present (task-glyph (type task-glyph) pane
                                                (task-view task-view) &key)
  (let* ((text-left-margin 6)
         (text-top-margin 4)
         (name-size :large)
         (family nil)
         (face :bold)
         (style (make-text-style family face name-size)))
    (with-accessors ((task task-glyph-task)
                     (x1 task-glyph-x1)
                     (y1 task-glyph-y1)
                     (x2 task-glyph-x2)
                     (y2 task-glyph-y2)
                     (fill-color task-glyph-fill-color)
                     (border-color task-glyph-border-color)
                     (expanded task-glyph-expanded))
        task-glyph

      ;; first we need to compute text sizes, then we can draw the
      ;; boxes, and then, finally, the strings that go in the boxes.
      (let ((name (gantt::name task))
            (text-list))
        (flet ((add-text (str)
                 (multiple-value-bind (width height final-x final-y baseline)
	             (text-size pane str :text-style style)
                   (declare (ignore final-x final-y baseline))
                   (push (list str width (+ text-top-margin height)) text-list))))
          (add-text name)
          (when expanded
            (alexandria:when-let ((start-date (start task)))
              (add-text (format nil "Start: ~A" (date-string start-date))))
            (alexandria:when-let ((end-date (end task)))
              (add-text (format nil "End: ~A" (date-string end-date))))
            (let ((resources (task-resources task)))
              (loop for resource in resources
                 do
                   (add-text (format nil "Resource: ~A" (gantt::name resource))))))
          (let ((height (apply #'+ (mapcar #'third text-list)))
                (max-width (apply #'max (mapcar #'second text-list))))
            (incf y2 height)
            (draw-rectangle* pane x1 y1 x2 y2 :ink fill-color)
            (draw-rectangle* pane x1 y1 x2 y2
                             :ink border-color :filled nil :line-thickness 4)
            (let ((text-x-offset (if (> max-width (- x2 x1))
                                     (+ x2 text-left-margin)
                                     (+ x1 text-left-margin))))
              (loop for (text width height) in (reverse text-list)
                 with y-offset = y1
                 do
                   (draw-text* pane text
                               text-x-offset
                               (+ y-offset text-top-margin)
                               :align-y :top
                               :ink +black+
                               :text-size name-size
                               :text-style style)
                   (incf y-offset height)))))))))

(define-presentation-method present (task (type task) pane
                                          (task-view task-view) &key)


  (let ((gantlet-pane (find-pane-named *application-frame* 'gantlet)))
    (with-bounding-rectangle* (viewport-x1 viewport-y1 viewport-x2 viewport-y2)
        (pane-viewport-region gantlet-pane)
      (with-bounding-rectangle* (pane-x1 pane-y1 pane-x2 pane-y2)
          gantlet-pane
        (with-bounding-rectangle* (sheet-x1 sheet-y1 sheet-x2 sheet-y2)
            (sheet-region gantlet-pane)
          (draw-text* pane (format nil "pane ~A ~A ~A ~A" pane-x1 pane-y1 pane-x2 pane-y2) 20 20)
          (draw-text* pane (format nil "viewport ~A ~A ~A ~A" viewport-x1 viewport-y1 viewport-x2 viewport-y2) 220 20)
          (draw-text* pane (format nil "sheet ~A ~A ~A ~A" sheet-x1 sheet-y1 sheet-x2 sheet-y2) 420 20)))))

  (let* ((start (task-view-start task-view))
         (end (task-view-end task-view))
         (pane-task-length (local-time:timestamp-difference end start))
         (pane-width (rectangle-width (sheet-region pane)))
         (pane-unit (/ pane-task-length pane-width))
         (task-height 16)
         (task-padding 12)
         (bottom-margin 6)
         (task-counter 0)
         (y-offset 40)
         (x-zoom (zoom-x-level pane))
         (y-zoom (zoom-y-level pane)))
    (labels ((draw-task (task)
               (let* ((task-start (or (start task) start))
                      (task-end (or (end task) end))
                      (ht (task-view-exapanded-task-hash-table task-view))
                      (expanded (gethash task ht)))
                 (let ((xstart (/ (local-time:timestamp-difference task-start start) pane-unit))
                       (xend (/ (local-time:timestamp-difference task-end start) pane-unit)))
                   (let ((tg (make-instance 'task-glyph
                                            :task task
                                            :task-view task-view
                                            :x1 (* x-zoom (max 0 xstart))
                                            :y1 y-offset
                                            :x2 (* x-zoom (min xend pane-width))
                                            :y2 (+ y-offset (* y-zoom task-height) bottom-margin)
                                            :fill-color (elt *task-colors* (mod task-counter (length *task-colors*)))
                                            :border-color (elt *task-border-colors* (mod task-counter (length *task-border-colors*)))
                                            :expanded expanded)))
                     (with-bounding-rectangle* (x1 y1 x2 y2)
                         (present tg 'task-glyph)
                       (declare (ignore x1 x2))
                       (incf y-offset (+ (- y2 y1) task-padding))))
                   (incf task-counter)))
               (loop for child across (gantt::children task)
                  do (draw-task child))))
      (draw-task task))))

(defun display-gantlet (frame pane)
  (declare (ignore frame))
  (let* ((task (pane-task pane)))
    (when task
      (present task 'task))))

(define-gantlet-app-command (com-quit :name t :keystroke (#\q :meta)) ()
  (frame-exit *application-frame*))

(define-gantlet-app-command (com-redraw :name t) ()
  (let ((gantlet-pane (find-pane-named *application-frame* 'gantlet)))
    (when gantlet-pane
      (redraw *application-frame* gantlet-pane)
      #+nil
      (let ((region (or (pane-viewport-region gantlet-pane)
                        (sheet-region gantlet-pane))))
        (when region (handle-repaint gantlet-pane region))))))

(define-gantlet-app-command (com-redisplay :name t) ()
  (let ((gantlet-pane (find-pane-named *application-frame* 'gantlet)))
    (when gantlet-pane
      (redisplay *application-frame* gantlet-pane)
      (let ((view-bounds (true-viewport-region gantlet-pane)))
        (with-bounding-rectangle* (x1 y1 x2 y2)
            view-bounds
          (draw-rectangle* gantlet-pane
                           (+ x1 5) (+ y1 5)
                           (- x2 5) (- y2 5)
                           :ink +red+
                           :filled nil)))
      )))

(make-command-table 'file-command-table
		    :errorp nil
		    :menu '(("Quit" :command com-quit)))

(make-command-table 'gantt-command-table
		    :errorp nil
		    :menu '(("Redraw" :command com-redraw)
                            ("Redisplay" :command com-redisplay)))

(make-command-table 'menubar-command-table
		    :errorp nil
		    :menu '(("File" :menu file-command-table)
                            ("Gantt" :menu gantt-command-table)))

(define-gantlet-app-command (com-show-task-glyph-info :name t :menu t
                                                      :keystroke (#\i :meta))
    ((task-glyph task-glyph))
  (let* ((task (task-glyph-task task-glyph))
         (task-view (task-glyph-task-view task-glyph))
         (ht (task-view-exapanded-task-hash-table task-view))
         (expanded (gethash task ht)))
    (if expanded
        (setf (gethash task ht) nil)
        (setf (gethash task ht) t))
    (let ((gantlet-pane (find-pane-named *application-frame* 'gantlet)))
      (when gantlet-pane
        (redraw *application-frame* gantlet-pane)))

    #+nil
    (notify-user *application-frame*
	         (format nil "~A."
		         (gantt::name task))
	         :title (format nil "Information on ~A" (gantt::name task))
	         :text-style '(:serif :roman 15))))

(define-presentation-to-command-translator task-glyph-info
    (task-glyph com-show-task-glyph-info gantlet-app
          :gesture :select
          :documentation "Show info for this task.")
    (object)
  (list object))

(defvar *gantlet-application*)

(defun gantlet-main ()
  (let ((frame (setf *gantlet-application* (make-application-frame 'gantlet-app))))
    (values frame
            (bt:make-thread
             (lambda ()
               (run-frame-top-level frame))))))


(defmethod true-viewport-region ((pane gantlet-pane))
  (untransform-region (sheet-native-transformation pane)
                    (sheet-native-region pane)))

(defmethod clim:redisplay-frame-panes :after
    ((frame gantlet-app) &key force-p)
  (declare (ignore force-p))
  (let ((pane (clim:find-pane-named clim:*application-frame* 'gantlet)))
    (clim:replay (clim:stream-output-history pane) pane)
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
      #+nil
      (clim:draw-rectangle* pane 0 0 1000 1000
                            :filled t
                            :ink clim:+background-ink+))))
