
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
          (stream-default-view pane) task-view)))

(defmethod shared-initialize :after ((pane gantlet-pane) slot-names &key)
  (let ((task (pane-task pane)))
    (when task
      (set-pane-task pane task))))

(defmethod note-space-requirements-changed ((pane gantlet-pane) child)
  (break)
  (setf (pane-needs-redisplay pane) t))

(defun redraw (frame pane)
  (window-clear pane)
  (display-gantlet frame pane)
  (clim:redisplay-frame-pane frame pane))

(defun zoom-x-callback (gadget scale)
  (let ((frame (pane-frame gadget)))
    (let ((pane (find-pane-named frame 'gantlet)))
      (setf (zoom-x-level pane) scale)
      (setf (pane-needs-redisplay pane) t)
      (redraw *application-frame* pane)
      (repaint-sheet pane +everywhere+))))

(defun zoom-y-callback (gadget scale)
  (let ((frame (pane-frame gadget)))
    (let ((pane (find-pane-named frame 'gantlet)))
      (setf (zoom-y-level pane) scale)
      (setf (pane-needs-redisplay pane) t)
      (redraw *application-frame* pane)
      (repaint-sheet pane +everywhere+))))

(define-application-frame gantlet-app ()
  ()
  (:menu-bar menubar-command-table)
  (:panes
   (gantlet gantlet-pane
            :display-function 'display-gantlet
            #| :default-view +gantt-chart-view+ |#
            :display-time :command)
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
        :width 600))
  (:layouts
   (default (vertically ()
              (horizontally ()
                  (scrolling ()
                    gantlet)
                  (labelling (:label "Zoom Y")
                    zoom-y))
              (labelling (:label "Zoom X")
                zoom-x)
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
   (border-color :initarg :border-color :accessor task-glyph-border-color)))

(define-presentation-method present (task-glyph (type task-glyph) pane
                                                (task-view task-view) &key)
  (let* ((label-left-margin 6)
         (label-height 16)
         (label-size :large)
         (family nil)
         (face :bold)
         (style (make-text-style family face label-size)))
    (with-accessors ((task task-glyph-task)
                     (x1 task-glyph-x1)
                     (y1 task-glyph-y1)
                     (x2 task-glyph-x2)
                     (y2 task-glyph-y2)
                     (fill-color task-glyph-fill-color)
                     (border-color task-glyph-border-color))
        task-glyph
      (let* ((ht (task-view-exapanded-task-hash-table task-view))
             (expanded (gethash task ht)))
        (when expanded (incf y2 24))
        (draw-rectangle* pane
                         x1 y1 x2 y2
                         :ink fill-color)
        (draw-rectangle* pane
                         x1 y1 x2 y2
                         :ink border-color
                         :filled nil
                         :line-thickness 4)
        (draw-text* pane
                    (gantt::name task)
                    (+ x1 label-left-margin)
                    (+ y1 label-height)
                    :ink +black+
                    :text-size label-size
                    :text-style style)))))

(define-presentation-method present (task (type task) pane
                                          (task-view task-view) &key)
  (let* ((start (task-view-start task-view))
         (end (task-view-end task-view))
         (pane-task-length (local-time:timestamp-difference end start))
         (pane-width (rectangle-width (sheet-region pane)))
         (pane-unit (/ pane-task-length pane-width))
         (task-height 16)
         (task-padding 12)
         (bottom-margin 6)
         (task-counter 0)
         (y-offset 20)
         (x-zoom (zoom-x-level pane))
         (y-zoom (zoom-y-level pane)))
    (labels ((draw-task (task)
               (let* ((task-start (or (start task) start))
                      (task-end (or (end task) end)))
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
                                            :border-color (elt *task-border-colors* (mod task-counter (length *task-border-colors*))))))
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

(make-command-table 'file-command-table
		    :errorp nil
		    :menu '(("Quit" :command com-quit)))

(make-command-table 'gantt-command-table
		    :errorp nil
		    :menu '(("Redraw" :command com-redraw)))

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

(defmethod handle-event ((pane gantlet-pane) (event pointer-button-release-event))
  #+(or)
  (break))

(defvar *gantlet-application*)

(defun gantlet-main ()
  (let ((frame (setf *gantlet-application* (make-application-frame 'gantlet-app))))
    (values frame
            (bt:make-thread
             (lambda ()
               (run-frame-top-level frame))))))

