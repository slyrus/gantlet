
(defpackage #:gantlet
  (:use #:clim #:clim-lisp #:clim-extensions #:gantt))

(in-package #:gantlet)

(defclass task-view ()
  ((task :initarg :task :accessor task-view-task)
   (start :initarg :start :accessor task-view-start)
   (end :initarg :end :accessor task-view-end)))

(defclass gantlet-pane (application-pane table-pane)
  ((task :initform nil :accessor pane-task)
   (task-view :initform :nil :accessor pane-task-view)
   (zoom-x-level :initform 1.0d0 :accessor zoom-x-level)
   (zoom-y-level :initform 1.0d0 :accessor zoom-y-level)))

(defun set-pane-task (pane task)
  (setf (pane-task pane) task)
  (setf (pane-task-view pane)
          (make-instance 'task-view
                         :task task
                         :start (start task)
                         :end (end task))))

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
           :orientation :horizontal
           :drag-callback 'zoom-y-callback
           :value-changed-callback 'zoom-y-callback
           :min-height 24 :max-height 24)
   (int :interactor
        :height 200
        :max-height 200
        :width 600))
  (:layouts
   (default (vertically ()
              (scrolling ()
                gantlet)
              (vertically ()
                (labelling (:label "Zoom X")
                  zoom-x)
                (labelling (:label "Zoom Y")
                  zoom-y))
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

(defun display-gantlet (frame pane)
  (declare (ignore frame))
  (let* ((task (pane-task pane)))
    (when task
      (let* ((task-view (pane-task-view pane))
             (start (task-view-start task-view))
             (end (task-view-end task-view))
             (pane-task-length (local-time:timestamp-difference end start))
             (pane-width (rectangle-width (sheet-region pane)))
             (pane-unit (/ pane-task-length pane-width))
             (task-height 16)
             (task-padding 12)
             (bottom-margin 6)
             (task-name-size :large)
             (family nil)
             (face :bold)
             (style (make-text-style family face task-name-size))
             (task-counter 0)
             (y-offset 20)
             (x-zoom (zoom-x-level pane))
             (y-zoom (zoom-y-level pane)))
        (labels ((draw-task (task)
                   (let* ((task-start (or (start task) start))
                          (task-end (or (end task) end)))
                     (let ((xstart (/ (local-time:timestamp-difference task-start start) pane-unit))
                           (xend (/ (local-time:timestamp-difference task-end start) pane-unit))
                           (medium (sheet-medium pane))
                           (str (gantt::name task)))
                       (multiple-value-bind (left top right bottom)
                           (climi::text-bounding-rectangle* medium str :text-style style)
                         (declare (ignore left right))
                         (draw-rectangle* pane
                                          (* x-zoom (max 0 xstart))
                                          (* y-zoom y-offset)
                                          (* x-zoom (min xend pane-width))
                                          (+ (* y-zoom (+ y-offset task-height)) bottom-margin)
                                          :ink (elt *task-colors* (mod task-counter (length *task-colors*))))
                         (draw-rectangle* pane
                                          (* x-zoom (max 0 xstart))
                                          (* y-zoom y-offset)
                                          (* x-zoom (min xend pane-width))
                                          (+ (* y-zoom (+ y-offset task-height)) bottom-margin)
                                          :ink (elt *task-border-colors* (mod task-counter
                                                                              (length *task-border-colors*)))
                                          :filled nil
                                          :line-thickness 4)
                         (draw-text* pane
                                     str
                                     (max (+ (* x-zoom xstart) 6) 0)
                                     (+ (* y-zoom y-offset) task-height)
                                     :ink +black+
                                     :text-size task-name-size
                                     :text-style style)
                         (incf y-offset (+ (- bottom top) task-padding))
                         (incf task-counter))))
                   (loop for child across (gantt::children task)
                      do (draw-task child))))
          (draw-task task))))))

(define-gantlet-app-command (com-quit :name t :keystroke (#\q :meta)) ()
  (frame-exit *application-frame*))

(define-gantlet-app-command (com-redraw :name t) ()
  (print *application-frame* *debug-io*)
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

(defvar *gantlet-application*)

(defun gantlet-main ()
  (let ((frame (setf *gantlet-application* (make-application-frame 'gantlet-app))))
    (values frame
            (bt:make-thread
             (lambda ()
               (run-frame-top-level frame))))))

