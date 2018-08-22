
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

(defmethod note-space-requirements-changed ((pane gantlet-pane) child))

(define-application-frame gantlet-app ()
  ()
  (:menu-bar menubar-command-table)
  (:panes
   (app gantlet-pane
        :height 1024 :width 1024
        :display-function 'display-gantlet)
   (int :interactor :height 200 :width 600))
  (:layouts
   (default (vertically ()
              (scrolling ()
                app)
              int))))

(defparameter *task-colors*
  (list (make-rgb-color 0.5 0.5 1.0)
        (make-rgb-color 0.5 1.0 0.5)
        (make-rgb-color 1.0 0.5 0.5)
        (make-rgb-color 0.5 0.75 0.5)
        (make-rgb-color 0.5 0.75 0.75)
        (make-rgb-color 0.5 0.5 0.75)))

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
             (task-padding 8)
             (bottom-margin 6)
             (task-name-size :large)
             (family nil)
             (face :bold)
             (style (make-text-style family face task-name-size))
             (task-counter 0)
             (y-offset 20))
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
                                          (max 0 xstart)
                                          y-offset
                                          (min xend pane-width)
                                          (+ y-offset task-height bottom-margin)
                                          :ink (elt *task-colors* (mod task-counter (length *task-colors*))))
                         (draw-text* pane
                                     str
                                     (max (+ xstart 6) 0)
                                     (+ y-offset task-height)
                                     :ink +black+
                                     :text-size task-name-size
                                     :text-style style)
                         (incf y-offset (+ (- bottom top) task-padding))
                         (incf task-counter))))
                   (loop for child across (gantt::children task)
                      do (draw-task child))))
          (draw-task task))))))

(defun gantlet-main ()
  (let ((frame (make-application-frame 'gantlet-app)))
    (values frame
            (bt:make-thread
             (lambda ()
               (run-frame-top-level frame))))))

(define-gantlet-app-command (com-quit :name t :keystroke (#\q :meta)) ()
  (frame-exit *application-frame*))

(defvar *moose*)
(define-gantlet-app-command (com-redraw :name t) ()
  (print *application-frame* *debug-io*)
  (setf *moose* *application-frame*)
  (let ((gantlet-pane (find-pane-named *application-frame* 'app)))
    (when gantlet-pane
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
