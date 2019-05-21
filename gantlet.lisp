
(in-package #:gantlet)

;; the zoom callbacks do not (yet) work as I would like. Problems:
;;
;; 1. When you zoom in the scroll bars are reset to 0, 0
;;
;; 2. There's a lot of flickering.
(defun zoom-x-drag (gadget scale)
  (let ((frame (pane-frame gadget)))
    (let ((pane (find-pane-named frame 'gantlet)))
      (when pane
        (let ((view (stream-default-view pane)))
          (when view
            (let ((old-x-scale (zoom-x-level view)))
              (climi::letf (((zoom-x-level view) scale))
                ;; 1. find current viewport center
                (let (new-x-pos
                      new-y-pos)
                  (when (pane-viewport pane)
                    (with-bounding-rectangle* (new-x1 new-y1 new-x2 new-y2)
                        (pane-viewport-region pane)
                      (let ((old-x-center (/ (+ new-x1 (/ (- new-x2 new-x1) 2)) old-x-scale)))
                        (setf new-x-pos (- (* old-x-center scale) (/ (- new-x2 new-x1) 2))
                              new-y-pos new-y1)
                        (redraw *application-frame* pane))))
                  ;; 2. set the viewport center to the previous viewport center
                  (when (and new-x-pos new-y-pos)
                    (scroll-extent pane new-x-pos new-y-pos)))))))))))

(defun zoom-x-value-changed (gadget scale)
  (let ((frame (pane-frame gadget)))
    (let ((pane (find-pane-named frame 'gantlet)))
      (when pane
        (let ((view (stream-default-view pane)))
          (when view
            (let ((old-x-scale (zoom-x-level view)))
              (setf (zoom-x-level view) scale)
            ;; 1. find current viewport center
              (let (new-x-pos
                    new-y-pos)
                (when (pane-viewport pane)
                  (with-bounding-rectangle* (new-x1 new-y1 new-x2 new-y2)
                      (pane-viewport-region pane)
                    (let ((old-x-center (/ (+ new-x1 (/ (- new-x2 new-x1) 2)) old-x-scale)))
                      (setf new-x-pos (- (* old-x-center scale) (/ (- new-x2 new-x1) 2))
                            new-y-pos new-y1)
                      (redraw *application-frame* pane)
                      #+nil
                      (progn
                        (draw-circle* pane
                                           (* old-x-center scale)
                                           (+ new-y1 (/ (- new-y2 new-y1) 2))
                                           20 :filled t :ink +orange+)
                        (draw-circle* pane
                                      (+ new-x1 (/ (- new-x2 new-x1) 2))
                                      (+ new-y1 (/ (- new-y2 new-y1) 2))
                                      20 :filled t :ink +pink+)))))
                ;; 2. set the viewport center to the previous viewport center
                (when (and new-x-pos new-y-pos)
                  (scroll-extent pane new-x-pos new-y-pos))))))))))

(defun zoom-y-drag (gadget scale)
  (let ((frame (pane-frame gadget)))
    (let ((pane (find-pane-named frame 'gantlet)))
      (when pane
        (let ((view (stream-default-view pane)))
          (when view
            (let ((old-y-scale (zoom-y-level view)))
              (climi::letf (((zoom-y-level view) scale))
                ;; 1. find current viewport center
                (let (new-x-pos
                      new-y-pos)
                  (when (pane-viewport pane)
                    (with-bounding-rectangle* (new-x1 new-y1 new-x2 new-y2)
                        (pane-viewport-region pane)
                      (let ((old-y-center (/ (+ new-y1 (/ (- new-y2 new-y1) 2)) old-y-scale)))
                        (setf new-x-pos new-x1
                              new-y-pos (- (* old-y-center scale) (/ (- new-y2 new-y1) 2)))
                        (redraw *application-frame* pane))))
                  ;; 2. set the viewport center to the previous viewport center
                  (when (and new-x-pos new-y-pos)
                    (scroll-extent pane new-x-pos new-y-pos)))))))))))

(defun zoom-y-value-changed (gadget scale)
  (declare (optimize (debug 3)))
  (let ((frame (pane-frame gadget)))
    (let ((pane (find-pane-named frame 'gantlet)))
      (when pane
        (let ((view (stream-default-view pane)))
          (when view
            (let ((old-y-scale (zoom-y-level view)))
              (setf (zoom-y-level view) scale)
            ;; 1. find current viewport center
              (let (new-x-pos
                    new-y-pos)
                (when (pane-viewport pane)
                  (with-bounding-rectangle* (new-x1 new-y1 new-x2 new-y2)
                      (pane-viewport-region pane)
                    (let ((old-y-center (/ (+ new-y1 (/ (- new-y2 new-y1) 2)) old-y-scale)))
                      (setf new-x-pos new-x1
                            new-y-pos (- (* old-y-center scale) (/ (- new-y2 new-y1) 2)))
                      (redraw *application-frame* pane)
                      #+nil
                      (progn
                        (draw-circle* pane
                                      (+ new-x1 (/ (- new-x2 new-x1) 2))
                                      (* old-y-center scale)
                                      20 :filled t :ink +orange+)
                        (draw-circle* pane
                                      (+ new-x1 (/ (- new-x2 new-x1) 2))
                                      (+ new-y1 (/ (- new-y2 new-y1) 2))
                                      20 :filled t :ink +pink+)))))
                ;; 2. set the viewport center to the previous viewport center
                (when (and new-x-pos new-y-pos)
                  (scroll-extent pane new-x-pos new-y-pos))))))))))

(define-application-frame gantlet-app ()
  ((task :initarg :task :accessor gantlet-app-task))
  (:menu-bar menubar-command-table)
  (:panes
   (gantlet
    (make-pane 'gantlet-chart-pane
               :background *app-pane-background-color*
               :display-function 'gantlet-chart-display
               :display-time :command-loop
               :height 600
               :width 200
               :task (gantlet-app-task *application-frame*)))
   (gantlet-table
    (make-pane 'gantlet-table-pane
               :background *app-pane-background-color*
               :display-function 'gantlet-table-display
               :display-time :command-loop
               :height 600
               :width 400
               :max-width 600
               :task (gantlet-app-task *application-frame*)))
   (resource-list
    (make-pane 'list-pane
	       :value 'clim:region-intersection
	       :name-key (lambda (x) (format nil "~(~A~)" x))))
   (unscheduled-task-list
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
           :drag-callback 'zoom-x-drag
           :value-changed-callback 'zoom-x-value-changed
           :min-height 24 :max-height 24)
   (zoom-y :slider
           :min-value 0.1
           :max-value 10
           :decimal-places 2
           :value 1.0d0
           :show-value-p t
           :orientation :vertical
           :drag-callback 'zoom-y-drag
           :value-changed-callback 'zoom-y-value-changed
           :min-width 24 :max-width 24)
   (interactor :interactor
               :height 200
               :width 1200))
  (:layouts
   (default (vertically ()
              (4/5
               (horizontally ()
                 (8/9
                  (horizontally ()
                    (1/4 (scrolling ()
                           gantlet-table))
                    (clim:make-pane 'clime:box-adjuster-gadget)
                    (vertically ()
                      (horizontally ()
                        (3/4 (scrolling ()
                               gantlet))
                        (labelling (:label "Zoom Y")
                          zoom-y))
                      (labelling (:label "Zoom X")
                        zoom-x))))
                 (clim:make-pane 'clime:box-adjuster-gadget)
                 (1/9
                  (vertically ()
                    (labelling (:label "Resources")
                      resource-list)
                    (labelling (:label "Unscheduled Tasks")
                      unscheduled-task-list)))))
              (clim:make-pane 'clime:box-adjuster-gadget)
              (1/5 interactor)))))

(defun gantlet-panes ()
  (mapcar (lambda (pane-name)
            (find-pane-named *application-frame* pane-name))
          '(gantlet gantlet-table)))

(defun update-list-panes (task)
  (let ((resource-list (find-pane-named *application-frame* 'resource-list)))
    (when resource-list
      (let ((resources (task-resources task)))
        (setf (climi::visible-items resource-list) (length resources))
        (setf (clime:list-pane-items resource-list :invoke-callback nil)
              (mapcar #'name resources)))))
  (let ((unscheduled-task-list (find-pane-named *application-frame* 'unscheduled-task-list)))
    (when unscheduled-task-list
      (let ((unscheduled-tasks
             (remove-if (lambda (x) (and (numberp (task-progress x))
                                         (>= (task-progress x) 1)))
                        (unscheduled-tasks task))))
        (setf (climi::visible-items unscheduled-task-list) (length unscheduled-tasks))
        (setf (clime:list-pane-items unscheduled-task-list :invoke-callback nil)
              (mapcar #'name unscheduled-tasks))))))

(defun gantlet-chart-display (frame pane)
  (let* ((pane-task (pane-task pane)))
    (when pane-task
      (present pane-task 'chart-top-level-task :stream pane)
      (update-list-panes pane-task)
      (clim:replay (clim:stream-output-history pane) pane))))

(defun gantlet-table-display (frame pane)
  (let* ((pane-task (pane-task pane)))
    (when pane-task
      (present pane-task 'table-top-level-task :stream pane)
      (update-list-panes pane-task)
      (clim:replay (clim:stream-output-history pane) pane))))

(define-gantlet-app-command (com-quit :name t :keystroke (#\q :meta)) ()
  (frame-exit *application-frame*))

(defun redraw-and-reset-scroll-extent (pane)
  (let ((vis-region (pane-viewport-region pane)))
    (with-bounding-rectangle* (x1 y1 x2 y2)
        vis-region
      (declare (ignore x2 y2))
      (redraw *application-frame* pane)
      (scroll-extent pane x1 y1))))

(defmacro do-for-gantlet-panes ((pane view) &body body)
  `(loop for ,pane in (gantlet-panes)
      do
        (let* ((,view (stream-default-view ,pane)))
          ,@body)))

;; show task info
(define-gantlet-app-command (com-show-task-info :name t :menu t
                                                :keystroke (#\i :meta))
    ((task task))
  (do-for-gantlet-panes (gantlet-pane task-view)
    (with-accessors ((show-task-info-hash-table task-view-show-task-info-hash-table))
        task-view
      (let ((expanded (gethash task show-task-info-hash-table)))
        (setf (gethash task show-task-info-hash-table) (not expanded))
        (redraw-and-reset-scroll-extent gantlet-pane)))))

(define-gantlet-app-command (com-show-all-child-tasks :name t :menu t) ()
  (do-for-gantlet-panes (gantlet-pane task-view)
    (with-accessors ((hide-task-children-hash-table task-view-hide-task-children-hash-table))
        task-view
      (clrhash hide-task-children-hash-table)
      (redraw-and-reset-scroll-extent gantlet-pane))))

(define-gantlet-app-command (com-hide-task-children :name t :menu t
                                                    :keystroke (#\h :meta))
    ((task task))
  (do-for-gantlet-panes (gantlet-pane task-view)
    (with-accessors ((hide-task-children-hash-table task-view-hide-task-children-hash-table))
        task-view
      (let ((expanded (gethash task hide-task-children-hash-table)))
        (setf (gethash task hide-task-children-hash-table) (not expanded))
        (redraw-and-reset-scroll-extent gantlet-pane)))))

(define-gantlet-app-command (com-redraw :name t) ()
  (do-for-gantlet-panes (gantlet-pane task-view)
    (declare (ignore task-view))
    (redraw *application-frame* gantlet-pane)))

(define-gantlet-app-command (com-hide-completed-tasks :name t) ()
  (do-for-gantlet-panes (gantlet-pane task-view)
    (with-accessors ((hide-completed-tasks task-view-hide-completed-tasks))
        task-view
      (setf hide-completed-tasks t)
      (redraw-and-reset-scroll-extent gantlet-pane))))

(define-gantlet-app-command (com-show-completed-tasks :name t) ()
  (do-for-gantlet-panes (gantlet-pane task-view)
    (with-accessors ((hide-completed-tasks task-view-hide-completed-tasks))
        task-view
      (setf hide-completed-tasks nil)
      (redraw-and-reset-scroll-extent gantlet-pane))))

(define-gantlet-app-command (com-hide-past-tasks :name t) ()
  (do-for-gantlet-panes (gantlet-pane task-view)
    (with-accessors ((hide-past-tasks task-view-hide-past-tasks))
        task-view
      (setf hide-past-tasks t)
      (redraw-and-reset-scroll-extent gantlet-pane))))

(define-gantlet-app-command (com-show-past-tasks :name t) ()
  (do-for-gantlet-panes (gantlet-pane task-view)
    (with-accessors ((hide-past-tasks task-view-hide-past-tasks))
        task-view
      (setf hide-past-tasks nil)
      (redraw-and-reset-scroll-extent gantlet-pane))))

(define-gantlet-app-command (com-hide-non-critical-tasks :name t) ()
  (do-for-gantlet-panes (gantlet-pane task-view)
    (with-accessors ((hide-non-critical-tasks task-view-hide-non-critical-tasks))
        task-view
      (setf hide-non-critical-tasks t)
      (redraw-and-reset-scroll-extent gantlet-pane))))

(define-gantlet-app-command (com-show-non-critical-tasks :name t) ()
  (do-for-gantlet-panes (gantlet-pane task-view)
    (with-accessors ((hide-non-critical-tasks task-view-hide-non-critical-tasks))
        task-view
      (setf hide-non-critical-tasks nil)
      (redraw-and-reset-scroll-extent gantlet-pane))))

(define-gantlet-app-command (com-set-earlier-start-date :name t) ()
  (do-for-gantlet-panes (gantlet-pane task-view)
    (with-accessors ((start task-view-start))
        task-view
      (setf start (local-time:adjust-timestamp
                      start
                    (:offset :month -1)))
      (redraw-and-reset-scroll-extent gantlet-pane))))

(define-gantlet-app-command (com-set-later-start-date :name t) ()
  (do-for-gantlet-panes (gantlet-pane task-view)
    (with-accessors ((start task-view-start))
        task-view
      (setf start (local-time:adjust-timestamp
                      start
                    (:offset :month +1)))
      (redraw-and-reset-scroll-extent gantlet-pane))))

;; gestures
(define-gesture-name show-task-info-gesture :pointer-button (:left))

(define-gesture-name hide-task-children-gesture :pointer-button (:left :control))

;; do we need this??
#+nil
(defmethod true-viewport-region ((pane gantlet-pane))
  (untransform-region (sheet-native-transformation pane)
                      (sheet-native-region pane)))

(define-gantlet-app-command (com-redisplay :name t) ()
  #+nil
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
                                 view
                                 (zoom-x 1.0)
                                 (zoom-y 1.0))
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
                                    :zoom-x zoom-x
                                    :zoom-y zoom-y
                                    :start (start task)
                                    :end (end task))))
      (let ((*standard-output* stream))
        (present task 'chart-top-level-task)))))

(define-gantlet-app-command (com-write-task-to-pdf :name t)
    ((pdf-pathname pathname
                   :default *default-pathname-defaults* :insert-default t))
  (let* ((gantlet-pane (find-pane-named *application-frame* 'gantlet)))
    (with-accessors ((task pane-task)
                     (task-view stream-default-view))
        gantlet-pane
      (with-bounding-rectangle* (x1 y1 x2 y2)
          (sheet-region gantlet-pane)
        (apply 'write-task-to-pdf-file task pdf-pathname
               :device-type (list (- x2 x1) (+ (- y2 y1) 500))
               (when task-view
                 `(:view ,task-view)))))))

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

(defun run-gantlet (&key (height 1200) (width 1600) task)
  (let ((frame (setf *gantlet-application* (make-application-frame 'gantlet-app
                                                                   :height height
                                                                   :width width
                                                                   :task task))))
    (values frame
            (bt:make-thread
             (lambda ()
               (run-frame-top-level frame))))))

#+nil
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
      (clim:draw-rectangle* pane 0 0 1000 1000
                            :filled t
                            :ink clim:+background-ink+))))

