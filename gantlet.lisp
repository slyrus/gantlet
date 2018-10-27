
(in-package #:gantlet)

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
      ;; 1. find current viewport center
      (when (pane-viewport pane)
        (with-bounding-rectangle* (old-x1 old-y1 old-x2 old-y2)
            (pane-viewport-region pane)
          (declare (ignore old-x2 old-y1 old-y2))
          (with-bounding-rectangle* (new-x1 new-y1 new-x2 new-y2)
              (pane-viewport-region pane)
            (declare (ignore new-y2))
            (let ((new-x-pos (+ old-x1 (/ (- new-x2 new-x1) 2))))
              ;; 2. set the viewport center to the previous viewport center
              (scroll-extent pane new-x-pos new-y1)))))
      (redraw *application-frame* pane))))

(defun zoom-y-callback (gadget scale)
  (let ((frame (pane-frame gadget)))
    (let ((pane (find-pane-named frame 'gantlet)))
      (let ((view (stream-default-view pane)))
        (when view
          (setf (zoom-y-level view) scale)))
      #+nil (setf (pane-needs-redisplay pane) t)
      (redraw *application-frame* pane)
      #+nil (repaint-sheet pane +everywhere+))))

(defun gantlet-display (frame pane)
  (declare (ignore frame))
  (let* ((task (pane-task pane)))
    (when task
      (present task 'top-level-task)
      (clim:replay (clim:stream-output-history pane) pane))))

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
        :max-height 100
        :width 1200))
  (:layouts
   (default (vertically ()
              (horizontally ()
                (8/9 (vertically ()
                        (horizontally ()
                          (scrolling ()
                            gantlet)
                          (labelling (:label "Zoom Y")
                            zoom-y))
                        (labelling (:label "Zoom X")
                          zoom-x)))
                (1/9 (labelling (:label "Resources")
                       resource-list)))
              int))))

(define-gantlet-app-command (com-quit :name t :keystroke (#\q :meta)) ()
  (frame-exit *application-frame*))

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

(defun run-gantlet (&key (height 1200) (width 1600))
  (let ((frame (setf *gantlet-application* (make-application-frame 'gantlet-app
                                                                   :height height
                                                                   :width width))))
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

