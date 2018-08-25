
* Overview

You will also need [Gantt](https://github.com/slyrus/gantt) to run this.

* Prerequisites

Quicklisp:

    (ql:quickload '(mcclim local-time time-interval named-readtables cl-who copy-directory))

To run this:

    (asdf:load-system "gantt")
    (asdf:load-system "gantt-example")
    (asdf:load-system "gantlet")

    (let ((app (gantlet::gantlet-main)))
      (sleep 0.5)
      (let ((gantlet-pane (clim:find-pane-named app 'gantlet)))
        (gantlet::set-pane-task gantlet-pane gantt-example::*example-project*)))

More documentation to follow.
