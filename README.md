
* Overview

You will also need [Gantt](https://github.com/slyrus/gantt) to run this.

To run this:

    (asdf:load-system "gantt")
    (asdf:load-system "gantt-example")
    (asdf:load-system "gantlet")

    (let ((app (gantlet::gantlet-main)))
      (sleep 0.5)
      (let ((gantlet-pane (clim:find-pane-named app 'gantlet)))
        (set-pane-task gantlet-pane gantt-example::*example-project*)))

More documentation to follow.
