
* Overview

To run this:

    (let ((app (gantlet::gantlet-main)))
      (sleep 0.5)
      (let ((gantlet-pane (clim:find-pane-named app 'gantlet)))
        (set-pane-task gantlet-pane gantt-example::*example-project*)))

More documentation to follow.
