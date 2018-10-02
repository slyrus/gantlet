
# Overview

You will also need [Gantt](https://github.com/slyrus/gantt) to run this.

# Prerequisites

## Quicklisp Dependenceies

You can pick up most of the needed libraries from Quicklisp as follows:

    (ql:quickload '(mcclim local-time time-interval named-readtables cl-who copy-directory))

Unfortunately, one currently needs to get time-interval from the github repo:

    git clone https://github.com/slyrus/time-interval.git

Hopefully this will get picked up in the next Quicklisp release and
users can go back to picking up time-interval from Quicklisp.

And to see the exmple GANTT data, the
[gantt-example](https://github.com/slyrus/gantt-example/) git repo can
be found here:

    git clone https://github.com/slyrus/gantt-example.git

To run this:

    (asdf:load-system "gantt-example")
    (asdf:load-system "gantlet")

    (let ((app (gantlet::gantlet-main)))
      (sleep 1.0)
      (let ((gantlet-pane (clim:find-pane-named app 'gantlet::gantlet)))
        (gantlet::set-pane-task gantlet-pane gantt-example::*example-project*)
        (gantlet::redraw app gantlet-pane)))

Unfortunately, one must still manually redraw the application pane to
see the data. Select Redraw from the Gantt menu to see the data.

More documentation to follow.
