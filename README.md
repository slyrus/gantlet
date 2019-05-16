
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

    (defparameter *application*
      (gantlet:run-gantlet :task gantt-example::*example-project*))

More documentation to follow.
