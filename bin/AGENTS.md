# bin/

`scheduler-dates`, package `scheduler-core-cli`: prints a recurrence's dates and what is
due on a day. Its behaviour is pinned by the cram test in `../test/cli.t/run.t`; update
that test with any flag or output change. Uses `core_unix.command_unix`, which the
library itself must not depend on.
