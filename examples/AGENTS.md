# examples/

Built with the project (and in CI), not installed.

- `three_jobs.ml`: three kinds of jobs on one runner; its output is diffed against
  `three_jobs.expected` by `dune build --root .. @scheduler-core/examples/runtest`.
  After an intended change, copy the new `three_jobs.output` from the build directory
  over the expected file.
- `quick_start.ml`: the README quick start, kept compiling; update both together.
