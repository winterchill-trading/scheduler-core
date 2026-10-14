# lib/

The `scheduler-core` library. Dependency order inside it: `Frequency`, `Recurrence`,
`Schedule_id`, `Clock`, `Run_key`, `Outcome`, `Run_record`, `Schedule`, `Job`, `Store`,
`Store_memory`, `Store_conformance`, `Runner`.

- `Schedule` is pure: no store, no clock. `Runner` is the only place that touches a
  store and calls jobs.
- New modules must be added to `scheduler_core.ml` with a one-line doc comment.
- Persisted types: change `Stable.V1` only by adding `V2`; readers validate through
  `Binable.Of_binable_without_uuid [@alert "-legacy"]`.
- Keep the library on `core` only (no Async, no unix).

Build: `DUNE_BUILD_DIR=_build-scheduler-core opam exec --switch 5.2.0+ox -- dune build --root .. @scheduler-core/all`.
