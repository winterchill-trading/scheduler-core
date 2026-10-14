# test/

Inline expect tests and Quickcheck properties (`scheduler_core_test`), plus a cram test
for `scheduler-dates` in `cli.t/`.

- Run: `DUNE_BUILD_DIR=_build-scheduler-core opam exec --switch 5.2.0+ox -- dune build --root .. @scheduler-core/runtest`;
  accept new output with `dune promote --root ..` (same `DUNE_BUILD_DIR`) and read
  every diff first.
- `test_stable.ml` pins bin_prot digests: a change there is a wire break.
- `test_runner_properties.ml` drives two runners on one store with crashes, pauses,
  resumes and skips; keep it passing before touching `lib/runner.ml`.
- `test_schedule_restore.ml` checks `restore (position t) = t` and that running on
  from a restored schedule matches running on from the original; any new `Schedule.t`
  field must be carried by `Schedule.Position` or derived, or that property fails.
- Generators shared between files live in `gen.ml`.
