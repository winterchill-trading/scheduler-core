# scheduler-core: notes for agents

## Purpose

Recurring schedules (once, weekly, fortnightly, monthly, every N days) with dates
computed by index from the start, a pure schedule state machine, and a runner that runs
each (schedule, due date) exactly once through a claim in a pluggable store. Any kind of
recurring job (standing orders, recurring buys, reminders, payroll runs) implements one
`Job.S` interface.

## Layout

| Path | Holds |
|---|---|
| `lib/` | the `scheduler-core` library (core only) |
| `bin/` | `scheduler-dates` command, package `scheduler-core-cli` |
| `test/` | expect tests, Quickcheck properties, stable digests, cram test for the CLI |
| `examples/` | `three_jobs` (output diffed in CI) and the README quick start |
| `.github/workflows/ci.yml` | CI |

## Build, test, docs

The workspace keeps every org repository as a sibling checkout with a root
`dune-workspace`. From this directory:

```
DUNE_BUILD_DIR=_build-scheduler-core opam exec --switch 5.2.0+ox -- dune build --root .. @scheduler-core/all
DUNE_BUILD_DIR=_build-scheduler-core opam exec --switch 5.2.0+ox -- dune build --root .. @scheduler-core/runtest
DUNE_BUILD_DIR=_build-scheduler-core opam exec --switch 5.2.0+ox -- dune build --root .. @scheduler-core/doc
```

`DUNE_BUILD_DIR=_build-scheduler-core` resolves from the workspace root because of
`--root ..` (`../_build-...` fails as outside the workspace). Standalone, with an OxCaml
switch: `dune build && dune test && dune build @examples/runtest`; there are no org
dependencies to pin.

## Conventions

- Format only this repo: `dune build --root .. @scheduler-core/fmt --auto-promote`, after
  tests pass. Never `dune fmt` across the workspace.
- Every module has an `.mli` with odoc on each public value.
- Persisted types have `Stable.V1` forms whose readers validate; their bin_prot digests
  are pinned in `test/test_stable.ml`. A changed digest means add `V2`, not edit `V1`.
- Expect tests for behaviour, Quickcheck (`Quickcheck.test` in `let%test_unit`) for
  invariants; shared generators live in `test/gen.ml`.
- The library never persists and never depends on Async: the runner is a functor over
  any `Monad.S`, the store is a module type, the clock is a value.
- Comments for humans at non-obvious choices; user-visible changes get a line under
  "Unreleased" in `CHANGES.md`.

## Dependencies and dependents

Depends on `core` and `ppx_jane` only (`core_unix` and `base_quickcheck` for tests, the
CLI uses `core_unix`). It deliberately does not reuse `../payments-core`'s
`Schedule`/`Standing_order` (README, "Relation to payments-core"). No org dependents yet.

## CI

`ox-setup` from `../infra` (cached opam switch), then `dune build`, `dune test`,
`dune build @examples/runtest`, plus the reusable obj-magic check. No pins needed.
Check with `gh run list --limit 1`, then at most one `gh run watch <id> --exit-status`.

## Pitfalls

- Binding `Runner.Make (...)` to a module named `Runner` hides the library's
  `Runner`; `Execution` and `Command_error` are re-exported from the functor result for
  that reason.
- A run claim and a missed/skipped settlement are different store operations
  (`claim` vs `settle_unclaimed`). A date with any run claim may already have had its
  side effect: it must be replayed, never settled as missed or skipped or resumed past.
- `Store.claim` and `Store.settle_unclaimed` must be atomic per run key in any database
  implementation; run `Store_conformance.Make` against it.
- Monthly dates come from the start and the index, never from the previous date;
  keep it that way or short months shift every later date.
- Tests that load named time zones need `core_unix.time_float_unix`; the library takes a
  `Time_float.Zone.t`.
- The main module `lib/scheduler_core.ml` must re-export every new module.
- `Schedule.t` is the V2 record; `Schedule.Stable.V1` converts through the V1 shape with
  `Binable.Of_binable1_without_uuid` so its digest stays pinned (a `with_uuid` variant
  would change it). With `Successful_runs`, dates come from
  `Recurrence.without_max_occurrences` and completion from the success count; route any
  new date lookup in `schedule.ml` through the local `dates`/`occurrence` helpers.

## Where to read more

`README.md`, `CHANGES.md`, `../specs/repos/scheduler-core.md` when present,
`../oxcaml-claude-skills/skills/janestreet-ocaml-style` and `banking-standards`, and
`../oxcaml-claude-skills/AGENTS.md`, the workspace-wide guide.

## Never do

- Install or upgrade opam packages that change the compiler, core, base, async or
  ppx_jane on the shared switch.
- Publish, tag or release.
- Commit credentials or print secrets.
- Poll CI in a loop.
