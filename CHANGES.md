# Changelog

All notable changes to this project are documented here. The format follows
[Keep a Changelog](https://keepachangelog.com/en/1.1.0/) and the project uses
[Semantic Versioning](https://semver.org/).

## Unreleased

### Added

- `Schedule.Max_occurrences_counts` and `?max_occurrences_counts` on `Schedule.create`
  and `Runner.add`: with `Successful_runs`, a recurrence's `max_occurrences` counts
  successful runs, so skipped, missed and failed dates do not use it up (an end date
  still applies). The default, `Dates`, is unchanged.
- `Schedule.next_to_skip ?today`: a paused schedule can name the date resuming on
  `today` would run first, and `Runner.skip ~paused:true` skips it while the schedule
  stays paused. Without the option paused schedules are refused as before.
- `Schedule.Stable.V2` with the new field. `Stable.V1` keeps its shape and digest, reads
  as `Dates`, and refuses to write a schedule counting successful runs.
- `Recurrence.without_max_occurrences`.
- `Schedule.Position`, `Schedule.position` and `Schedule.restore`: rebuild a schedule
  from a stored position (the last date behind it, such as the last paid date, the
  success count, failures in a row and the status) without replaying run history.
  Inconsistent positions are refused with `Schedule.Restore_error`. A Quickcheck
  property checks that restoring and then running equals running from history.
  `Schedule.Stable.Position.V1` has a pinned digest.

## 0.1.0

First version prepared for opam; not yet published.

### Added

- `Frequency`, `Recurrence`: once, weekly, fortnightly, monthly (with an optional day of
  month, clamped to short months) and every N days; dates computed by index from the
  start; end date and maximum count; index lookups and range queries; validation.
- `Clock`: injected time and zone, with a manual clock.
- `Schedule`: pure state machine with pause, resume (passed dates not paid), cancel,
  skip, completion, a pause after consecutive failures and a missed-date policy (latest
  only, or catch up).
- `Run_key`, `Run_record`, `Outcome`: one durable record per (schedule, due date).
- `Job`: the interface shared by every kind of recurring job.
- `Store`, `Store_memory`, `Store_conformance`: persistence as a module type, an
  in-memory implementation and a conformance suite.
- `Runner`: exactly-once runs per due date over any monad, with stale-claim take-over
  and replay.
- `scheduler-dates` command in the `scheduler-core-cli` package.
- `Stable.V1` bin_prot and sexp forms with pinned digests.
