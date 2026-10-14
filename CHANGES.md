# Changelog

All notable changes to this project are documented here. The format follows
[Keep a Changelog](https://keepachangelog.com/en/1.1.0/) and the project uses
[Semantic Versioning](https://semver.org/).

## Unreleased

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
