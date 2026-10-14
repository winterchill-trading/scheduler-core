# scheduler-core

Recurring schedules, due dates and exactly-once runs for any kind of job: standing
orders, recurring buys, payroll runs, reminders, rule evaluations.

- **Dates by index.** Date `i` of a schedule is computed from its start, never from the
  previous date, so a monthly schedule on 31 January runs on 28 February, 31 March,
  30 April, and never drifts to the 28th. Frequencies: once ("send later"), weekly,
  fortnightly, monthly (optionally on a given day of the month) and every N days, bounded
  by an end date and/or a maximum count.
- **Due on the owner's calendar.** The clock and the time zone are injected; a date is
  due from midnight in that zone.
- **Exactly once per date.** Before running a job the runner claims the
  `(schedule, date)` pair in a store. A claim older than two minutes belongs to a runner
  that crashed or hung: the next runner takes it over and calls the job again with the
  same key, which the job passes on as its idempotency key, so the work is replayed, not
  repeated. A date that carries a claim is never marked missed or skipped and is never
  resumed past.
- **Gaps pay only the latest date.** After an outage, the earlier passed dates are
  recorded as missed and only the latest one runs (or, per schedule, every passed date
  runs in order).
- **Pause after failures.** Three failed dates in a row (configurable) pause the schedule
  with the reason. Resuming continues from the first date on or after today; dates that
  passed while paused are not paid.
- **Pause, resume, skip, cancel**, with refusals as values a service maps to its own
  error codes. A paused schedule can skip the date it would run first after resuming
  (`Runner.skip ~paused:true`).
- **Count dates or successful runs.** `max_occurrences` bounds the dates by default;
  with `~max_occurrences_counts:Successful_runs` "twelve instalments" means twelve
  successful runs, and skipped, missed or failed dates do not use one up.

## Why separate

Every bank and broker has recurring work, and the hard parts are the same for all of it:
month-end arithmetic, time zones, crash safety and what to do after downtime. Keeping
them in one small library, independent of payments, ledgers and databases, lets each
kind of job be a few lines and lets the scheduling rules be tested once, with property
tests, instead of in every service.

## Quick start

```ocaml
open! Core
open Scheduler_core

module Pay = struct
  type t = { cents : int } [@@deriving sexp_of]

  let validate _ _ = Ok ()

  (* Pass the run key on as the idempotency key of the payment request. *)
  let run _t (c : Job.Context.t) = Ok ("payment-" ^ Run_key.to_string c.key)
end

module Store = Store_memory.Make (Monad.Ident) (Pay)
module Runner = Runner.Make (Monad.Ident) (Pay) (Store)

let () =
  let clock = Clock.wall ~zone:Time_float.Zone.utc in
  let runner = Runner.create ~store:(Store.create ()) ~clock () in
  let recurrence =
    Recurrence.create_exn
      (Monthly { day_of_month = None })
      ~start:(Date.of_string "2027-01-31")
      ()
  in
  ignore (Runner.add runner ~id:(Schedule_id.create_exn "rent") { cents = 900_00 } recurrence);
  (* call from a timer, e.g. every minute *)
  List.iter (Runner.tick runner) ~f:(fun e -> print_s [%sexp (e : Runner.Execution.t)])
;;
```

With Async, instantiate the runner over `Deferred` (`Runner.Make (Deferred) (Job)
(Store)`), make the clock read the time source
(`Clock.create ~now:(fun () -> Time_source.now ts) ~zone`) and call `tick` from
`Clock.every`. [`examples/three_jobs.ml`](examples/three_jobs.ml) runs a standing order,
a recurring buy and a reminder on one runner, including a gap and a failure.

The `scheduler-dates` command (package `scheduler-core-cli`) prints a recurrence:

```
$ scheduler-dates -frequency monthly -start 2027-01-31 -count 4
0	2027-01-31	SUN
1	2027-02-28	SUN
2	2027-03-31	WED
3	2027-04-30	FRI
```

## Modules

| Module | What it is |
|---|---|
| `Frequency` | once, weekly, fortnightly, monthly (day of month), every N days |
| `Recurrence` | dates by index, bounds, lookups (`first_index_on_or_after`, `between`), validation |
| `Clock` | injected time and zone; `Clock.Manual` for tests and demo controls |
| `Schedule` | pure state machine: `due`, `settle`, `pause`, `resume`, `cancel`, `upcoming` |
| `Run_key`, `Run_record`, `Outcome` | one durable record per (schedule, date): claimed, then finished |
| `Job` | the interface every kind of job implements (`validate`, `run`) |
| `Store` | persistence module type; `Store_memory`; `Store_conformance` for other implementations |
| `Runner` | `Make (Monad) (Job) (Store)`: `tick`, `add`, `pause`, `resume`, `skip`, `cancel` |

State machine:

```
active --pause--> paused --resume--> active   (dates passed while paused are not run)
active | paused --cancel--> cancelled
active --3 failed dates in a row--> paused (failures, with the last reason)
active | paused --last date settled--> completed
date: claimed --> succeeded | failed;  skipped (owner);  missed (gap)
```

## Storage

The library never persists anything. A service implements `Store.S` over its own
database; the only hard requirement is that `claim` and `settle_unclaimed` are atomic per
run key (an insert that fails on a duplicate, or a compare-and-set). Run
`Store_conformance.Make` against the implementation in the service's test suite; it is
the same suite `Store_memory` passes. All persisted types have `Stable.V1` bin_prot and
sexp forms with pinned digests; reading validates. Schedules also have `Stable.V2`,
which carries `max_occurrences_counts`; store new rows with V2 (it reads V1 sexps, and
V1 refuses to write a schedule counting successful runs rather than drop the setting).

## Relation to payments-core

`payments-core` has `Schedule` and `Standing_order`, which tie a recurrence to a
transfer draft. This library does not build on them, for three reasons: the work to run
is not always a transfer (orders, payroll batches, reminders); the dates there are
stepped from the previous run, with daily, weekly-on-a-weekday and monthly frequencies
only, while this library needs fortnightly, every N days, a single date and bounded
counts computed by index; and `payments-core` brings money, ledger and crypto
dependencies a scheduler has no use for. Depending on `core` only keeps it usable from
any service. A standing order is a `Job` here whose `run` builds the transfer.

## Tests

Expect tests pin the dates, the transitions and the runner's behaviour around crashes,
gaps and failures. Quickcheck properties check that dates strictly increase and respect
their bounds, that monthly dates depend only on their index, that the arithmetic index
lookups agree with a scan, that due dates are contiguous, and that two runners sharing
a store with random clock jumps, crashes, pauses, resumes and skips never run a date's
first attempt twice and never record a missed or skipped date whose job ran.

## Known gaps

- Editing a schedule's recurrence (as opposed to its job parameters) is not modelled;
  cancel and create a new schedule.
- Business-day calendars (move a date off a weekend or holiday) are left to the job or
  the caller.

## For agents

[`AGENTS.md`](AGENTS.md) has the build commands, conventions and pitfalls.

## Used by

No org repositories yet.
