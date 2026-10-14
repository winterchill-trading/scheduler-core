(lang dune 3.17)

(name scheduler-core)

(generate_opam_files true)

(license MIT)

(authors "WinterChill AI")

(maintainers "hello@winterchill.xyz")

(source
 (github winterchill-trading/scheduler-core))

(homepage "https://github.com/winterchill-trading/scheduler-core")

(bug_reports "https://github.com/winterchill-trading/scheduler-core/issues")

(documentation "https://github.com/winterchill-trading/scheduler-core#readme")

(package
 (name scheduler-core)
 (synopsis "Recurring schedules, due dates and exactly-once runs for any kind of job")
 (description
  "scheduler-core computes the dates of a recurring schedule (once, weekly, fortnightly, monthly, every N days) by index from its start, so a monthly schedule on the 31st clamps to the end of shorter months without drifting. A pure state machine tracks each schedule (active, paused, completed, cancelled), pauses it after consecutive failures, resumes it without paying passed dates and pays only the latest date after a gap. A runner over any monad claims each (schedule, date) pair in a store before calling the job, so a crashed or stale run is retried with the same key and a date is never paid twice. Stores are a module type with an in-memory implementation; the clock and time zone are injected. Requires the OxCaml compiler.")
 (tags
  (scheduler recurring payments standing-order oxcaml))
 (depends
  oxcaml
  (ocaml
   (and
    (>= 5.2.0)
    (< 5.3~)))
  dune
  (core
   (and
    (>= v0.18~preview.130.106+341)
    (< v0.19~)))
  (ppx_jane
   (and
    (>= v0.18~preview.130.106+341)
    (< v0.19~)))
  (core_unix
   (and
    :with-test
    (>= v0.18~preview.130.106+341)
    (< v0.19~)))
  (base_quickcheck :with-test)
  (odoc :with-doc)))

(package
 (name scheduler-core-cli)
 (synopsis "Command-line tool that lists the dates of a recurring schedule")
 (description
  "The scheduler-dates command prints the run dates of a recurrence (frequency, start, end, maximum count, day of month) and which date is due on a given day. Built on the scheduler-core library.")
 (tags
  (scheduler recurring cli oxcaml))
 (depends
  oxcaml
  (ocaml
   (and
    (>= 5.2.0)
    (< 5.3~)))
  dune
  (scheduler-core
   (= :version))
  (core
   (and
    (>= v0.18~preview.130.106+341)
    (< v0.19~)))
  (core_unix
   (and
    (>= v0.18~preview.130.106+341)
    (< v0.19~)))
  (ppx_jane
   (and
    (>= v0.18~preview.130.106+341)
    (< v0.19~)))
  (odoc :with-doc)))
