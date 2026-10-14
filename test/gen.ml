open! Core
open Scheduler_core

let date =
  Quickcheck.Generator.map
    (Int.gen_incl 0 (365 * 8))
    ~f:(fun n -> Date.add_days (Date.of_string "2024-01-01") n)
;;

let frequency =
  let open Quickcheck.Generator in
  union
    [ return Frequency.Once
    ; return Frequency.Weekly
    ; return Frequency.Fortnightly
    ; map
        (Option.quickcheck_generator (Int.gen_incl 1 31))
        ~f:(fun day_of_month -> Frequency.Monthly { day_of_month })
    ; map (Int.gen_incl 1 Frequency.max_interval_days) ~f:(fun n ->
        Frequency.Every_n_days n)
    ]
;;

let recurrence =
  let open Quickcheck.Generator.Let_syntax in
  let%bind frequency
  and start = date
  and end_after = Option.quickcheck_generator (Int.gen_incl 0 2000)
  and max_occurrences = Option.quickcheck_generator (Int.gen_incl 1 40) in
  let end_date = Option.map end_after ~f:(Date.add_days start) in
  match Recurrence.create frequency ~start ?end_date ?max_occurrences () with
  | Ok r -> return r
  | Error _ -> return (Recurrence.create_exn frequency ~start ())
;;
