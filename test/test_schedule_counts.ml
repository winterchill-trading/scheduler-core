open! Core
open Scheduler_core

let d = Date.of_string
let id = Schedule_id.create_exn "s1"

let ok = function
  | Ok t -> t
  | Error r -> raise_s [%sexp (r : Schedule.Refusal.t)]
;;

let show t =
  print_s
    [%sexp
      { status : Schedule.Status.t = Schedule.status t
      ; next : Schedule.Occurrence.t option = Schedule.next t
      ; succeeded : int = Schedule.succeeded t
      }]
;;

(* Weekly from Friday 2027-01-01, three occurrences. *)
let weekly_3 ?max_occurrences_counts () =
  Schedule.create
    ?max_occurrences_counts
    ~id
    ~job:()
    (Recurrence.create_exn Weekly ~start:(d "2027-01-01") ~max_occurrences:3 ())
;;

let run_through t outcomes =
  List.fold outcomes ~init:t ~f:(fun t outcome ->
    match Schedule.next t with
    | None -> t
    | Some o -> Schedule.settle t ~index:o.index outcome)
;;

let outcomes : Outcome.t list =
  [ Succeeded { reference = "p" }
  ; Skipped
  ; Missed
  ; Failed { reason = "declined" }
  ; Succeeded { reference = "p" }
  ; Succeeded { reference = "p" }
  ]
;;

let%expect_test "dates counted: skipped, missed and failed dates use up the limit" =
  let t = run_through (weekly_3 ()) outcomes in
  show t;
  [%expect {| ((status Completed) (next ()) (succeeded 1)) |}]
;;

let%expect_test "successful runs counted: the schedule runs until three succeed" =
  let t = weekly_3 ~max_occurrences_counts:Successful_runs () in
  print_s
    [%sexp
      (Schedule.upcoming t ~from:(d "2027-01-01") ~until:(d "2027-12-31")
       : Schedule.Occurrence.t list)];
  let t = run_through t (List.take outcomes 4) in
  show t;
  (* Two runs are still owed, so only two dates are listed. *)
  print_s
    [%sexp
      (Schedule.upcoming t ~from:(d "2027-01-01") ~until:(d "2027-12-31")
       : Schedule.Occurrence.t list)];
  let t = run_through t (List.drop outcomes 4) in
  show t;
  [%expect
    {|
    (((index 0) (date 2027-01-01)) ((index 1) (date 2027-01-08))
     ((index 2) (date 2027-01-15)))
    ((status Active) (next (((index 4) (date 2027-01-29)))) (succeeded 1))
    (((index 4) (date 2027-01-29)) ((index 5) (date 2027-02-05)))
    ((status Completed) (next ()) (succeeded 3))
    |}]
;;

let%expect_test "an end date still ends a schedule counting successful runs" =
  let t =
    Schedule.create
      ~max_occurrences_counts:Successful_runs
      ~id
      ~job:()
      (Recurrence.create_exn
         Weekly
         ~start:(d "2027-01-01")
         ~end_date:(d "2027-01-10")
         ~max_occurrences:3
         ())
  in
  let t = run_through t [ Skipped; Skipped ] in
  show t;
  [%expect {| ((status Completed) (next ()) (succeeded 0)) |}]
;;

let%expect_test "skip next on a paused schedule" =
  let t = weekly_3 () |> Schedule.pause |> ok in
  print_s
    [%sexp
      (Schedule.next_to_skip t : (Schedule.Occurrence.t, Schedule.Refusal.t) Result.t)];
  (* Paused on 2027-01-05: resuming today would run 2027-01-08 first, so that is what a
     skip applies to; the date already passed (2027-01-01) is never run anyway. *)
  let today = d "2027-01-05" in
  let o = Schedule.next_to_skip ~today t |> ok in
  print_s [%sexp (o : Schedule.Occurrence.t)];
  let t = Schedule.settle t ~index:o.index Skipped in
  show t;
  let t = Schedule.resume t ~today |> ok in
  show t;
  let t = Schedule.pause t |> ok in
  print_s
    [%sexp
      (Schedule.next_to_skip ~today:(d "2027-02-01") t
       : (Schedule.Occurrence.t, Schedule.Refusal.t) Result.t)];
  [%expect
    {|
    (Error Not_active)
    ((index 1) (date 2027-01-08))
    ((status (Paused By_owner)) (next (((index 2) (date 2027-01-15))))
     (succeeded 0))
    ((status Active) (next (((index 2) (date 2027-01-15)))) (succeeded 0))
    (Error Nothing_to_skip)
    |}]
;;

let%expect_test "stable: V1 reads as dates; V2 keeps the count; V1 refuses to drop it" =
  let t = weekly_3 ~max_occurrences_counts:Successful_runs () in
  let v2 =
    Binable.to_string
      (module struct
        type t = unit Schedule.Stable.V2.t [@@deriving bin_io]
      end)
      t
  in
  let back =
    Binable.of_string
      (module struct
        type t = unit Schedule.Stable.V2.t [@@deriving bin_io]
      end)
      v2
  in
  print_s
    [%sexp (Schedule.max_occurrences_counts back : Schedule.Max_occurrences_counts.t)];
  print_s [%sexp (t : unit Schedule.Stable.V2.t)];
  let plain = weekly_3 () in
  let v1 =
    Binable.to_string
      (module struct
        type t = unit Schedule.Stable.V1.t [@@deriving bin_io]
      end)
      plain
  in
  let read =
    Binable.of_string
      (module struct
        type t = unit Schedule.Stable.V1.t [@@deriving bin_io]
      end)
      v1
  in
  print_s [%sexp (Schedule.equal Unit.equal read plain : bool)];
  print_s
    [%sexp
      (Schedule.Stable.V2.t_of_sexp
         Unit.t_of_sexp
         (Schedule.Stable.V1.sexp_of_t Unit.sexp_of_t plain)
       |> Schedule.max_occurrences_counts
       : Schedule.Max_occurrences_counts.t)];
  print_s
    [%sexp
      (Or_error.try_with (fun () -> Schedule.Stable.V1.sexp_of_t Unit.sexp_of_t t)
       |> Result.is_error
       : bool)];
  print_endline [%bin_digest: unit Schedule.Stable.V2.t];
  [%expect
    {|
    Successful_runs
    ((id s1) (job ())
     (recurrence ((frequency Weekly) (start 2027-01-01) (max_occurrences 3)))
     (status Active) (missed_policy Latest_only) (max_consecutive_failures 3)
     (next_index 0) (succeeded 0) (consecutive_failures 0)
     (max_occurrences_counts Successful_runs))
    true
    Dates
    true
    4f2cc541b520dccf760152a663acda7c
    |}]
;;
