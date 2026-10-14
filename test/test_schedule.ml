open! Core
open Scheduler_core

let d = Date.of_string
let id = Schedule_id.create_exn "s1"

let monthly_31 ?missed_policy () =
  Schedule.create
    ?missed_policy
    ~id
    ~job:()
    (Recurrence.create_exn (Monthly { day_of_month = None }) ~start:(d "2027-01-31") ())
;;

let show t =
  print_s
    [%sexp
      { status : Schedule.Status.t = Schedule.status t
      ; next : Schedule.Occurrence.t option = Schedule.next t
      ; succeeded : int = Schedule.succeeded t
      ; consecutive_failures : int = Schedule.consecutive_failures t
      }]
;;

let show_due t today = print_s [%sexp (Schedule.due t ~today:(d today) : Schedule.Due.t)]
let ok = function
  | Ok t -> t
  | Error r -> raise_s [%sexp (r : Schedule.Refusal.t)]
;;

let%expect_test "nothing is due before the first date; the date is due on its day" =
  let t = monthly_31 () in
  show_due t "2027-01-30";
  show_due t "2027-01-31";
  [%expect
    {|
    ((run ()) (missed ()))
    ((run (((index 0) (date 2027-01-31)))) (missed ()))
    |}]
;;

let%expect_test "after a gap only the latest date runs; earlier ones are missed" =
  let t = monthly_31 () in
  show_due t "2027-04-02";
  [%expect
    {|
    ((run (((index 2) (date 2027-03-31))))
     (missed (((index 0) (date 2027-01-31)) ((index 1) (date 2027-02-28)))))
    |}];
  let t = monthly_31 ~missed_policy:Catch_up () in
  show_due t "2027-04-02";
  [%expect
    {|
    ((run
      (((index 0) (date 2027-01-31)) ((index 1) (date 2027-02-28))
       ((index 2) (date 2027-03-31))))
     (missed ()))
    |}]
;;

let%expect_test "three failures in a row pause the schedule" =
  let t = monthly_31 () in
  let t = Schedule.settle t ~index:0 (Failed { reason = "insufficient_funds" }) in
  let t = Schedule.settle t ~index:1 (Succeeded { reference = "p1" }) in
  show t;
  [%expect
    {|
    ((status Active) (next (((index 2) (date 2027-03-31)))) (succeeded 1)
     (consecutive_failures 0))
    |}];
  let t =
    List.fold [ 2; 3; 4 ] ~init:t ~f:(fun t index ->
      Schedule.settle t ~index (Failed { reason = "insufficient_funds" }))
  in
  show t;
  [%expect
    {|
    ((status (Paused (Failures (count 3) (last_reason insufficient_funds))))
     (next (((index 5) (date 2027-06-30)))) (succeeded 1)
     (consecutive_failures 3))
    |}];
  (* a replayed result for a settled date changes nothing *)
  [%test_result: unit Schedule.t]
    (Schedule.settle t ~index:3 (Succeeded { reference = "late" }))
    ~expect:t
;;

let%expect_test "resume does not pay dates that passed while paused" =
  let t = monthly_31 () |> Schedule.pause |> ok in
  show t;
  let t = Schedule.resume t ~today:(d "2027-04-15") |> ok in
  show t;
  show_due t "2027-04-15";
  [%expect
    {|
    ((status (Paused By_owner)) (next (((index 0) (date 2027-01-31))))
     (succeeded 0) (consecutive_failures 0))
    ((status Active) (next (((index 3) (date 2027-04-30)))) (succeeded 0)
     (consecutive_failures 0))
    ((run ()) (missed ()))
    |}]
;;

let%expect_test "refusals" =
  let t = monthly_31 () in
  let print r = print_s [%sexp (Result.map r ~f:ignore : (unit, Schedule.Refusal.t) Result.t)] in
  print (Schedule.resume t ~today:(d "2027-01-01"));
  let paused = Schedule.pause t |> ok in
  print (Schedule.pause paused);
  print (Result.map (Schedule.next_to_skip paused) ~f:ignore);
  let cancelled = Schedule.cancel paused |> ok in
  print (Schedule.cancel cancelled);
  print (Schedule.pause cancelled);
  print (Schedule.resume cancelled ~today:(d "2027-01-01"));
  [%expect
    {|
    (Error Not_paused)
    (Error Not_active)
    (Error Not_active)
    (Error Already_ended)
    (Error Already_ended)
    (Error Already_ended)
    |}]
;;

let%expect_test "a once schedule completes after its date; skip settles the next date" =
  let t =
    Schedule.create ~id ~job:() (Recurrence.create_exn Once ~start:(d "2027-05-01") ())
  in
  let o = Schedule.next_to_skip t |> ok in
  print_s [%sexp (o : Schedule.Occurrence.t)];
  let t = Schedule.settle t ~index:o.index Skipped in
  show t;
  print_s
    [%sexp (Result.map (Schedule.next_to_skip t) ~f:ignore : (unit, Schedule.Refusal.t) Result.t)];
  [%expect
    {|
    ((index 0) (date 2027-05-01))
    ((status Completed) (next ()) (succeeded 0) (consecutive_failures 0))
    (Error Already_ended)
    |}]
;;

let%expect_test "resume after the last date completes" =
  let t =
    Schedule.create
      ~id
      ~job:()
      (Recurrence.create_exn Weekly ~start:(d "2027-05-01") ~max_occurrences:2 ())
  in
  let t = Schedule.pause t |> ok in
  show (Schedule.resume t ~today:(d "2027-06-01") |> ok);
  [%expect {| ((status Completed) (next ()) (succeeded 0) (consecutive_failures 0)) |}]
;;

let%expect_test "upcoming starts at the next unsettled date" =
  let t = monthly_31 () in
  let t = Schedule.settle t ~index:0 (Succeeded { reference = "p" }) in
  print_s
    [%sexp
      (Schedule.upcoming t ~from:(d "2027-01-01") ~until:(d "2027-05-01")
       : Schedule.Occurrence.t list)];
  [%expect
    {|
    (((index 1) (date 2027-02-28)) ((index 2) (date 2027-03-31))
     ((index 3) (date 2027-04-30)))
    |}]
;;
