open! Core
open Scheduler_core

(* Pinned bin_prot shape digests. A change here means stored records written by an earlier
   version can no longer be read: add a V2 instead of editing V1. *)
let%expect_test "stable digests" =
  print_endline [%bin_digest: Schedule_id.Stable.V1.t];
  print_endline [%bin_digest: Frequency.Stable.V1.t];
  print_endline [%bin_digest: Recurrence.Stable.V1.t];
  print_endline [%bin_digest: Run_key.Stable.V1.t];
  print_endline [%bin_digest: Outcome.Stable.V1.t];
  print_endline [%bin_digest: Run_record.Stable.V1.t];
  print_endline [%bin_digest: unit Schedule.Stable.V1.t];
  [%expect
    {|
    d9a8da25d5656b016fb4dbdc2e4197fb
    5a6165d6bc8d8e9c36bae9127808bf37
    0aeaf407721ef77103981bc5ec7ccb99
    77f7ed6ed1cf6bd4866d058c2e31b339
    7ddb69cbfaf347f0c2396d9fb8aa5e69
    35d0c114b591cec781b5048028698222
    54fd0457efcc6bb3b5cd07b6ba3e9939
    |}]
;;

let%expect_test "sexp forms" =
  let d = Date.of_string in
  let r =
    Recurrence.create_exn
      (Monthly { day_of_month = Some 31 })
      ~start:(d "2027-01-31")
      ~max_occurrences:12
      ()
  in
  print_s [%sexp (r : Recurrence.Stable.V1.t)];
  let s = Schedule.create ~id:(Schedule_id.create_exn "s-1") ~job:"rent" r in
  print_s [%sexp (s : string Schedule.Stable.V1.t)];
  print_endline (Run_key.to_string (Schedule.key s { index = 0; date = d "2027-01-31" }));
  [%expect
    {|
    ((frequency (Monthly (day_of_month (31)))) (start 2027-01-31)
     (max_occurrences 12))
    ((id s-1) (job rent)
     (recurrence
      ((frequency (Monthly (day_of_month (31)))) (start 2027-01-31)
       (max_occurrences 12)))
     (status Active) (missed_policy Latest_only) (max_consecutive_failures 3)
     (next_index 0) (succeeded 0) (consecutive_failures 0))
    s-1:2027-01-31
    |}]
;;

let%expect_test "reading validates" =
  let show sexp =
    print_s
      [%sexp
        (Or_error.try_with (fun () ->
           Recurrence.Stable.V1.t_of_sexp (Sexp.of_string sexp))
         : Recurrence.t Or_error.t)]
  in
  show "((frequency (Every_n_days 0)) (start 2027-01-01))";
  show "((frequency Weekly) (start 2027-01-08) (end_date 2027-01-01))";
  print_s
    [%sexp
      (Or_error.try_with (fun () ->
         Schedule_id.Stable.V1.t_of_sexp (Sexp.of_string "a:b"))
       : Schedule_id.t Or_error.t)];
  print_s [%sexp (Run_key.of_string "rent:2027-02-28" : Run_key.t Or_error.t)];
  print_s [%sexp (Run_key.of_string "rent-2027-02-28" : Run_key.t Or_error.t)];
  [%expect
    {|
    (Error ("invalid recurrence" (e (Interval_days_out_of_range 0))))
    (Error ("invalid recurrence" (e End_before_start)))
    (Error ("invalid schedule id" (s a:b)))
    (Ok ((schedule rent) (due_date 2027-02-28)))
    (Error ("run key without ':'" (s rent-2027-02-28)))
    |}]
;;
