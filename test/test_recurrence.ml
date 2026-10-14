open! Core
open Scheduler_core

let d = Date.of_string

let show ?(n = 6) r =
  Recurrence.dates r
  |> Fn.flip Sequence.take n
  |> Sequence.iter ~f:(fun (i, date) ->
    printf "%d %s %s\n" i (Date.to_string date) (Day_of_week.to_string (Date.day_of_week date)))
;;

let%expect_test "monthly on the 31st clamps by index and never drifts" =
  show ~n:7 (Recurrence.create_exn (Monthly { day_of_month = None }) ~start:(d "2027-01-31") ());
  [%expect
    {|
    0 2027-01-31 SUN
    1 2027-02-28 SUN
    2 2027-03-31 WED
    3 2027-04-30 FRI
    4 2027-05-31 MON
    5 2027-06-30 WED
    6 2027-07-31 SAT
    |}];
  (* leap year *)
  show ~n:3 (Recurrence.create_exn (Monthly { day_of_month = None }) ~start:(d "2028-01-31") ());
  [%expect
    {|
    0 2028-01-31 MON
    1 2028-02-29 TUE
    2 2028-03-31 FRI
    |}]
;;

let%expect_test "an explicit day before the start's day starts next month" =
  show
    ~n:3
    (Recurrence.create_exn (Monthly { day_of_month = Some 5 }) ~start:(d "2027-05-20") ());
  [%expect
    {|
    0 2027-06-05 SAT
    1 2027-07-05 MON
    2 2027-08-05 THU
    |}];
  show
    ~n:3
    (Recurrence.create_exn (Monthly { day_of_month = Some 30 }) ~start:(d "2027-02-03") ());
  [%expect
    {|
    0 2027-02-28 SUN
    1 2027-03-30 TUE
    2 2027-04-30 FRI
    |}]
;;

let%expect_test "weekly, fortnightly, every N days and once" =
  show ~n:3 (Recurrence.create_exn Weekly ~start:(d "2027-03-01") ());
  [%expect
    {|
    0 2027-03-01 MON
    1 2027-03-08 MON
    2 2027-03-15 MON
    |}];
  show ~n:3 (Recurrence.create_exn Fortnightly ~start:(d "2027-03-01") ());
  [%expect
    {|
    0 2027-03-01 MON
    1 2027-03-15 MON
    2 2027-03-29 MON
    |}];
  show ~n:3 (Recurrence.create_exn (Every_n_days 10) ~start:(d "2027-03-01") ());
  [%expect
    {|
    0 2027-03-01 MON
    1 2027-03-11 THU
    2 2027-03-21 SUN
    |}];
  show (Recurrence.create_exn Once ~start:(d "2027-03-01") ());
  [%expect {| 0 2027-03-01 MON |}]
;;

let%expect_test "end date and maximum count bound the dates" =
  let r =
    Recurrence.create_exn Weekly ~start:(d "2027-03-01") ~end_date:(d "2027-03-20") ()
  in
  show r;
  print_s [%sexp (Recurrence.last_date r : Date.t option)];
  [%expect
    {|
    0 2027-03-01 MON
    1 2027-03-08 MON
    2 2027-03-15 MON
    (2027-03-15)
    |}];
  let r = Recurrence.create_exn Weekly ~start:(d "2027-03-01") ~max_occurrences:2 () in
  show r;
  print_s [%sexp (Recurrence.last_date r : Date.t option)];
  [%expect
    {|
    0 2027-03-01 MON
    1 2027-03-08 MON
    (2027-03-08)
    |}]
;;

let%expect_test "validation" =
  let start = d "2027-03-10" in
  let try_ ?end_date ?max_occurrences f =
    print_s
      [%sexp
        (Result.map (Recurrence.create f ~start ?end_date ?max_occurrences ()) ~f:ignore
         : (unit, Recurrence.Invalid.t) Result.t)]
  in
  try_ (Every_n_days 0);
  try_ (Every_n_days 367);
  try_ (Monthly { day_of_month = Some 32 });
  try_ Weekly ~max_occurrences:0;
  try_ Weekly ~max_occurrences:1001;
  try_ Weekly ~end_date:(d "2027-03-09");
  try_ (Monthly { day_of_month = Some 5 }) ~end_date:(d "2027-04-01");
  try_ Weekly ~end_date:(d "2027-03-10");
  [%expect
    {|
    (Error (Interval_days_out_of_range 0))
    (Error (Interval_days_out_of_range 367))
    (Error (Day_of_month_out_of_range 32))
    (Error (Max_occurrences_out_of_range 0))
    (Error (Max_occurrences_out_of_range 1001))
    (Error End_before_start)
    (Error No_date_before_end)
    (Ok ())
    |}]
;;

let%expect_test "start date check" =
  let r = Recurrence.create_exn Once ~start:(d "2027-03-10") () in
  let check today =
    print_s
      [%sexp
        (Recurrence.check_start r ~today:(d today) ~max_days_ahead:365
         : (unit, [ `Past | `Too_far ]) Result.t)]
  in
  check "2027-03-11";
  check "2027-03-10";
  check "2026-03-10";
  check "2026-03-09";
  [%expect
    {|
    (Error Past)
    (Ok ())
    (Ok ())
    (Error Too_far)
    |}]
;;

let%expect_test "between and index lookups" =
  let r = Recurrence.create_exn (Monthly { day_of_month = None }) ~start:(d "2027-01-31") () in
  print_s
    [%sexp
      (Recurrence.between r ~from:(d "2027-02-01") ~until:(d "2027-05-30")
       : (int * Date.t) list)];
  print_s
    [%sexp
      ( Recurrence.first_index_on_or_after r (d "2027-03-01")
      , Recurrence.last_index_on_or_before r (d "2027-03-30")
      , Recurrence.last_index_on_or_before r (d "2027-01-30")
        : int option * int option * int option )];
  [%expect
    {|
    ((1 2027-02-28) (2 2027-03-31) (3 2027-04-30))
    ((2) (1) ())
    |}]
;;
