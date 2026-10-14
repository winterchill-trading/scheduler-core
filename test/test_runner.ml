open! Core
open Scheduler_core

(* A payment job that books into a toy ledger keyed by run key, as a real job would pass
   the key on as its idempotency key. *)
module Pay = struct
  type t =
    { amount : int
    ; payee : string
    }
  [@@deriving sexp_of]

  let ledger : (string, int) Hashtbl.t = Hashtbl.create (module String)
  let calls = ref []
  let balance = ref 1_000
  let crash_next = ref false

  let reset ~balance:b =
    Hashtbl.clear ledger;
    calls := [];
    balance := b;
    crash_next := false
  ;;

  let validate _ _ = Ok ()

  let run t (c : Job.Context.t) =
    let key = Run_key.to_string c.key in
    calls := (key, c.attempt) :: !calls;
    match Hashtbl.find ledger key with
    | Some _ -> Ok ("pay-" ^ key) (* replay *)
    | None ->
      if !balance < t.amount
      then Error "insufficient_funds"
      else (
        balance := !balance - t.amount;
        Hashtbl.set ledger ~key ~data:t.amount;
        if !crash_next
        then (
          crash_next := false;
          failwith "process died after booking")
        else Ok ("pay-" ^ key))
  ;;
end

module Store = Store_memory.Make (Monad.Ident) (Pay)
module R = Runner.Make (Monad.Ident) (Pay) (Store)

let d = Date.of_string
let berlin = Time_float_unix.Zone.find_exn "Europe/Berlin"

let setup ?missed_policy ?(balance = 1_000) ~start freq =
  Pay.reset ~balance;
  let manual = Clock.Manual.create ~start:Time_ns.epoch ~zone:berlin in
  Clock.Manual.set_date manual (d start);
  let store = Store.create () in
  let runner = R.create ~store ~clock:(Clock.Manual.clock manual) () in
  let id = Schedule_id.create_exn "rent" in
  let recurrence = Recurrence.create_exn freq ~start:(d start) () in
  (match
     R.add runner ?missed_policy ~id { amount = 100; payee = "landlord" } recurrence
   with
   | Ok _ -> ()
   | Error e -> raise_s [%sexp (e : Runner.Command_error.t)]);
  manual, store, runner, id
;;

let print_executions es =
  List.iter es ~f:(fun e -> print_s [%sexp (e : Runner.Execution.t)])
;;

let print_schedule store id =
  let s = Option.value_exn (Store.find store id) in
  print_s
    [%sexp
      { status : Schedule.Status.t = Schedule.status s
      ; next : Schedule.Occurrence.t option = Schedule.next s
      ; succeeded : int = Schedule.succeeded s
      }]
;;

let%expect_test "one payment per due date, however often the runner ticks" =
  let manual, store, runner, id =
    setup ~start:"2027-01-31" (Monthly { day_of_month = None })
  in
  print_executions (R.tick runner);
  print_executions (R.tick runner);
  Clock.Manual.advance manual (Time_ns.Span.of_hr 5.);
  print_executions (R.tick runner);
  Clock.Manual.set_date manual (d "2027-02-28");
  print_executions (R.tick runner);
  print_schedule store id;
  print_s
    [%sexp
      (Hashtbl.to_alist Pay.ledger |> List.sort ~compare:[%compare: string * int]
       : (string * int) list)];
  [%expect
    {|
    ((key ((schedule rent) (due_date 2027-01-31))) (index 0)
     (status
      (Ran (attempt 1) (outcome (Succeeded (reference pay-rent:2027-01-31))))))
    ((key ((schedule rent) (due_date 2027-02-28))) (index 1)
     (status
      (Ran (attempt 1) (outcome (Succeeded (reference pay-rent:2027-02-28))))))
    ((status Active) (next (((index 2) (date 2027-03-31)))) (succeeded 2))
    ((rent:2027-01-31 100) (rent:2027-02-28 100))
    |}]
;;

let%expect_test "due dates follow the clock's time zone" =
  let manual, _, runner, _ = setup ~start:"2027-03-01" Weekly in
  (* Berlin is UTC+1 in winter: 22:30 UTC is still 28 February there, 23:30 UTC is already
     1 March *)
  Clock.Manual.set manual (Time_ns.of_string_with_utc_offset "2027-02-28 22:30:00Z");
  print_executions (R.tick runner);
  [%expect {| |}];
  Clock.Manual.set manual (Time_ns.of_string_with_utc_offset "2027-02-28 23:30:00Z");
  print_executions (R.tick runner);
  [%expect
    {|
    ((key ((schedule rent) (due_date 2027-03-01))) (index 0)
     (status
      (Ran (attempt 1) (outcome (Succeeded (reference pay-rent:2027-03-01))))))
    |}]
;;

let%expect_test "after a gap only the latest date is paid" =
  let manual, store, runner, id =
    setup ~start:"2027-01-31" (Monthly { day_of_month = None })
  in
  Clock.Manual.set_date manual (d "2027-04-10");
  print_executions (R.tick runner);
  print_schedule store id;
  print_s [%sexp (Hashtbl.length Pay.ledger : int)];
  [%expect
    {|
    ((key ((schedule rent) (due_date 2027-01-31))) (index 0)
     (status Marked_missed))
    ((key ((schedule rent) (due_date 2027-02-28))) (index 1)
     (status Marked_missed))
    ((key ((schedule rent) (due_date 2027-03-31))) (index 2)
     (status
      (Ran (attempt 1) (outcome (Succeeded (reference pay-rent:2027-03-31))))))
    ((status Active) (next (((index 3) (date 2027-04-30)))) (succeeded 1))
    1
    |}]
;;

let%expect_test "a crash after the side effect is replayed with the same key after the \
                 stale limit"
  =
  let manual, store, runner, id = setup ~start:"2027-05-03" Weekly in
  Pay.crash_next := true;
  (match R.tick runner with
   | _ -> print_endline "no crash?"
   | exception exn -> print_s [%message "tick raised" (exn : exn)]);
  (* a fresh claim is left alone *)
  Clock.Manual.advance manual (Time_ns.Span.of_min 1.);
  print_executions (R.tick runner);
  print_s
    [%sexp
      (R.cancel runner id |> Result.map ~f:ignore
       : (unit, Runner.Command_error.t) Result.t)];
  Clock.Manual.advance manual (Time_ns.Span.of_min 2.);
  print_executions (R.tick runner);
  print_schedule store id;
  print_s [%sexp (!Pay.calls : (string * int) list), (!Pay.balance : int)];
  [%expect
    {|
    ("tick raised" (exn (Failure "process died after booking")))
    ((key ((schedule rent) (due_date 2027-05-03))) (index 0) (status Busy))
    (Error Run_in_progress)
    ((key ((schedule rent) (due_date 2027-05-03))) (index 0)
     (status
      (Ran (attempt 2) (outcome (Succeeded (reference pay-rent:2027-05-03))))))
    ((status Active) (next (((index 1) (date 2027-05-10)))) (succeeded 1))
    (((rent:2027-05-03 2) (rent:2027-05-03 1)) 900)
    |}]
;;

let%expect_test "three failed dates in a row pause the schedule; resume skips passed \
                 dates"
  =
  let manual, store, runner, id = setup ~balance:150 ~start:"2027-05-03" Weekly in
  List.iter
    [ "2027-05-03"; "2027-05-10"; "2027-05-17"; "2027-05-24"; "2027-05-31" ]
    ~f:(fun date ->
      Clock.Manual.set_date manual (d date);
      print_executions (R.tick runner));
  print_schedule store id;
  Pay.balance := 1_000;
  Clock.Manual.set_date manual (d "2027-06-09");
  print_schedule store id;
  (match R.resume runner id with
   | Ok _ -> ()
   | Error e -> print_s [%sexp (e : Runner.Command_error.t)]);
  print_schedule store id;
  print_executions (R.tick runner);
  [%expect
    {|
    ((key ((schedule rent) (due_date 2027-05-03))) (index 0)
     (status
      (Ran (attempt 1) (outcome (Succeeded (reference pay-rent:2027-05-03))))))
    ((key ((schedule rent) (due_date 2027-05-10))) (index 1)
     (status (Ran (attempt 1) (outcome (Failed (reason insufficient_funds))))))
    ((key ((schedule rent) (due_date 2027-05-17))) (index 2)
     (status (Ran (attempt 1) (outcome (Failed (reason insufficient_funds))))))
    ((key ((schedule rent) (due_date 2027-05-24))) (index 3)
     (status (Ran (attempt 1) (outcome (Failed (reason insufficient_funds))))))
    ((status (Paused (Failures (count 3) (last_reason insufficient_funds))))
     (next (((index 4) (date 2027-05-31)))) (succeeded 1))
    ((status (Paused (Failures (count 3) (last_reason insufficient_funds))))
     (next (((index 4) (date 2027-05-31)))) (succeeded 1))
    ((status Active) (next (((index 6) (date 2027-06-14)))) (succeeded 1))
    |}]
;;

let%expect_test "skip, cancel and their refusals" =
  let manual, store, runner, id = setup ~start:"2027-05-03" Weekly in
  let show r =
    print_s
      [%sexp
        (Result.map r ~f:(fun s -> Schedule.next s)
         : (Schedule.Occurrence.t option, Runner.Command_error.t) Result.t)]
  in
  show (R.skip runner id);
  print_executions (R.tick runner);
  Clock.Manual.set_date manual (d "2027-05-10");
  print_executions (R.tick runner);
  show (R.cancel runner id);
  show (R.cancel runner id);
  show (R.skip runner id);
  show (R.pause runner (Schedule_id.create_exn "nope"));
  print_s [%sexp (Store.records store id : Run_record.t list)];
  [%expect
    {|
    (Ok (((index 1) (date 2027-05-10))))
    ((key ((schedule rent) (due_date 2027-05-10))) (index 1)
     (status
      (Ran (attempt 1) (outcome (Succeeded (reference pay-rent:2027-05-10))))))
    (Ok ())
    (Error (Refused Already_ended))
    (Error (Refused Already_ended))
    (Error (Unknown_schedule nope))
    (((key ((schedule rent) (due_date 2027-05-03))) (index 0)
      (state
       (Finished (outcome Skipped) (attempts 0)
        (finished_at "2027-05-02 22:00:00Z"))))
     ((key ((schedule rent) (due_date 2027-05-10))) (index 1)
      (state
       (Finished (outcome (Succeeded (reference pay-rent:2027-05-10)))
        (attempts 1) (finished_at "2027-05-09 22:00:00Z")))))
    |}]
;;

let%expect_test "a date whose job may have run is never skipped or passed over" =
  let manual, store, runner, id = setup ~start:"2027-05-03" Weekly in
  Pay.crash_next := true;
  (try ignore (R.tick runner : Runner.Execution.t list) with
   | Failure _ -> ());
  let show r =
    print_s
      [%sexp
        (Result.map r ~f:(fun s -> Schedule.status s)
         : (Schedule.Status.t, Runner.Command_error.t) Result.t)]
  in
  (* the owner pauses and tries to skip or resume past the date: both refused *)
  show (R.pause runner id);
  Clock.Manual.advance manual (Time_ns.Span.of_day 9.);
  show (R.skip runner id);
  show (R.resume runner id);
  (* the next tick replays the stale claim even though the schedule is paused *)
  print_executions (R.tick runner);
  show (R.resume runner id);
  print_schedule store id;
  print_s [%sexp (!Pay.balance : int)];
  [%expect
    {|
    (Ok (Paused By_owner))
    (Error (Refused Not_active))
    (Error Run_in_progress)
    ((key ((schedule rent) (due_date 2027-05-03))) (index 0)
     (status
      (Ran (attempt 2) (outcome (Succeeded (reference pay-rent:2027-05-03))))))
    (Ok Active)
    ((status Active) (next (((index 2) (date 2027-05-17)))) (succeeded 1))
    900
    |}]
;;

let%expect_test "skip on a paused schedule, and a limit counting successful runs" =
  Pay.reset ~balance:250;
  let manual = Clock.Manual.create ~start:Time_ns.epoch ~zone:berlin in
  Clock.Manual.set_date manual (d "2027-05-03");
  let store = Store.create () in
  let runner = R.create ~store ~clock:(Clock.Manual.clock manual) () in
  let id = Schedule_id.create_exn "instalments" in
  let recurrence =
    Recurrence.create_exn Weekly ~start:(d "2027-05-03") ~max_occurrences:2 ()
  in
  let show r =
    print_s
      [%sexp
        (Result.map r ~f:(fun s -> Schedule.status s, Schedule.next s)
         : ( Schedule.Status.t * Schedule.Occurrence.t option
             , Runner.Command_error.t )
             Result.t)]
  in
  show
    (R.add
       runner
       ~max_occurrences_counts:Successful_runs
       ~id
       { amount = 100; payee = "lender" }
       recurrence);
  print_executions (R.tick runner);
  show (R.pause runner id);
  (* A paused schedule is refused unless the caller asks for the paused behaviour. *)
  show (R.skip runner id);
  show (R.skip ~paused:true runner id);
  Clock.Manual.set_date manual (d "2027-05-17");
  show (R.resume runner id);
  print_executions (R.tick runner);
  print_schedule store id;
  [%expect
    {|
    (Ok (Active (((index 0) (date 2027-05-03)))))
    ((key ((schedule instalments) (due_date 2027-05-03))) (index 0)
     (status
      (Ran (attempt 1)
       (outcome (Succeeded (reference pay-instalments:2027-05-03))))))
    (Ok ((Paused By_owner) (((index 1) (date 2027-05-10)))))
    (Error (Refused Not_active))
    (Ok ((Paused By_owner) (((index 2) (date 2027-05-17)))))
    (Ok (Active (((index 2) (date 2027-05-17)))))
    ((key ((schedule instalments) (due_date 2027-05-17))) (index 2)
     (status
      (Ran (attempt 1)
       (outcome (Succeeded (reference pay-instalments:2027-05-17))))))
    ((status Completed) (next ()) (succeeded 2))
    |}]
;;
