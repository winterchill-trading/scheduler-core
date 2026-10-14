(* Three kinds of recurring work on one runner: a monthly standing order, a weekly
   recurring buy and a fortnightly reminder. Each kind only says how to run one date;
   dates, claims, missed dates and the failure pause come from scheduler-core. *)

open! Core
open Scheduler_core

module Work = struct
  type t =
    | Standing_order of
        { beneficiary : string
        ; cents : int
        }
    | Recurring_buy of
        { instrument : string
        ; cents : int
        }
    | Reminder of { text : string }
  [@@deriving sexp_of]

  let balance = ref 2_500_00

  (* Recurring buys are refused on anything but weekly or monthly dates. *)
  let validate t recurrence =
    match t, Recurrence.frequency recurrence with
    | Recurring_buy _, (Weekly | Monthly _) | (Standing_order _ | Reminder _), _ -> Ok ()
    | Recurring_buy _, _ -> Or_error.error_string "recurring buys run weekly or monthly"
  ;;

  (* A real job passes [Run_key.to_string c.key] on as its idempotency key. *)
  let run t (c : Job.Context.t) =
    let debit cents what =
      if !balance < cents
      then Error "insufficient_funds"
      else (
        balance := !balance - cents;
        Ok (sprintf "%s-%s" what (Run_key.to_string c.key)))
    in
    match t with
    | Standing_order { cents; _ } -> debit cents "payment"
    | Recurring_buy { cents; _ } -> debit cents "order"
    | Reminder _ -> Ok ("message-" ^ Run_key.to_string c.key)
  ;;
end

module Store = Store_memory.Make (Monad.Ident) (Work)
module Runner = Runner.Make (Monad.Ident) (Work) (Store)

let d = Date.of_string

let () =
  let zone = Time_float.Zone.utc in
  let clock = Clock.Manual.create ~start:Time_ns.epoch ~zone in
  Clock.Manual.set_date clock (d "2027-01-31");
  let store = Store.create () in
  let runner = Runner.create ~store ~clock:(Clock.Manual.clock clock) () in
  let add id work frequency =
    match
      Runner.add
        runner
        ~id:(Schedule_id.create_exn id)
        work
        (Recurrence.create_exn frequency ~start:(d "2027-01-31") ())
    with
    | Ok _ -> printf "added %s\n" id
    | Error e -> print_s [%message "refused" id (e : Runner.Command_error.t)]
  in
  add
    "rent"
    (Standing_order { beneficiary = "landlord"; cents = 900_00 })
    (Monthly { day_of_month = None });
  add "etf" (Recurring_buy { instrument = "world-etf"; cents = 50_00 }) Weekly;
  add
    "etf-daily"
    (Recurring_buy { instrument = "world-etf"; cents = 5_00 })
    (Every_n_days 1);
  add "plants" (Reminder { text = "water the plants" }) Fortnightly;
  List.iter
    [ "2027-01-31"; "2027-02-07"; "2027-02-28"; "2027-03-31"; "2027-04-30" ]
    ~f:(fun day ->
      Clock.Manual.set_date clock (d day);
      printf "-- %s\n" day;
      List.iter (Runner.tick runner) ~f:(fun e ->
        let status =
          match e.status with
          | Ran { outcome = Succeeded { reference }; _ } -> "ok " ^ reference
          | Ran { outcome = Failed { reason }; _ } -> "failed " ^ reason
          | Marked_missed -> "missed"
          | other -> Sexp.to_string [%sexp (other : Runner.Execution.status)]
        in
        printf "%s %s\n" (Run_key.to_string e.key) status));
  printf "-- schedules\n";
  List.iter (Store.all store) ~f:(fun s ->
    printf
      "%s %s, next %s\n"
      (Schedule_id.to_string (Schedule.id s))
      (Sexp.to_string [%sexp (Schedule.status s : Schedule.Status.t)])
      (Option.value_map (Schedule.next s) ~default:"-" ~f:(fun o -> Date.to_string o.date)))
;;
