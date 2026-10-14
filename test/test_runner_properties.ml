open! Core
open Scheduler_core

(* A job whose side effects are counted per run key and which can be told to crash after
   its side effect, the worst moment for exactly-once. *)
module Job = struct
  type t = { fail_every : int } [@@deriving sexp_of]

  let effects = Hashtbl.create (module String)
  let first_attempts = Hashtbl.create (module String)
  let crash = ref false
  let validate _ _ = Ok ()

  let run t (c : Job.Context.t) =
    let key = Run_key.to_string c.key in
    if c.attempt = 1 then Hashtbl.incr first_attempts key;
    if t.fail_every > 0 && c.index % t.fail_every = 0
    then Error "refused"
    else (
      if not (Hashtbl.mem effects key) then Hashtbl.set effects ~key ~data:();
      if !crash then failwith "crash" else Ok key)
  ;;
end

module Store = Store_memory.Make (Monad.Ident) (Job)
module R = Runner.Make (Monad.Ident) (Job) (Store)

type op =
  | Advance_minutes of int
  | Advance_days of int
  | Tick of bool (** which of two runners *)
  | Tick_crashing of bool
  | Pause
  | Resume
  | Skip
[@@deriving sexp_of]

let op_gen =
  let open Quickcheck.Generator in
  weighted_union
    [ 3., map (Int.gen_incl 1 10) ~f:(fun m -> Advance_minutes m)
    ; 3., map (Int.gen_incl 1 40) ~f:(fun d -> Advance_days d)
    ; 6., map Bool.quickcheck_generator ~f:(fun b -> Tick b)
    ; 1., map Bool.quickcheck_generator ~f:(fun b -> Tick_crashing b)
    ; 0.5, return Pause
    ; 1., return Resume
    ; 0.5, return Skip
    ]
;;

let case_gen =
  let open Quickcheck.Generator.Let_syntax in
  let%map r = Gen.recurrence
  and policy =
    Quickcheck.Generator.of_list [ Schedule.Missed_policy.Latest_only; Catch_up ]
  and fail_every = Int.gen_incl 0 4
  and ops = List.gen_with_length 40 op_gen in
  r, policy, fail_every, ops
;;

let%test_unit "each date settles once; succeeded dates had one side effect" =
  Quickcheck.test
    case_gen
    ~trials:300
    ~sexp_of:[%sexp_of: Recurrence.t * Schedule.Missed_policy.t * int * op list]
    ~f:(fun (r, missed_policy, fail_every, ops) ->
      Hashtbl.clear Job.effects;
      Hashtbl.clear Job.first_attempts;
      let zone = Time_float.Zone.utc in
      let manual = Clock.Manual.create ~start:Time_ns.epoch ~zone in
      Clock.Manual.set_date manual (Recurrence.start r);
      let store = Store.create () in
      let clock = Clock.Manual.clock manual in
      let r1 = R.create ~store ~clock () in
      let r2 = R.create ~store ~clock () in
      let id = Schedule_id.create_exn "q" in
      ignore (R.add r1 ~missed_policy ~id { fail_every } r : _ Result.t);
      let succeeded_runs = Hashtbl.create (module String) in
      List.iter ops ~f:(fun op ->
        let tick runner =
          match R.tick runner with
          | executions ->
            (* replays of stale claims (attempt > 1) come on top of the one new date *)
            let ran =
              List.filter executions ~f:(fun e ->
                match e.status with
                | Ran { attempt = 1; _ } -> true
                | _ -> false)
            in
            (match missed_policy with
             | Latest_only -> assert (List.length ran <= 1)
             | Catch_up -> ());
            List.iter executions ~f:(fun e ->
              match e.status with
              | Ran { outcome = Succeeded _; _ } ->
                Hashtbl.incr succeeded_runs (Run_key.to_string e.key)
              | _ -> ())
          | exception Failure _ -> ()
        in
        match op with
        | Advance_minutes m ->
          Clock.Manual.advance manual (Time_ns.Span.of_int_sec (60 * m))
        | Advance_days d ->
          Clock.Manual.advance manual (Time_ns.Span.of_day (Float.of_int d))
        | Tick b -> tick (if b then r1 else r2)
        | Tick_crashing b ->
          Job.crash := true;
          tick (if b then r1 else r2);
          Job.crash := false
        | Pause -> ignore (R.pause r1 id : _ Result.t)
        | Resume -> ignore (R.resume r2 id : _ Result.t)
        | Skip -> ignore (R.skip r1 id : _ Result.t));
      (* the job ran its first attempt at most once per date *)
      Hashtbl.iter Job.first_attempts ~f:(fun n -> assert (n = 1));
      (* a date reported as succeeded by a runner was reported once *)
      Hashtbl.iter succeeded_runs ~f:(fun n -> assert (n = 1));
      let records = Store.records store id in
      let s = Option.value_exn (Store.find store id) in
      (* the schedule's success count matches the records it has settled *)
      let settled_successes =
        List.count records ~f:(fun rr ->
          rr.index < Schedule.next_index s
          &&
          match Run_record.outcome rr with
          | Some (Succeeded _) -> true
          | _ -> false)
      in
      [%test_result: int] (Schedule.succeeded s) ~expect:settled_successes;
      (* every record is for a real date of the recurrence *)
      List.iter records ~f:(fun rr ->
        [%test_result: Date.t option]
          (Recurrence.date r rr.index)
          ~expect:(Some rr.key.due_date);
        match Run_record.outcome rr with
        | Some (Succeeded _) ->
          assert (Hashtbl.mem Job.effects (Run_key.to_string rr.key))
        | Some (Missed | Skipped) ->
          assert (not (Hashtbl.mem Job.effects (Run_key.to_string rr.key)))
        | Some (Failed _) | None -> ()))
;;
