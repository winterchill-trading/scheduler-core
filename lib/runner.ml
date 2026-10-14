open! Core

module Execution = struct
  type status =
    | Ran of
        { attempt : int
        ; outcome : Outcome.t
        }
    | Already_settled of Outcome.t
    | Marked_missed
    | Busy
  [@@deriving sexp_of]

  type t =
    { key : Run_key.t
    ; index : int
    ; status : status
    }
  [@@deriving sexp_of]
end

module Command_error = struct
  type t =
    | Unknown_schedule of Schedule_id.t
    | Duplicate_schedule of Schedule_id.t
    | Invalid_job of Error.t
    | Refused of Schedule.Refusal.t
    | Run_in_progress
  [@@deriving sexp_of]
end

module Context = Job.Context

let default_stale_after = Time_ns.Span.of_int_sec 120

module Make
    (M : Monad.S)
    (Job : Job.S with type 'a io := 'a M.t)
    (Store : Store.S with type 'a io := 'a M.t and type job := Job.t) =
struct
  module Execution = Execution
  module Command_error = Command_error
  open M.Let_syntax

  type t =
    { store : Store.t
    ; clock : Clock.t
    ; stale_after : Time_ns.Span.t
    }

  let create ?(stale_after = default_stale_after) ~store ~clock () =
    { store; clock; stale_after }
  ;;

  let store t = t.store
  let clock t = t.clock

  let add
    t
    ?max_consecutive_failures
    ?missed_policy
    ?max_occurrences_counts
    ~id
    job
    recurrence
    =
    match Job.validate job recurrence with
    | Error e -> return (Error (Command_error.Invalid_job e))
    | Ok () ->
      (match%bind Store.find t.store id with
       | Some _ -> return (Error (Command_error.Duplicate_schedule id))
       | None ->
         let s =
           Schedule.create
             ?max_consecutive_failures
             ?missed_policy
             ?max_occurrences_counts
             ~id
             ~job
             recurrence
         in
         let%map () = Store.save t.store s in
         Ok s)
  ;;

  let with_schedule t id ~f =
    match%bind Store.find t.store id with
    | None -> return (Error (Command_error.Unknown_schedule id))
    | Some s -> f s
  ;;

  let save_result t = function
    | Error r -> return (Error (Command_error.Refused r))
    | Ok s ->
      let%map () = Store.save t.store s in
      Ok s
  ;;

  (* Whether a runner holds a fresh claim on the schedule's next date. *)
  (* Whether the schedule's next date carries an unfinished run claim, fresh or stale. The
     job may have done its work under that claim, so nothing may move the schedule past
     the date until a runner has replayed the job and recorded the outcome. *)
  let next_is_claimed t s =
    match Schedule.next s with
    | None -> return false
    | Some o ->
      (match%map Store.record t.store (Schedule.key s o) with
       | Some { state = Claimed _; _ } -> true
       | Some { state = Finished _; _ } | None -> false)
  ;;

  let pause t id = with_schedule t id ~f:(fun s -> save_result t (Schedule.pause s))

  let resume t id =
    with_schedule t id ~f:(fun s ->
      if%bind next_is_claimed t s
      then return (Error Command_error.Run_in_progress)
      else save_result t (Schedule.resume s ~today:(Clock.today t.clock)))
  ;;

  let cancel t id =
    with_schedule t id ~f:(fun s ->
      if%bind next_is_claimed t s
      then return (Error Command_error.Run_in_progress)
      else save_result t (Schedule.cancel s))
  ;;

  (* Applies a settled outcome to the latest saved copy of the schedule. *)
  let apply t id ~index outcome =
    match%bind Store.find t.store id with
    | None -> return None
    | Some s ->
      let s = Schedule.settle s ~index outcome in
      let%map () = Store.save t.store s in
      Some s
  ;;

  let skip ?(paused = false) t id =
    with_schedule t id ~f:(fun s ->
      let today = if paused then Some (Clock.today t.clock) else None in
      match Schedule.next_to_skip ?today s with
      | Error r -> return (Error (Command_error.Refused r))
      | Ok o ->
        let key = Schedule.key s o in
        (match%bind
           Store.settle_unclaimed
             t.store
             key
             ~index:o.index
             Skipped
             ~now:(Clock.now t.clock)
         with
         | `Claimed _ -> return (Error Command_error.Run_in_progress)
         | `Finished r ->
           (* settled meanwhile; catch the schedule up and let the owner look again *)
           let%map (_ : Job.t Schedule.t option) =
             apply t id ~index:o.index (Option.value_exn (Run_record.outcome r))
           in
           Error Command_error.Run_in_progress
         | `Settled _ ->
           (match%map apply t id ~index:o.index Skipped with
            | Some s -> Ok s
            | None -> Error (Command_error.Unknown_schedule id))))
  ;;

  let update_job t id job =
    with_schedule t id ~f:(fun s ->
      if Schedule.Status.is_ended (Schedule.status s)
      then return (Error (Command_error.Refused Already_ended))
      else (
        match Job.validate job (Schedule.recurrence s) with
        | Error e -> return (Error (Command_error.Invalid_job e))
        | Ok () -> save_result t (Ok (Schedule.with_job s job))))
  ;;

  let rec fold_until l ~init ~f =
    match l with
    | [] -> return (List.rev init)
    | x :: rest ->
      (match%bind f x with
       | `Continue e -> fold_until rest ~init:(e :: init) ~f
       | `Stop e -> return (List.rev (e :: init))
       | `Pass -> fold_until rest ~init ~f
       | `Halt -> return (List.rev init))
  ;;

  let is_active s = Schedule.Status.equal (Schedule.status s) Active

  let continue_if_active t id ~index outcome execution =
    match%map apply t id ~index outcome with
    | Some s when is_active s -> `Continue execution
    | Some _ | None -> `Stop execution
  ;;

  (* Claims a date and runs its job (a replay when the claim was taken over). *)
  let run_date t (s : Job.t Schedule.t) (o : Schedule.Occurrence.t) =
    let id = Schedule.id s in
    let key = Schedule.key s o in
    let execution status = { Execution.key; index = o.index; status } in
    let now = Clock.now t.clock in
    match%bind Store.claim t.store key ~index:o.index ~now ~stale_after:t.stale_after with
    | Running _ -> return (`Stop (execution Busy))
    | Finished r ->
      let outcome = Option.value_exn (Run_record.outcome r) in
      continue_if_active t id ~index:o.index outcome (execution (Already_settled outcome))
    | Claimed { attempt } ->
      let%bind result =
        Job.run (Schedule.job s) { Context.key; index = o.index; attempt; now }
      in
      let outcome : Outcome.t =
        match result with
        | Ok reference -> Succeeded { reference }
        | Error reason -> Failed { reason }
      in
      let%bind record =
        Store.finish t.store key ~index:o.index outcome ~now:(Clock.now t.clock)
      in
      (* If another runner finished first, its outcome is the one that counts. *)
      let outcome = Option.value (Run_record.outcome record) ~default:outcome in
      continue_if_active
        t
        id
        ~index:o.index
        outcome
        (execution (Ran { attempt; outcome }))
  ;;

  (* Marks a passed-over date missed, unless a run claim exists on it: then the job may
     have run, so the date is run (replayed) instead once the claim is stale. *)
  let miss_date t (s : Job.t Schedule.t) (o : Schedule.Occurrence.t) =
    let id = Schedule.id s in
    let key = Schedule.key s o in
    let execution status = { Execution.key; index = o.index; status } in
    let now = Clock.now t.clock in
    match%bind Store.settle_unclaimed t.store key ~index:o.index Missed ~now with
    | `Settled _ ->
      continue_if_active t id ~index:o.index Missed (execution Marked_missed)
    | `Finished r ->
      let outcome = Option.value_exn (Run_record.outcome r) in
      continue_if_active t id ~index:o.index outcome (execution (Already_settled outcome))
    | `Claimed r ->
      if Run_record.is_running r ~now ~stale_after:t.stale_after
      then return (`Stop (execution Busy))
      else run_date t s o
  ;;

  let process t s =
    let { Schedule.Due.run; missed } = Schedule.due s ~today:(Clock.today t.clock) in
    let dates =
      List.map missed ~f:(fun o -> o, `Missed) @ List.map run ~f:(fun o -> o, `Run)
    in
    fold_until dates ~init:[] ~f:(fun (o, what) ->
      (* each date works on the latest saved schedule *)
      match%bind Store.find t.store (Schedule.id s) with
      | Some s when is_active s ->
        if Schedule.next_index s <= o.index
        then (
          match what with
          | `Missed -> miss_date t s o
          | `Run -> run_date t s o)
        else (* settled by another runner meanwhile *) return `Pass
      | Some _ | None -> return `Halt)
  ;;

  (* Stale claims on schedules that are not active (paused while their job ran) are
     replayed here; active schedules reach theirs through [process]. *)
  let resolve_stale_claims t =
    let now = Clock.now t.clock in
    let%bind claims = Store.claimed t.store in
    let stale =
      List.filter claims ~f:(fun r ->
        not (Run_record.is_running r ~now ~stale_after:t.stale_after))
    in
    fold_until stale ~init:[] ~f:(fun (r : Run_record.t) ->
      match%bind Store.find t.store r.key.schedule with
      | Some s when (not (is_active s)) && Schedule.next_index s <= r.index ->
        (match%map run_date t s { index = r.index; date = r.key.due_date } with
         | `Continue e | `Stop e -> `Continue e
         | (`Pass | `Halt) as other -> other)
      | Some _ | None -> return `Pass)
  ;;

  let tick t =
    let%bind resolved = resolve_stale_claims t in
    let%bind active = Store.active t.store in
    let%map per_schedule =
      fold_until active ~init:[] ~f:(fun s ->
        let%map e = process t s in
        `Continue e)
    in
    resolved @ List.concat per_schedule
  ;;

  let tick_one t id = with_schedule t id ~f:(fun s -> process t s >>| Result.return)
end
