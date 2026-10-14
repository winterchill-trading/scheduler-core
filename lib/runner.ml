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

  let add t ?max_consecutive_failures ?missed_policy ~id job recurrence =
    match Job.validate job recurrence with
    | Error e -> return (Error (Command_error.Invalid_job e))
    | Ok () ->
      (match%bind Store.find t.store id with
       | Some _ -> return (Error (Command_error.Duplicate_schedule id))
       | None ->
         let s =
           Schedule.create ?max_consecutive_failures ?missed_policy ~id ~job recurrence
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
  let next_is_running t s =
    match Schedule.next s with
    | None -> return false
    | Some o ->
      (match%map Store.record t.store (Schedule.key s o) with
       | None -> false
       | Some r ->
         Run_record.is_running r ~now:(Clock.now t.clock) ~stale_after:t.stale_after)
  ;;

  let pause t id = with_schedule t id ~f:(fun s -> save_result t (Schedule.pause s))

  let resume t id =
    with_schedule t id ~f:(fun s ->
      save_result t (Schedule.resume s ~today:(Clock.today t.clock)))
  ;;

  let cancel t id =
    with_schedule t id ~f:(fun s ->
      if%bind next_is_running t s
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

  let skip t id =
    with_schedule t id ~f:(fun s ->
      match Schedule.next_to_skip s with
      | Error r -> return (Error (Command_error.Refused r))
      | Ok o ->
        let key = Schedule.key s o in
        let now = Clock.now t.clock in
        (match%bind
           Store.claim t.store key ~index:o.index ~now ~stale_after:t.stale_after
         with
         | Running _ -> return (Error Command_error.Run_in_progress)
         | Finished r ->
           (* settled meanwhile; catch the schedule up and let the owner retry *)
           let%map _ =
             apply t id ~index:o.index (Option.value_exn (Run_record.outcome r))
           in
           Error Command_error.Run_in_progress
         | Claimed _ ->
           let%bind (_ : Run_record.t) =
             Store.finish t.store key ~index:o.index Skipped ~now
           in
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

  (* Settles one date: [`Missed] marks it missed, [`Run] runs the job. Returns the
     execution and whether to go on with the schedule's later dates. *)
  let settle_date t (s : Job.t Schedule.t) (o : Schedule.Occurrence.t) ~what =
    let id = Schedule.id s in
    let key = Schedule.key s o in
    let execution status = { Execution.key; index = o.index; status } in
    let now = Clock.now t.clock in
    match%bind Store.claim t.store key ~index:o.index ~now ~stale_after:t.stale_after with
    | Running _ -> return (`Stop (execution Busy))
    | Finished r ->
      let outcome = Option.value_exn (Run_record.outcome r) in
      (match%map apply t id ~index:o.index outcome with
       | Some s when is_active s -> `Continue (execution (Already_settled outcome))
       | _ -> `Stop (execution (Already_settled outcome)))
    | Claimed { attempt } ->
      let%bind outcome, status =
        match what with
        | `Missed -> return (Outcome.Missed, Execution.Marked_missed)
        | `Run ->
          let%map result =
            Job.run (Schedule.job s) { Context.key; index = o.index; attempt; now }
          in
          let outcome : Outcome.t =
            match result with
            | Ok reference -> Succeeded { reference }
            | Error reason -> Failed { reason }
          in
          outcome, Execution.Ran { attempt; outcome }
      in
      let%bind record =
        Store.finish t.store key ~index:o.index outcome ~now:(Clock.now t.clock)
      in
      (* If another runner finished first, its outcome is the one that counts. *)
      let outcome = Option.value (Run_record.outcome record) ~default:outcome in
      let status : Execution.status =
        match status with
        | Ran { attempt; _ } -> Ran { attempt; outcome }
        | other -> other
      in
      (match%map apply t id ~index:o.index outcome with
       | Some s when is_active s -> `Continue (execution status)
       | _ -> `Stop (execution status))
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
        then settle_date t s o ~what
        else (* settled by another runner meanwhile *) return `Pass
      | Some _ | None -> return `Halt)
  ;;

  let tick t =
    let%bind active = Store.active t.store in
    let%map per_schedule =
      fold_until active ~init:[] ~f:(fun s ->
        let%map e = process t s in
        `Continue e)
    in
    List.concat per_schedule
  ;;

  let tick_one t id = with_schedule t id ~f:(fun s -> process t s >>| Result.return)
end
