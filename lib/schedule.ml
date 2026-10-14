open! Core

(* The persisted shapes; [Stable] at the end of the file exposes them. *)
module Repr = struct
  module V1 = struct
    module Missed_policy = struct
      type t =
        | Latest_only
        | Catch_up
      [@@deriving bin_io, sexp, compare, equal, hash]
    end

    module Pause_reason = struct
      type t =
        | By_owner
        | Failures of
            { count : int
            ; last_reason : string
            }
      [@@deriving bin_io, sexp, compare, equal, hash]
    end

    module Status = struct
      type t =
        | Active
        | Paused of Pause_reason.t
        | Completed
        | Cancelled
      [@@deriving bin_io, sexp, compare, equal, hash]
    end

    type 'job t =
      { id : Schedule_id.Stable.V1.t
      ; job : 'job
      ; recurrence : Recurrence.Stable.V1.t
      ; status : Status.t
      ; missed_policy : Missed_policy.t
      ; max_consecutive_failures : int
      ; next_index : int
      ; succeeded : int
      ; consecutive_failures : int
      }
    [@@deriving bin_io, sexp, compare, equal]
  end

  module V2 = struct
    module Max_occurrences_counts = struct
      type t =
        | Dates
        | Successful_runs
      [@@deriving bin_io, sexp, compare, equal, hash]
    end

    type 'job t =
      { id : Schedule_id.Stable.V1.t
      ; job : 'job
      ; recurrence : Recurrence.Stable.V1.t
      ; status : V1.Status.t
      ; missed_policy : V1.Missed_policy.t
      ; max_consecutive_failures : int
      ; next_index : int
      ; succeeded : int
      ; consecutive_failures : int
      ; max_occurrences_counts : Max_occurrences_counts.t
           [@sexp.default Dates] [@sexp_drop_default.equal]
      }
    [@@deriving bin_io, sexp, compare, equal]
  end
end

module Missed_policy = Repr.V1.Missed_policy
module Pause_reason = Repr.V1.Pause_reason
module Max_occurrences_counts = Repr.V2.Max_occurrences_counts

module Status = struct
  include Repr.V1.Status

  let name = function
    | Active -> "active"
    | Paused _ -> "paused"
    | Completed -> "completed"
    | Cancelled -> "cancelled"
  ;;

  let is_ended = function
    | Completed | Cancelled -> true
    | Active | Paused _ -> false
  ;;
end

module Refusal = struct
  type t =
    | Not_active
    | Not_paused
    | Already_ended
    | Nothing_to_skip
  [@@deriving sexp, compare, equal]
end

module Occurrence = struct
  type t =
    { index : int
    ; date : Date.t
    }
  [@@deriving sexp, compare, equal]
end

type 'job t = 'job Repr.V2.t =
  { id : Schedule_id.t
  ; job : 'job
  ; recurrence : Recurrence.t
  ; status : Status.t
  ; missed_policy : Missed_policy.t
  ; max_consecutive_failures : int
  ; next_index : int
  ; succeeded : int
  ; consecutive_failures : int
  ; max_occurrences_counts : Max_occurrences_counts.t
       [@sexp.default Dates] [@sexp_drop_default.equal]
  }
[@@deriving sexp, compare, equal]

let of_v1 (v1 : _ Repr.V1.t) =
  { id = v1.id
  ; job = v1.job
  ; recurrence = v1.recurrence
  ; status = v1.status
  ; missed_policy = v1.missed_policy
  ; max_consecutive_failures = v1.max_consecutive_failures
  ; next_index = v1.next_index
  ; succeeded = v1.succeeded
  ; consecutive_failures = v1.consecutive_failures
  ; max_occurrences_counts = Dates
  }
;;

let to_v1 (t : _ t) : _ Repr.V1.t =
  (match t.max_occurrences_counts with
   | Dates -> ()
   | Successful_runs ->
     raise_s
       [%message
         "Schedule.Stable.V1 cannot hold a schedule counting successful runs; use V2"
           ~id:(t.id : Schedule_id.t)]);
  { id = t.id
  ; job = t.job
  ; recurrence = t.recurrence
  ; status = t.status
  ; missed_policy = t.missed_policy
  ; max_consecutive_failures = t.max_consecutive_failures
  ; next_index = t.next_index
  ; succeeded = t.succeeded
  ; consecutive_failures = t.consecutive_failures
  }
;;

let default_max_consecutive_failures = 3

let create
  ?(max_consecutive_failures = default_max_consecutive_failures)
  ?(missed_policy = Missed_policy.Latest_only)
  ?(max_occurrences_counts = Max_occurrences_counts.Dates)
  ~id
  ~job
  recurrence
  =
  if max_consecutive_failures < 1
  then
    raise_s
      [%message
        "max_consecutive_failures must be at least 1" (max_consecutive_failures : int)];
  { id
  ; job
  ; recurrence
  ; status = Active
  ; missed_policy
  ; max_consecutive_failures
  ; next_index = 0
  ; succeeded = 0
  ; consecutive_failures = 0
  ; max_occurrences_counts
  }
;;

let id t = t.id
let job t = t.job
let recurrence t = t.recurrence
let status t = t.status
let missed_policy t = t.missed_policy
let max_consecutive_failures t = t.max_consecutive_failures
let next_index t = t.next_index
let succeeded t = t.succeeded
let consecutive_failures t = t.consecutive_failures
let max_occurrences_counts t = t.max_occurrences_counts
let with_job t job = { t with job }
let key t (o : Occurrence.t) = Run_key.create t.id o.date

(* When successful runs are counted, the dates themselves are unbounded by
   [max_occurrences] (only [end_date] ends them); the count ends the schedule instead. *)
let dates t =
  match t.max_occurrences_counts with
  | Dates -> t.recurrence
  | Successful_runs -> Recurrence.without_max_occurrences t.recurrence
;;

let runs_left t =
  match t.max_occurrences_counts, Recurrence.max_occurrences t.recurrence with
  | Dates, _ | Successful_runs, None -> None
  | Successful_runs, Some max -> Some (Int.max 0 (max - t.succeeded))
;;

let occurrence t index =
  if Option.equal Int.equal (runs_left t) (Some 0)
  then None
  else
    Option.map
      (Recurrence.date (dates t) index)
      ~f:(fun date -> { Occurrence.index; date })
;;

let next t = if Status.is_ended t.status then None else occurrence t t.next_index

module Due = struct
  type t =
    { run : Occurrence.t list
    ; missed : Occurrence.t list
    }
  [@@deriving sexp, compare, equal]

  let empty = { run = []; missed = [] }
end

let unsettled_until t ~today =
  match Recurrence.last_index_on_or_before (dates t) today with
  | Some last when last >= t.next_index ->
    List.range t.next_index last ~stop:`inclusive |> List.filter_map ~f:(occurrence t)
  | _ -> []
;;

let due t ~today =
  match t.status with
  | Paused _ | Completed | Cancelled -> Due.empty
  | Active ->
    let passed = unsettled_until t ~today in
    (match t.missed_policy, List.rev passed with
     | _, [] -> Due.empty
     | Catch_up, _ -> { run = passed; missed = [] }
     | Latest_only, latest :: earlier -> { run = [ latest ]; missed = List.rev earlier })
;;

let complete_if_done t =
  match t.status with
  | Active | Paused _ ->
    if Option.is_none (occurrence t t.next_index)
    then { t with status = Completed }
    else t
  | Completed | Cancelled -> t
;;

let settle t ~index (outcome : Outcome.t) =
  if index < t.next_index
  then t
  else (
    let t = { t with next_index = index + 1 } in
    let t =
      match outcome with
      | Succeeded _ -> { t with succeeded = t.succeeded + 1; consecutive_failures = 0 }
      | Missed | Skipped -> { t with consecutive_failures = 0 }
      | Failed { reason } ->
        let count = t.consecutive_failures + 1 in
        let t = { t with consecutive_failures = count } in
        (match t.status with
         | Active when count >= t.max_consecutive_failures ->
           { t with status = Paused (Failures { count; last_reason = reason }) }
         | _ -> t)
    in
    complete_if_done t)
;;

let pause t =
  match t.status with
  | Active -> Ok { t with status = Paused By_owner }
  | Paused _ -> Error Refusal.Not_active
  | Completed | Cancelled -> Error Refusal.Already_ended
;;

let resume t ~today =
  match t.status with
  | Paused _ ->
    (match Recurrence.first_index_on_or_after (dates t) today with
     | None -> Ok { t with status = Completed; consecutive_failures = 0 }
     | Some i ->
       let next_index = Int.max i t.next_index in
       Ok
         (complete_if_done
            { t with status = Active; next_index; consecutive_failures = 0 }))
  | Active -> Error Refusal.Not_paused
  | Completed | Cancelled -> Error Refusal.Already_ended
;;

let cancel t =
  match t.status with
  | Active | Paused _ -> Ok { t with status = Cancelled }
  | Completed | Cancelled -> Error Refusal.Already_ended
;;

let next_to_skip ?today t =
  match t.status, today with
  | Active, _ ->
    (match next t with
     | Some o -> Ok o
     | None -> Error Refusal.Nothing_to_skip)
  | Paused _, Some today ->
    (* The date resuming today would run first: dates before today are never run. *)
    (match Recurrence.first_index_on_or_after (dates t) today with
     | None -> Error Refusal.Nothing_to_skip
     | Some i ->
       (match occurrence t (Int.max i t.next_index) with
        | Some o -> Ok o
        | None -> Error Refusal.Nothing_to_skip))
  | Paused _, None -> Error Refusal.Not_active
  | (Completed | Cancelled), _ -> Error Refusal.Already_ended
;;

let upcoming ?limit t ~from ~until =
  match next t with
  | None -> []
  | Some next ->
    let limit =
      match runs_left t with
      | None -> limit
      | Some left -> Some (Int.min left (Option.value limit ~default:1000))
    in
    Recurrence.between ?limit (dates t) ~from:(Date.max from next.date) ~until
    |> List.map ~f:(fun (index, date) -> { Occurrence.index; date })
;;

module Position = struct
  module V1 = struct
    type t =
      { through : Date.Stable.V1.t option
      ; succeeded : int
      ; consecutive_failures : int
      ; status : Repr.V1.Status.t
      }
    [@@deriving bin_io, sexp, compare, equal]
  end

  include V1
end

module Restore_error = struct
  type t =
    | Negative_count of
        { succeeded : int
        ; consecutive_failures : int
        }
    | More_outcomes_than_dates of
        { outcomes : int
        ; dates : int
        }
    | Failures_not_paused of
        { consecutive_failures : int
        ; max_consecutive_failures : int
        }
  [@@deriving sexp, compare, equal]
end

let position t =
  { Position.through =
      (if t.next_index = 0 then None else Recurrence.date (dates t) (t.next_index - 1))
  ; succeeded = t.succeeded
  ; consecutive_failures = t.consecutive_failures
  ; status = t.status
  }
;;

let restore
  ?max_consecutive_failures
  ?missed_policy
  ?max_occurrences_counts
  ~id
  ~job
  recurrence
  (p : Position.t)
  =
  let t =
    create
      ?max_consecutive_failures
      ?missed_policy
      ?max_occurrences_counts
      ~id
      ~job
      recurrence
  in
  (* Dates strictly increase, so the count of dates on or before [through] is the index of
     the first date not yet behind the schedule. *)
  let next_index =
    match p.through with
    | None -> 0
    | Some d ->
      Option.value_map (Recurrence.last_index_on_or_before (dates t) d) ~default:0 ~f:succ
  in
  let outcomes = p.succeeded + p.consecutive_failures in
  if p.succeeded < 0 || p.consecutive_failures < 0
  then
    Error
      (Restore_error.Negative_count
         { succeeded = p.succeeded; consecutive_failures = p.consecutive_failures })
  else if outcomes > next_index
  then Error (More_outcomes_than_dates { outcomes; dates = next_index })
  else if Status.equal p.status Active
          && p.consecutive_failures >= t.max_consecutive_failures
  then
    Error
      (Failures_not_paused
         { consecutive_failures = p.consecutive_failures
         ; max_consecutive_failures = t.max_consecutive_failures
         })
  else
    Ok
      (complete_if_done
         { t with
           next_index
         ; succeeded = p.succeeded
         ; consecutive_failures = p.consecutive_failures
         ; status = p.status
         })
;;

module Stable = struct
  module Position = struct
    module V1 = Position.V1
  end

  module V1 = struct
    type nonrec 'job t = 'job t [@@deriving compare, equal]

    (* Without a uuid so the bin_prot shape, and its pinned digest, stays that of V1. *)
    include
      Binable.Of_binable1_without_uuid [@alert "-legacy"]
        (Repr.V1)
        (struct
          type nonrec 'job t = 'job t

          let to_binable = to_v1
          let of_binable = of_v1
        end)

    include
      Sexpable.Of_sexpable1
        (Repr.V1)
        (struct
          type nonrec 'job t = 'job t

          let to_sexpable = to_v1
          let of_sexpable = of_v1
        end)
  end

  module V2 = struct
    type 'job t = 'job Repr.V2.t [@@deriving bin_io, sexp, compare, equal]
  end
end
