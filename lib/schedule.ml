open! Core

module Stable = struct
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
end

module Missed_policy = Stable.V1.Missed_policy
module Pause_reason = Stable.V1.Pause_reason

module Status = struct
  include Stable.V1.Status

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

type 'job t = 'job Stable.V1.t =
  { id : Schedule_id.t
  ; job : 'job
  ; recurrence : Recurrence.t
  ; status : Status.t
  ; missed_policy : Missed_policy.t
  ; max_consecutive_failures : int
  ; next_index : int
  ; succeeded : int
  ; consecutive_failures : int
  }
[@@deriving sexp, compare, equal]

let default_max_consecutive_failures = 3

let create
  ?(max_consecutive_failures = default_max_consecutive_failures)
  ?(missed_policy = Missed_policy.Latest_only)
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
let with_job t job = { t with job }
let key t (o : Occurrence.t) = Run_key.create t.id o.date

let occurrence t index =
  Option.map (Recurrence.date t.recurrence index) ~f:(fun date ->
    { Occurrence.index; date })
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
  match Recurrence.last_index_on_or_before t.recurrence today with
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
    if Option.is_none (Recurrence.date t.recurrence t.next_index)
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
    (match Recurrence.first_index_on_or_after t.recurrence today with
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

let next_to_skip t =
  match t.status with
  | Active ->
    (match next t with
     | Some o -> Ok o
     | None -> Error Refusal.Nothing_to_skip)
  | Paused _ -> Error Refusal.Not_active
  | Completed | Cancelled -> Error Refusal.Already_ended
;;

let upcoming ?limit t ~from ~until =
  match next t with
  | None -> []
  | Some next ->
    Recurrence.between ?limit t.recurrence ~from:(Date.max from next.date) ~until
    |> List.map ~f:(fun (index, date) -> { Occurrence.index; date })
;;
