(** A schedule: a job, its {!Recurrence} and where it has got to. A pure state machine;
    the {!Runner} moves it as runs finish.

    {v
    active --pause--> paused --resume--> active   (dates passed while paused are not run)
    active | paused --cancel--> cancelled
    active --N failed dates in a row--> paused (Failures)
    active | paused --last date settled--> completed
    v}

    Each date is settled once, in index order: it succeeds, fails, is skipped by the
    owner or is missed after a gap. A failed date is not retried; the schedule moves on
    to the next date and pauses after [max_consecutive_failures] failures in a row
    (default 3). *)

open! Core

module Missed_policy : sig
  (** What happens to dates that passed while nothing ran (an outage, a sleeping
      laptop in a demo). *)
  type t =
    | Latest_only
    (** run only the latest passed date and mark the earlier ones {!Outcome.Missed};
        the safe choice for payments: a customer back from an outage is not hit by a
        burst of transfers *)
    | Catch_up (** run every passed date, oldest first *)
  [@@deriving bin_io, sexp, compare, equal, hash]
end

module Pause_reason : sig
  type t =
    | By_owner
    | Failures of
        { count : int
        ; last_reason : string
        }
  [@@deriving bin_io, sexp, compare, equal, hash]
end

module Status : sig
  type t =
    | Active
    | Paused of Pause_reason.t
    | Completed
    | Cancelled
  [@@deriving bin_io, sexp, compare, equal, hash]

  (** [active], [paused], [completed], [cancelled] *)
  val name : t -> string

  (** [Completed] or [Cancelled]. *)
  val is_ended : t -> bool
end

module Refusal : sig
  (** Why a transition was refused, for the caller to map onto its own error codes. *)
  type t =
    | Not_active (** pause and skip need an active schedule *)
    | Not_paused (** resume needs a paused schedule *)
    | Already_ended (** completed or cancelled *)
    | Nothing_to_skip (** no date left *)
  [@@deriving sexp, compare, equal]
end

(** One date of the schedule. *)
module Occurrence : sig
  type t =
    { index : int
    ; date : Date.t
    }
  [@@deriving sexp, compare, equal]
end

type 'job t [@@deriving sexp, compare, equal]

(** Default 3. *)
val default_max_consecutive_failures : int

(** A new active schedule positioned on its first date. Raises if
    [max_consecutive_failures] is below 1. *)
val create
  :  ?max_consecutive_failures:int
  -> ?missed_policy:Missed_policy.t
  -> id:Schedule_id.t
  -> job:'job
  -> Recurrence.t
  -> 'job t

val id : _ t -> Schedule_id.t
val job : 'job t -> 'job
val recurrence : _ t -> Recurrence.t
val status : _ t -> Status.t
val missed_policy : _ t -> Missed_policy.t
val max_consecutive_failures : _ t -> int

(** Index of the first date not yet settled. *)
val next_index : _ t -> int

(** Dates that succeeded so far. *)
val succeeded : _ t -> int

val consecutive_failures : _ t -> int

(** The next unsettled date of an active or paused schedule. *)
val next : _ t -> Occurrence.t option

(** The run key of an occurrence of this schedule. *)
val key : _ t -> Occurrence.t -> Run_key.t

(** Replaces the job (an edited amount or reference); dates and counters stay. *)
val with_job : 'a t -> 'b -> 'b t

module Due : sig
  type t =
    { run : Occurrence.t list (** to run now, in order *)
    ; missed : Occurrence.t list (** to settle as {!Outcome.Missed}, in order *)
    }
  [@@deriving sexp, compare, equal]
end

(** What an active schedule owes on [today] (dates are due from the start of their day).
    Empty unless the schedule is active. Under [Latest_only] [run] holds at most one
    date, the latest on or before [today], and [missed] the unsettled ones before it.
    Under [Catch_up] [run] holds every unsettled date on or before [today]. *)
val due : _ t -> today:Date.t -> Due.t

(** Settles the date with this index. Settling an index below {!next_index} is a no-op
    (a replayed result); an index above it moves past the dates in between without a
    record of them (the {!Runner} settles them as missed first). A failure counts towards the pause; any other
    outcome resets the count. A schedule whose last date is settled completes;
    a cancelled schedule only records the outcome. *)
val settle : 'job t -> index:int -> Outcome.t -> 'job t

val pause : 'job t -> ('job t, Refusal.t) Result.t

(** Resumes from the first date on or after [today]; earlier dates are not run. Resets
    the failure count. Completes the schedule when no date is left. *)
val resume : 'job t -> today:Date.t -> ('job t, Refusal.t) Result.t

val cancel : 'job t -> ('job t, Refusal.t) Result.t

(** The date an owner's "skip next" applies to: the next date of an active schedule.
    The caller settles it as {!Outcome.Skipped}. *)
val next_to_skip : _ t -> (Occurrence.t, Refusal.t) Result.t

(** The next dates of an active or paused schedule in [\[from, until\]] (at most [limit],
    default 1000), for an "upcoming payments" view. *)
val upcoming : ?limit:int -> _ t -> from:Date.t -> until:Date.t -> Occurrence.t list

module Stable : sig
  module V1 : sig
    type nonrec 'job t = 'job t [@@deriving bin_io, sexp, compare, equal]
  end
end
