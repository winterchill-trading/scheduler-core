(** The dates of a schedule: a {!Frequency}, a start date and optional bounds.

    Date [i] (counting from 0) is computed from the start by index, so a monthly schedule
    starting on 31 January runs on 28 February (29 in a leap year), then 31 March, 30
    April, and so on, never drifting to the 28th. A date is excluded when it falls after
    [end_date] or when its index reaches [max_occurrences]. *)

open! Core

type t [@@deriving sexp, compare, equal, hash]

module Invalid : sig
  (** Why {!create} refused a recurrence. *)
  type t =
    | Interval_days_out_of_range of int (** {!Frequency.Every_n_days} outside 1..366 *)
    | Day_of_month_out_of_range of int (** outside 1..31 *)
    | Max_occurrences_out_of_range of int (** outside 1..[max_occurrences_limit] *)
    | End_before_start
    | No_date_before_end (** the first date already falls after [end_date] *)
  [@@deriving sexp, compare, equal]
end

(** Largest [max_occurrences] accepted, 1000. *)
val max_occurrences_limit : int

val create
  :  Frequency.t
  -> start:Date.t
  -> ?end_date:Date.t
  -> ?max_occurrences:int
  -> unit
  -> (t, Invalid.t) Result.t

(** Raises on what {!create} rejects. *)
val create_exn
  :  Frequency.t
  -> start:Date.t
  -> ?end_date:Date.t
  -> ?max_occurrences:int
  -> unit
  -> t

val frequency : t -> Frequency.t
val start : t -> Date.t
val end_date : t -> Date.t option
val max_occurrences : t -> int option

(** Date number [i], or [None] when [i] is negative or the date is beyond the end (a
    {!Frequency.Once} recurrence has only date 0). *)
val date : t -> int -> Date.t option

(** The first date, which always exists for a value built by {!create}. *)
val first_date : t -> Date.t

(** The last date, or [None] when the recurrence has no end. *)
val last_date : t -> Date.t option

(** Index of the first date on or after the given day, if any. *)
val first_index_on_or_after : t -> Date.t -> int option

(** Index of the last date on or before the given day, if any. *)
val last_index_on_or_before : t -> Date.t -> int option

(** The dates from index [from_index] on (default 0), in order. Infinite when the
    recurrence has no end. *)
val dates : ?from_index:int -> t -> (int * Date.t) Sequence.t

(** The dates in the closed range [[from, until]], at most [limit] of them (default 1000). *)
val between : ?limit:int -> t -> from:Date.t -> until:Date.t -> (int * Date.t) list

(** A start date check for services that accept schedules from users: [`Past] before
    [today], [`Too_far] more than [max_days_ahead] days after it. *)
val check_start
  :  t
  -> today:Date.t
  -> max_days_ahead:int
  -> (unit, [ `Past | `Too_far ]) Result.t

module Stable : sig
  module V1 : sig
    (** Reading validates, as {!create} does. *)
    type nonrec t = t [@@deriving bin_io, sexp, compare, equal, hash]
  end
end
