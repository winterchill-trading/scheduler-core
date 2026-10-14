(** How often a schedule repeats.

    Every frequency counts from the schedule's start date by index, never from the
    previous date, so clamping one short month does not move the dates after it. *)

open! Core

type t =
  | Once (** a single date: "send later" *)
  | Weekly (** every 7 days from the start *)
  | Fortnightly (** every 14 days from the start *)
  | Monthly of { day_of_month : int option }
  (** the same day each month, clamped to the last day of shorter months; [None] takes
      the start date's day. A given day before the start's day moves the first date to
      the next month. *)
  | Every_n_days of int (** every [n] days from the start, [n] in 1..366 *)
[@@deriving bin_io, sexp, compare, equal, hash]

(** Lowercase wire names: [once], [weekly], [fortnightly], [monthly], [every_n_days]. *)
val name : t -> string

(** [Error] for an interval outside 1..366 or a day of month outside 1..31. *)
val validate : t -> unit Or_error.t

(** Largest interval {!Every_n_days} accepts. *)
val max_interval_days : int

module Stable : sig
  module V1 : sig
    type nonrec t = t [@@deriving bin_io, sexp, compare, equal, hash]
  end
end
