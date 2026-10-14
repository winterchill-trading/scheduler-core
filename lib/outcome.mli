(** How a due date ended. *)

open! Core

type t =
  | Succeeded of { reference : string }
  (** the job did its work; [reference] is what it produced (a payment or order id) *)
  | Failed of { reason : string }
  (** the job refused or failed for good on this date; [reason] is a short code such as
      [insufficient_funds] that a caller can render *)
  | Missed (** passed over after a gap, when only the latest due date runs *)
  | Skipped (** the owner skipped this date before it ran *)
[@@deriving bin_io, sexp, compare, equal, hash]

val is_failure : t -> bool

module Stable : sig
  module V1 : sig
    type nonrec t = t [@@deriving bin_io, sexp, compare, equal, hash]
  end
end
