(** The identity of one run: a schedule and one of its due dates.

    At most one run record exists per key, and the key is what a job passes on as its own
    idempotency key (a payment request key, an order client id), so a retried run replays
    the original side effect instead of making a second one. Its text form is
    [<schedule id>:<YYYY-MM-DD>]. *)

open! Core

type t =
  { schedule : Schedule_id.t
  ; due_date : Date.t
  }
[@@deriving bin_io, sexp, compare, equal, hash]

include Comparable.S_plain with type t := t
include Hashable.S_plain with type t := t

val create : Schedule_id.t -> Date.t -> t

(** [<schedule id>:<YYYY-MM-DD>] *)
val to_string : t -> string

val of_string : string -> t Or_error.t

module Stable : sig
  module V1 : sig
    type nonrec t = t [@@deriving bin_io, sexp, compare, equal, hash]
  end
end
