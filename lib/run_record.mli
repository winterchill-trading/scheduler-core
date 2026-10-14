(** The durable record of one run, one per {!Run_key}.

    A record starts [Claimed] when a runner takes the date and ends [Finished] with an
    {!Outcome}. A claim older than the store's stale limit belongs to a runner that
    crashed or hung; the next runner takes it over with [attempt + 1] and the same key. *)

open! Core

module State : sig
  type t =
    | Claimed of
        { attempt : int (** 1 for the first claim *)
        ; claimed_at : Time_ns.t
        }
    | Finished of
        { outcome : Outcome.t
        ; attempts : int
        (** claims taken on the date; 0 when it was settled without one *)
        ; finished_at : Time_ns.t
        }
  [@@deriving sexp, compare, equal]
end

type t =
  { key : Run_key.t
  ; index : int (** the date's index in the recurrence *)
  ; state : State.t
  }
[@@deriving sexp, compare, equal]

val outcome : t -> Outcome.t option

(** [Claimed] and claimed at or after [now - stale_after]: another runner is working on
    it. *)
val is_running : t -> now:Time_ns.t -> stale_after:Time_ns.Span.t -> bool

module Stable : sig
  module V1 : sig
    type nonrec t = t [@@deriving bin_io, sexp, compare, equal]
  end
end
