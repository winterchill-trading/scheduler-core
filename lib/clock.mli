(** The current time and the time zone that turns it into a calendar date.

    Due dates are calendar dates in the schedule owner's zone: a payment due on 1 March in
    Berlin is due from midnight Berlin time, which is 23:00 UTC on 28 February. The clock
    is a value so tests, replays and demo controls can move time. *)

open! Core

type t

(** [now] is called once per {!today}. With Async, pass
    [fun () -> Time_source.now time_source]. *)
val create : now:(unit -> Time_ns.t) -> zone:Time_float.Zone.t -> t

(** The wall clock. *)
val wall : zone:Time_float.Zone.t -> t

val now : t -> Time_ns.t
val zone : t -> Time_float.Zone.t

(** The calendar date of {!now} in {!zone}. *)
val today : t -> Date.t

(** The same clock in another zone. *)
val with_zone : t -> Time_float.Zone.t -> t

(** A clock that only moves when told to, for tests and demo controls. *)
module Manual : sig
  type clock := t
  type t

  val create : start:Time_ns.t -> zone:Time_float.Zone.t -> t
  val clock : t -> clock
  val set : t -> Time_ns.t -> unit
  val advance : t -> Time_ns.Span.t -> unit

  (** Moves to midnight at the start of the given date in the clock's zone. *)
  val set_date : t -> Date.t -> unit
end
