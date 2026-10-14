open! Core

module Claim = struct
  type t =
    | Claimed of { attempt : int }
    | Running of Run_record.t
    | Finished of Run_record.t
  [@@deriving sexp_of]
end

module type S = sig
  type 'a io
  type job
  type t

  val find : t -> Schedule_id.t -> job Schedule.t option io
  val save : t -> job Schedule.t -> unit io
  val active : t -> job Schedule.t list io

  val claim
    :  t
    -> Run_key.t
    -> index:int
    -> now:Time_ns.t
    -> stale_after:Time_ns.Span.t
    -> Claim.t io

  val finish
    :  t
    -> Run_key.t
    -> index:int
    -> Outcome.t
    -> now:Time_ns.t
    -> Run_record.t io

  val record : t -> Run_key.t -> Run_record.t option io
  val records : t -> Schedule_id.t -> Run_record.t list io
end
