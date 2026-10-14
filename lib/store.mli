(** Where schedules and run records live. Libraries never persist: a service implements
    this signature over its database, and {!Store_memory} serves tests and offline modes.

    Exactly-once rests on {!S.claim}: it must be atomic per run key (an insert that fails
    on a duplicate key, or a compare-and-set), so of two runners racing for the same date
    only one gets [Claimed]. Schedules are saved last-writer-wins; the runner re-reads a
    schedule before applying a run's outcome to it. *)

open! Core

module Claim : sig
  type t =
    | Claimed of { attempt : int }
    (** the caller owns the date now; [attempt] is 1, or more when a stale claim was taken
        over *)
    | Running of Run_record.t (** another runner holds a fresh claim *)
    | Finished of Run_record.t (** the date is settled already *)
  [@@deriving sexp_of]
end

module type S = sig
  type 'a io

  (** The job parameters stored with each schedule. *)
  type job

  type t

  val find : t -> Schedule_id.t -> job Schedule.t option io

  (** Inserts or replaces. *)
  val save : t -> job Schedule.t -> unit io

  (** Every schedule whose status is [Active]. *)
  val active : t -> job Schedule.t list io

  (** Takes the date: creates a [Claimed] record with attempt 1 when none exists, replaces
      a claim older than [stale_after] with [attempt + 1], and otherwise returns the
      existing record. *)
  val claim
    :  t
    -> Run_key.t
    -> index:int
    -> now:Time_ns.t
    -> stale_after:Time_ns.Span.t
    -> Claim.t io

  (** Settles the date. The first outcome written wins: finishing a finished record
      returns it unchanged. *)
  val finish
    :  t
    -> Run_key.t
    -> index:int
    -> Outcome.t
    -> now:Time_ns.t
    -> Run_record.t io

  val record : t -> Run_key.t -> Run_record.t option io

  (** A schedule's records, oldest due date first. *)
  val records : t -> Schedule_id.t -> Run_record.t list io
end
