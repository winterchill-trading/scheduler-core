(** What a schedule runs. One interface for every kind of recurring work: a standing
    order pays a beneficiary, a recurring buy places a market order, a reminder sends a
    message, a payroll run creates a batch. The runner decides when; the job decides
    what.

    A job must be idempotent by {!Context.key}: the runner calls it again with the same
    key when a previous attempt crashed or hung (a stale claim), and that call must
    return the first attempt's result instead of repeating the side effect. Passing
    [Run_key.to_string key] as the downstream idempotency key does this. *)

open! Core

module Context : sig
  type t =
    { key : Run_key.t (** schedule and due date; the idempotency key *)
    ; index : int (** the date's index in the recurrence; 0 for the first date *)
    ; attempt : int (** 1 for the first claim, higher after a stale claim was taken over *)
    ; now : Time_ns.t
    }
  [@@deriving sexp_of]
end

module type S = sig
  (** The effect the job runs in: [Monad.Ident] in tests, [Deferred.t] in an Async
      service. *)
  type 'a io

  (** The job's own parameters, stored in the schedule (amount, beneficiary,
      instrument). *)
  type t [@@deriving sexp_of]

  (** Refuses recurrences this kind of job does not support (for example recurring
      buys that run only weekly or monthly). *)
  val validate : t -> Recurrence.t -> unit Or_error.t

  (** [Ok reference] when the work is done, [Error reason] when it failed for good on
      this date (the reason is a short code). An exception, or a result that never
      comes back, leaves the claim to go stale and be retried with the same key. *)
  val run : t -> Context.t -> (string, string) Result.t io
end
