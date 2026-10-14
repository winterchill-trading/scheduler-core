(** Runs due schedules against a {!Store.S}, exactly once per (schedule, date).

    Each {!Make.tick} reads the active schedules, computes today in the clock's zone and,
    per schedule: settles passed-over dates as {!Outcome.Missed} (under
    {!Schedule.Missed_policy.Latest_only}), claims each date to run, calls the job with
    the run key, records the outcome, and applies it to the schedule (re-read from the
    store, so a pause made while the job ran is kept).

    A date whose claim is fresh is left alone; one whose claim is older than [stale_after]
    (default 2 minutes) is taken over and the job is called again with the same key and
    [attempt + 1], which the job replays. A settled date is never run again. The runner
    works in any monad, so the same code drives tests ([Monad.Ident]) and an Async service
    ([Deferred]). *)

open! Core

module Execution : sig
  type status =
    | Ran of
        { attempt : int
        ; outcome : Outcome.t
        }
    | Already_settled of Outcome.t
    (** a finished record existed (a previous tick crashed after recording it); the
        schedule caught up from it without running the job *)
    | Marked_missed
    | Busy (** another runner holds a fresh claim on this date *)
  [@@deriving sexp_of]

  type t =
    { key : Run_key.t
    ; index : int
    ; status : status
    }
  [@@deriving sexp_of]
end

module Command_error : sig
  type t =
    | Unknown_schedule of Schedule_id.t
    | Duplicate_schedule of Schedule_id.t
    | Invalid_job of Error.t (** {!Job.S.validate} refused the recurrence *)
    | Refused of Schedule.Refusal.t
    | Run_in_progress (** the next date is being run right now *)
  [@@deriving sexp_of]
end

(** Default stale limit for claims: 2 minutes. *)
val default_stale_after : Time_ns.Span.t

module Make
    (M : Monad.S)
    (Job : Job.S with type 'a io := 'a M.t)
    (Store : Store.S with type 'a io := 'a M.t and type job := Job.t) : sig
  type t

  val create : ?stale_after:Time_ns.Span.t -> store:Store.t -> clock:Clock.t -> unit -> t
  val store : t -> Store.t
  val clock : t -> Clock.t

  (** Validates the job against the recurrence and saves a new active schedule. *)
  val add
    :  t
    -> ?max_consecutive_failures:int
    -> ?missed_policy:Schedule.Missed_policy.t
    -> id:Schedule_id.t
    -> Job.t
    -> Recurrence.t
    -> (Job.t Schedule.t, Command_error.t) Result.t M.t

  val pause : t -> Schedule_id.t -> (Job.t Schedule.t, Command_error.t) Result.t M.t

  (** Resumes from the first date on or after today (in the clock's zone). *)
  val resume : t -> Schedule_id.t -> (Job.t Schedule.t, Command_error.t) Result.t M.t

  (** Refused with [Run_in_progress] while the next date is being run. *)
  val cancel : t -> Schedule_id.t -> (Job.t Schedule.t, Command_error.t) Result.t M.t

  (** Settles the next date as {!Outcome.Skipped}. Refused with [Run_in_progress] while
      that date is being run. *)
  val skip : t -> Schedule_id.t -> (Job.t Schedule.t, Command_error.t) Result.t M.t

  (** Replaces the job parameters of a schedule that has not ended; later dates use them. *)
  val update_job
    :  t
    -> Schedule_id.t
    -> Job.t
    -> (Job.t Schedule.t, Command_error.t) Result.t M.t

  (** Runs what is due on every active schedule, one schedule after another. *)
  val tick : t -> Execution.t list M.t

  (** Runs what is due on one schedule (an operator's "run now" or a demo control). *)
  val tick_one : t -> Schedule_id.t -> (Execution.t list, Command_error.t) Result.t M.t
end
