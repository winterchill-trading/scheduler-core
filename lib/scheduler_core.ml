(** Recurring schedules, due dates and exactly-once runs for any kind of job.

    Start with {!Recurrence} (the dates), {!Schedule} (the state machine) and {!Runner}
    (claims each due date in a {!Store} and calls a {!Job}). *)

(** How often a schedule repeats. *)
module Frequency = Frequency

(** The dates of a schedule, computed by index from its start. *)
module Recurrence = Recurrence

(** Opaque schedule identifiers. *)
module Schedule_id = Schedule_id

(** Injectable current time and time zone. *)
module Clock = Clock

(** One (schedule, due date) pair; the job's idempotency key. *)
module Run_key = Run_key

(** How a due date ended. *)
module Outcome = Outcome

(** The durable record of one run. *)
module Run_record = Run_record

(** The pure schedule state machine. *)
module Schedule = Schedule

(** The interface shared by every kind of recurring job. *)
module Job = Job

(** Persistence as a module type. *)
module Store = Store

(** An in-memory store. *)
module Store_memory = Store_memory

(** Scenarios every store implementation must pass. *)
module Store_conformance = Store_conformance

(** Runs due schedules exactly once per date. *)
module Runner = Runner
