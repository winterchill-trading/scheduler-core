(** An in-memory {!Store.S} for tests, examples and offline modes. Every call completes
    at once, so it is trivially atomic within one process. *)

open! Core

module Make (M : Monad.S) (Job : T) : sig
  include Store.S with type 'a io = 'a M.t and type job = Job.t

  val create : unit -> t

  (** Every schedule, in id order. *)
  val all : t -> Job.t Schedule.t list
end
