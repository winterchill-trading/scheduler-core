(** Scenarios every {!Store.S} implementation must pass: claim, take-over of a stale
    claim, first-writer-wins finish, unclaimed settlement and listing. A service runs them
    against its database store in its own test suite, next to {!Store_memory}.

    Give it a fresh, empty store; the scenarios use schedule ids starting with
    [conformance-]. *)

open! Core

module Make
    (M : Monad.S)
    (Store : Store.S with type 'a io := 'a M.t and type job := string) : sig
  (** [Ok ()] when every scenario passes, otherwise the first difference. *)
  val check : Store.t -> unit Or_error.t M.t
end
