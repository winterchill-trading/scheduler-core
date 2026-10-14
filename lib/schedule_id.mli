(** Opaque identifier of a schedule, chosen by the caller (a database key, a UUID). It
    must not contain [':'], which separates the identifier from the date in a
    {!Run_key}. *)

open! Core

type t [@@deriving bin_io, sexp, compare, equal, hash]

include Identifiable.S with type t := t

(** Fails on an empty string or one containing [':'] or whitespace. *)
val create : string -> t Or_error.t

(** Raises on what {!create} rejects. *)
val create_exn : string -> t

module Stable : sig
  module V1 : sig
    type nonrec t = t [@@deriving bin_io, sexp, compare, equal, hash]
  end
end
