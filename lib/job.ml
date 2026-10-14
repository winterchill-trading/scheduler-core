open! Core

module Context = struct
  type t =
    { key : Run_key.t
    ; index : int
    ; attempt : int
    ; now : Time_ns.t
    }
  [@@deriving sexp_of]
end

module type S = sig
  type 'a io
  type t [@@deriving sexp_of]

  val validate : t -> Recurrence.t -> unit Or_error.t
  val run : t -> Context.t -> (string, string) Result.t io
end
