open! Core

let invalid s =
  String.is_empty s
  || String.exists s ~f:(fun c -> Char.equal c ':' || Char.is_whitespace c)
;;

module Stable = struct
  module V1 = struct
    module Repr = struct
      type t = string [@@deriving bin_io, sexp, compare, equal, hash]
    end

    type t = string [@@deriving compare, equal, hash, sexp_of]

    let of_repr s =
      if invalid s then raise_s [%message "invalid schedule id" (s : string)] else s
    ;;

    let t_of_sexp sexp = of_repr (Repr.t_of_sexp sexp)

    (* Validate on read so a damaged record cannot smuggle a ':' into a run key. *)
    include
      Binable.Of_binable_without_uuid [@alert "-legacy"]
        (Repr)
        (struct
          type nonrec t = t

          let to_binable = Fn.id
          let of_binable = of_repr
        end)
  end
end

module T = struct
  include Stable.V1

  let module_name = "Scheduler_core.Schedule_id"
  let to_string = Fn.id
  let of_string = Stable.V1.of_repr
end

include T
include Identifiable.Make (T)

let create s =
  if invalid s
  then Or_error.error_s [%message "invalid schedule id" (s : string)]
  else Ok s
;;

let create_exn s = Or_error.ok_exn (create s)
