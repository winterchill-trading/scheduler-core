open! Core

module Stable = struct
  module V1 = struct
    type t =
      { schedule : Schedule_id.Stable.V1.t
      ; due_date : Date.Stable.V1.t
      }
    [@@deriving bin_io, sexp, compare, equal, hash]
  end
end

module T = struct
  include Stable.V1
end

include T
include Comparable.Make_plain (T)
include Hashable.Make_plain (T)

let create schedule due_date = { schedule; due_date }

let to_string t =
  sprintf "%s:%s" (Schedule_id.to_string t.schedule) (Date.to_string t.due_date)
;;

let of_string s =
  match String.lsplit2 s ~on:':' with
  | None -> Or_error.error_s [%message "run key without ':'" (s : string)]
  | Some (id, date) ->
    Or_error.try_with_join (fun () ->
      Or_error.map (Schedule_id.create id) ~f:(fun schedule ->
        { schedule; due_date = Date.of_string date }))
;;
