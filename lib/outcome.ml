open! Core

module Stable = struct
  module V1 = struct
    type t =
      | Succeeded of { reference : string }
      | Failed of { reason : string }
      | Missed
      | Skipped
    [@@deriving bin_io, sexp, compare, equal, hash]
  end
end

include Stable.V1

let is_failure = function
  | Failed _ -> true
  | Succeeded _ | Missed | Skipped -> false
;;
