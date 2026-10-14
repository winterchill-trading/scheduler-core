open! Core

module Stable = struct
  module V1 = struct
    module State = struct
      type t =
        | Claimed of
            { attempt : int
            ; claimed_at : Time_ns.Stable.Alternate_sexp.V1.t
            }
        | Finished of
            { outcome : Outcome.Stable.V1.t
            ; attempts : int
            ; finished_at : Time_ns.Stable.Alternate_sexp.V1.t
            }
      [@@deriving bin_io, sexp, compare, equal]
    end

    type t =
      { key : Run_key.Stable.V1.t
      ; index : int
      ; state : State.t
      }
    [@@deriving bin_io, sexp, compare, equal]
  end
end

include Stable.V1

let outcome t =
  match t.state with
  | Finished { outcome; _ } -> Some outcome
  | Claimed _ -> None
;;

let is_running t ~now ~stale_after =
  match t.state with
  | Finished _ -> false
  | Claimed { claimed_at; _ } -> Time_ns.( >= ) claimed_at (Time_ns.sub now stale_after)
;;
