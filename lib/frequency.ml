open! Core

module Stable = struct
  module V1 = struct
    type t =
      | Once
      | Weekly
      | Fortnightly
      | Monthly of { day_of_month : int option }
      | Every_n_days of int
    [@@deriving bin_io, sexp, compare, equal, hash]
  end
end

include Stable.V1

let name = function
  | Once -> "once"
  | Weekly -> "weekly"
  | Fortnightly -> "fortnightly"
  | Monthly _ -> "monthly"
  | Every_n_days _ -> "every_n_days"
;;

let max_interval_days = 366

let validate = function
  | Once | Weekly | Fortnightly | Monthly { day_of_month = None } -> Ok ()
  | Monthly { day_of_month = Some d } ->
    if d >= 1 && d <= 31
    then Ok ()
    else Or_error.error_s [%message "day of month outside 1..31" (d : int)]
  | Every_n_days n ->
    if n >= 1 && n <= max_interval_days
    then Ok ()
    else Or_error.error_s [%message "interval outside 1..366 days" (n : int)]
;;
