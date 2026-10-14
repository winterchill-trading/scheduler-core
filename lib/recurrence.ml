open! Core

module Invalid = struct
  type t =
    | Interval_days_out_of_range of int
    | Day_of_month_out_of_range of int
    | Max_occurrences_out_of_range of int
    | End_before_start
    | No_date_before_end
  [@@deriving sexp, compare, equal]
end

let max_occurrences_limit = 1000

module Repr = struct
  type t =
    { frequency : Frequency.Stable.V1.t
    ; start : Date.Stable.V1.t
    ; end_date : Date.Stable.V1.t option [@sexp.option]
    ; max_occurrences : int option [@sexp.option]
    }
  [@@deriving bin_io, sexp, compare, equal, hash]
end

type t = Repr.t [@@deriving compare, equal, hash, sexp_of]

let frequency (t : t) = t.frequency
let start (t : t) = t.start
let end_date (t : t) = t.end_date
let max_occurrences (t : t) = t.max_occurrences
let without_max_occurrences (t : t) = { t with max_occurrences = None }

(* Months since year 0, so adding [k] months is integer arithmetic and the month and year
   come back with [/] and [mod]. *)
let month_number date = (Date.year date * 12) + Month.to_int (Date.month date) - 1

let date_in_month ~month_number ~day =
  let year = month_number / 12 in
  let month = Month.of_int_exn ((month_number mod 12) + 1) in
  Date.create_exn ~y:year ~m:month ~d:(Int.min day (Date.days_in_month ~year ~month))
;;

let monthly_day (t : t) =
  match t.frequency with
  | Monthly { day_of_month = Some d } -> d
  | _ -> Date.day t.start
;;

(* With an explicit day before the start's day (start 20 May, day 5) the first date is in
   the next month; [monthly_offset] is 0 or 1. *)
let monthly_offset (t : t) =
  let d = date_in_month ~month_number:(month_number t.start) ~day:(monthly_day t) in
  if Date.( < ) d t.start then 1 else 0
;;

let step_days (t : t) =
  match t.frequency with
  | Weekly -> Some 7
  | Fortnightly -> Some 14
  | Every_n_days n -> Some n
  | Once | Monthly _ -> None
;;

(* Date [i] ignoring [end_date] and [max_occurrences]. *)
let raw_date (t : t) i =
  match t.frequency with
  | Once -> if i = 0 then Some t.start else None
  | Monthly _ ->
    Some
      (date_in_month
         ~month_number:(month_number t.start + monthly_offset t + i)
         ~day:(monthly_day t))
  | Weekly | Fortnightly | Every_n_days _ ->
    let step = Option.value_exn (step_days t) in
    Some (Date.add_days t.start (step * i))
;;

let in_bounds (t : t) i d =
  (match t.max_occurrences with
   | Some m -> i < m
   | None -> true)
  &&
  match t.end_date with
  | Some e -> Date.( <= ) d e
  | None -> true
;;

let date t i =
  if i < 0
  then None
  else (
    match raw_date t i with
    | Some d when in_bounds t i d -> Some d
    | _ -> None)
;;

let first_date t = Option.value_exn (date t 0)

(* The smallest index whose raw date is on or after [day] (ignoring bounds), as a guess
   from arithmetic and then corrected by at most a step either way. *)
let raw_index_on_or_after (t : t) day =
  if Date.( <= ) day t.start
  then Some 0
  else (
    match t.frequency with
    | Once -> None
    | Weekly | Fortnightly | Every_n_days _ ->
      let step = Option.value_exn (step_days t) in
      let diff = Date.diff day t.start in
      Some ((diff + step - 1) / step)
    | Monthly _ ->
      let guess =
        Int.max 0 (month_number day - month_number t.start - monthly_offset t)
      in
      let rec fix i =
        match raw_date t i with
        | Some d when Date.( < ) d day -> fix (i + 1)
        | _ -> i
      in
      Some (fix guess))
;;

let first_index_on_or_after t day =
  match raw_index_on_or_after t day with
  | None -> None
  | Some i -> Option.map (date t i) ~f:(fun _ -> i)
;;

let last_index (t : t) =
  match t.frequency, t.max_occurrences, t.end_date with
  | Once, _, _ -> Some 0
  | _, None, None -> None
  | _, max, end_ ->
    let by_max = Option.map max ~f:(fun m -> m - 1) in
    let by_end =
      Option.bind end_ ~f:(fun e ->
        (* the last raw index on or before [e] *)
        match raw_index_on_or_after t (Date.add_days e 1) with
        | Some i -> Some (i - 1)
        | None -> Some 0)
    in
    (match by_max, by_end with
     | Some a, Some b -> Some (Int.min a b)
     | Some a, None | None, Some a -> Some a
     | None, None -> None)
;;

let last_date t = Option.bind (last_index t) ~f:(date t)

let last_index_on_or_before t day =
  let candidate =
    match raw_index_on_or_after t (Date.add_days day 1) with
    | Some i -> i - 1
    | None -> 0 (* [Once] with [day] on or after the start *)
  in
  let candidate =
    match last_index t with
    | Some l -> Int.min l candidate
    | None -> candidate
  in
  match date t candidate with
  | Some d when Date.( <= ) d day -> Some candidate
  | _ -> None
;;

let dates ?(from_index = 0) t =
  Sequence.unfold ~init:(Int.max 0 from_index) ~f:(fun i ->
    Option.map (date t i) ~f:(fun d -> (i, d), i + 1))
;;

let between ?(limit = 1000) t ~from ~until =
  match first_index_on_or_after t from with
  | None -> []
  | Some from_index ->
    dates ~from_index t
    |> Sequence.take_while ~f:(fun (_, d) -> Date.( <= ) d until)
    |> Fn.flip Sequence.take limit
    |> Sequence.to_list
;;

let validate (t : t) =
  let open Result.Let_syntax in
  let%bind () =
    match t.frequency with
    | Every_n_days n when n < 1 || n > Frequency.max_interval_days ->
      Error (Invalid.Interval_days_out_of_range n)
    | Monthly { day_of_month = Some d } when d < 1 || d > 31 ->
      Error (Invalid.Day_of_month_out_of_range d)
    | _ -> Ok ()
  in
  let%bind () =
    match t.max_occurrences with
    | Some m when m < 1 || m > max_occurrences_limit ->
      Error (Invalid.Max_occurrences_out_of_range m)
    | _ -> Ok ()
  in
  match t.end_date with
  | Some e when Date.( < ) e t.start -> Error Invalid.End_before_start
  | _ -> if Option.is_none (date t 0) then Error Invalid.No_date_before_end else Ok t
;;

let create frequency ~start ?end_date ?max_occurrences () =
  validate { frequency; start; end_date; max_occurrences }
;;

let create_exn frequency ~start ?end_date ?max_occurrences () =
  match create frequency ~start ?end_date ?max_occurrences () with
  | Ok t -> t
  | Error e -> raise_s [%message "invalid recurrence" (e : Invalid.t)]
;;

let check_start t ~today ~max_days_ahead =
  if Date.( < ) (start t) today
  then Error `Past
  else if Date.diff (start t) today > max_days_ahead
  then Error `Too_far
  else Ok ()
;;

let of_repr repr =
  match validate repr with
  | Ok t -> t
  | Error e -> raise_s [%message "invalid recurrence" (e : Invalid.t)]
;;

let t_of_sexp sexp = of_repr (Repr.t_of_sexp sexp)

module Stable = struct
  module V1 = struct
    type nonrec t = t [@@deriving compare, equal, hash, sexp_of]

    let t_of_sexp = t_of_sexp

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
