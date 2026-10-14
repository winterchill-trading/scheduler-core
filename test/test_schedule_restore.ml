open! Core
open Scheduler_core

let id = Schedule_id.create_exn "r"

(* What can happen to a schedule between two stores of it. *)
module Step = struct
  type t =
    | Settle_next of Outcome.t
    | Pause
    | Resume of int (** days after the next date, possibly negative *)
    | Cancel
  [@@deriving sexp_of]

  let gen =
    let open Quickcheck.Generator.Let_syntax in
    Quickcheck.Generator.weighted_union
      [ ( 6.
        , let%map ok = Bool.quickcheck_generator in
          Settle_next
            (if ok then Succeeded { reference = "p" } else Failed { reason = "f" }) )
      ; 1., return (Settle_next Skipped)
      ; 1., return (Settle_next Missed)
      ; 1., return Pause
      ; ( 1.
        , let%map d = Int.gen_incl (-40) 120 in
          Resume d )
      ; 0.1, return Cancel
      ]
  ;;

  let apply t step =
    match step with
    | Settle_next outcome ->
      (match Schedule.next t with
       | Some (o : Schedule.Occurrence.t)
         when not (Schedule.Status.is_ended (Schedule.status t)) ->
         Schedule.settle t ~index:o.index outcome
       | _ -> t)
    | Pause -> Result.ok (Schedule.pause t) |> Option.value ~default:t
    | Resume days ->
      let today =
        match Schedule.next t with
        | Some o -> Date.add_days o.date days
        | None -> Recurrence.start (Schedule.recurrence t)
      in
      Result.ok (Schedule.resume t ~today) |> Option.value ~default:t
    | Cancel -> Result.ok (Schedule.cancel t) |> Option.value ~default:t
  ;;
end

type case =
  { recurrence : Recurrence.t
  ; counts : Schedule.Max_occurrences_counts.t
  ; max_failures : int
  ; before : Step.t list
  ; after : Step.t list
  }
[@@deriving sexp_of]

let gen =
  let open Quickcheck.Generator.Let_syntax in
  let%map recurrence = Gen.recurrence
  and counts =
    Quickcheck.Generator.of_list
      [ Schedule.Max_occurrences_counts.Dates; Successful_runs ]
  and max_failures = Int.gen_incl 1 4
  and before = List.quickcheck_generator Step.gen
  and after = List.quickcheck_generator Step.gen in
  { recurrence; counts; max_failures; before; after }
;;

let%test_unit "restore then run equals run from history" =
  Quickcheck.test gen ~sexp_of:sexp_of_case ~trials:2000 ~f:(fun c ->
    let fresh () =
      Schedule.create
        ~max_consecutive_failures:c.max_failures
        ~max_occurrences_counts:c.counts
        ~id
        ~job:()
        c.recurrence
    in
    let run t steps = List.fold steps ~init:t ~f:Step.apply in
    let stored = run (fresh ()) c.before in
    let restored =
      Schedule.restore
        ~max_consecutive_failures:c.max_failures
        ~max_occurrences_counts:c.counts
        ~id
        ~job:()
        c.recurrence
        (Schedule.position stored)
      |> Result.map_error ~f:[%sexp_of: Schedule.Restore_error.t]
      |> Result.map_error ~f:Error.create_s
      |> ok_exn
    in
    [%test_result: unit Schedule.t] restored ~expect:stored;
    [%test_result: unit Schedule.t] (run restored c.after) ~expect:(run stored c.after))
;;

let d = Date.of_string

let monthly =
  Recurrence.create_exn
    (Monthly { day_of_month = Some 1 })
    ~start:(d "2027-01-01")
    ~max_occurrences:12
    ()
;;

let show r =
  print_s
    [%sexp
      (Result.map r ~f:(fun t ->
         [%message
           ""
             ~status:(Schedule.Status.name (Schedule.status t))
             ~next:(Schedule.next t : Schedule.Occurrence.t option)
             ~succeeded:(Schedule.succeeded t : int)])
       : (Sexp.t, Schedule.Restore_error.t) Result.t)]
;;

let restore
  ?(succeeded = 0)
  ?(consecutive_failures = 0)
  ?(status = Schedule.Status.Active)
  through
  =
  Schedule.restore
    ~id
    ~job:()
    monthly
    { through = Option.map through ~f:d; succeeded; consecutive_failures; status }
;;

let%expect_test "restoring from a last paid date" =
  show (restore None);
  show (restore (Some "2027-03-01") ~succeeded:3);
  (* a date between two schedule dates counts the earlier one as behind *)
  show (restore (Some "2027-03-15") ~succeeded:3);
  show (restore (Some "2027-12-01") ~succeeded:12);
  [%expect
    {|
    (Ok ((status active) (next (((index 0) (date 2027-01-01)))) (succeeded 0)))
    (Ok ((status active) (next (((index 3) (date 2027-04-01)))) (succeeded 3)))
    (Ok ((status active) (next (((index 3) (date 2027-04-01)))) (succeeded 3)))
    (Ok ((status completed) (next ()) (succeeded 12)))
    |}]
;;

let%expect_test "inconsistent positions are refused" =
  show (restore (Some "2027-02-01") ~succeeded:3);
  show (restore (Some "2027-02-01") ~succeeded:(-1));
  show (restore (Some "2027-05-01") ~succeeded:1 ~consecutive_failures:3);
  [%expect
    {|
    (Error (More_outcomes_than_dates (outcomes 3) (dates 2)))
    (Error (Negative_count (succeeded -1) (consecutive_failures 0)))
    (Error
     (Failures_not_paused (consecutive_failures 3) (max_consecutive_failures 3)))
    |}]
;;
