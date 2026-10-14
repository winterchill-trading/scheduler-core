open! Core
open Scheduler_core

let first n r = Recurrence.dates r |> Fn.flip Sequence.take n |> Sequence.to_list

let rec pairs = function
  | a :: (b :: _ as rest) -> (a, b) :: pairs rest
  | _ -> []
;;

let%test_unit "dates strictly increase from the first date and respect the bounds" =
  Quickcheck.test Gen.recurrence ~sexp_of:Recurrence.sexp_of_t ~f:(fun r ->
    let ds = first 60 r in
    [%test_result: Date.t option]
      (Option.map (List.hd ds) ~f:snd)
      ~expect:(Some (Recurrence.first_date r));
    assert (Date.( >= ) (Recurrence.first_date r) (Recurrence.start r));
    List.iteri ds ~f:(fun k (i, _) -> [%test_result: int] i ~expect:k);
    List.iter (pairs ds) ~f:(fun ((_, a), (_, b)) -> assert (Date.( < ) a b));
    List.iter ds ~f:(fun (i, d) ->
      Option.iter (Recurrence.end_date r) ~f:(fun e -> assert (Date.( <= ) d e));
      Option.iter (Recurrence.max_occurrences r) ~f:(fun m -> assert (i < m))))
;;

let%test_unit "monthly dates depend only on the index (no drift after clamping)" =
  Quickcheck.test
    (Quickcheck.Generator.both Gen.date (Option.quickcheck_generator (Int.gen_incl 1 31)))
    ~sexp_of:[%sexp_of: Date.t * int option]
    ~f:(fun (start, day_of_month) ->
      let r = Recurrence.create_exn (Monthly { day_of_month }) ~start () in
      let day = Option.value day_of_month ~default:(Date.day start) in
      let ds = first 40 r in
      List.iter ds ~f:(fun (_, d) ->
        let last = Date.days_in_month ~year:(Date.year d) ~month:(Date.month d) in
        [%test_result: int] (Date.day d) ~expect:(Int.min day last));
      let first_of_month d = Date.create_exn ~y:(Date.year d) ~m:(Date.month d) ~d:1 in
      List.iter (pairs ds) ~f:(fun ((_, a), (_, b)) ->
        [%test_result: Date.t]
          (Date.add_months (first_of_month a) 1)
          ~expect:(first_of_month b)))
;;

let%test_unit "day-based frequencies keep a fixed step" =
  Quickcheck.test Gen.recurrence ~sexp_of:Recurrence.sexp_of_t ~f:(fun r ->
    let step =
      match Recurrence.frequency r with
      | Weekly -> Some 7
      | Fortnightly -> Some 14
      | Every_n_days n -> Some n
      | Once | Monthly _ -> None
    in
    Option.iter step ~f:(fun step ->
      List.iter (first 30 r) ~f:(fun (i, d) ->
        [%test_result: int] (Date.diff d (Recurrence.start r)) ~expect:(step * i))))
;;

(* The arithmetic index lookups agree with a linear scan of the dates. *)
let%test_unit "index lookups match a scan" =
  Quickcheck.test
    (Quickcheck.Generator.both Gen.recurrence Gen.date)
    ~sexp_of:[%sexp_of: Recurrence.t * Date.t]
    ~f:(fun (r, day) ->
      (* past the first date and at least one full step (366 days at most) after [day] *)
      let horizon = Date.add_days (Date.max day (Recurrence.first_date r)) 400 in
      let scan =
        Recurrence.dates r
        |> Sequence.take_while ~f:(fun (_, d) -> Date.( <= ) d horizon)
        |> Sequence.to_list
      in
      [%test_result: int option]
        (Recurrence.first_index_on_or_after r day)
        ~expect:(List.find scan ~f:(fun (_, d) -> Date.( >= ) d day) |> Option.map ~f:fst);
      [%test_result: int option]
        (Recurrence.last_index_on_or_before r day)
        ~expect:
          (List.filter scan ~f:(fun (_, d) -> Date.( <= ) d day)
           |> List.last
           |> Option.map ~f:fst))
;;

let%test_unit "last_date is the last of the dates" =
  Quickcheck.test Gen.recurrence ~sexp_of:Recurrence.sexp_of_t ~f:(fun r ->
    match Recurrence.last_date r with
    | None -> ()
    | Some last ->
      [%test_result: Date.t option]
        (Recurrence.dates r |> Sequence.to_list |> List.last |> Option.map ~f:snd)
        ~expect:(Some last))
;;

let%test_unit "sexp and bin_prot round trip" =
  Quickcheck.test Gen.recurrence ~sexp_of:Recurrence.sexp_of_t ~f:(fun r ->
    [%test_result: Recurrence.t]
      (Recurrence.Stable.V1.t_of_sexp (Recurrence.Stable.V1.sexp_of_t r))
      ~expect:r;
    [%test_result: Recurrence.t]
      (Binable.of_string
         (module Recurrence.Stable.V1)
         (Binable.to_string (module Recurrence.Stable.V1) r))
      ~expect:r)
;;
