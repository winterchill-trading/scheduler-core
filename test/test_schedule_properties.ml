open! Core
open Scheduler_core

let id = Schedule_id.create_exn "p"

let gen =
  let open Quickcheck.Generator.Let_syntax in
  let%map r = Gen.recurrence
  and policy = Quickcheck.Generator.of_list [ Schedule.Missed_policy.Latest_only; Catch_up ]
  and settled = Int.gen_incl 0 5
  and days_after = Int.gen_incl (-30) 900 in
  let t = Schedule.create ~missed_policy:policy ~id ~job:() r in
  let t =
    List.fold (List.range 0 settled) ~init:t ~f:(fun t index ->
      Schedule.settle t ~index (Succeeded { reference = "x" }))
  in
  t, Date.add_days (Recurrence.start r) days_after
;;

let sexp_of_case = [%sexp_of: unit Schedule.t * Date.t]

let%test_unit "due dates are contiguous from the next index, none after today" =
  Quickcheck.test gen ~sexp_of:sexp_of_case ~f:(fun (t, today) ->
    let { Schedule.Due.run; missed } = Schedule.due t ~today in
    let all =
      List.sort (run @ missed) ~compare:(fun (a : Schedule.Occurrence.t) b ->
        Int.compare a.index b.index)
    in
    List.iteri all ~f:(fun k o ->
      [%test_result: int] o.index ~expect:(Schedule.next_index t + k);
      assert (Date.( <= ) o.date today));
    (* nothing unsettled on or before today is left out *)
    (match Recurrence.last_index_on_or_before (Schedule.recurrence t) today with
     | Some last when last >= Schedule.next_index t && Schedule.Status.equal (Schedule.status t) Active ->
       [%test_result: int] (List.length all) ~expect:(last - Schedule.next_index t + 1)
     | _ -> [%test_result: int] (List.length all) ~expect:0);
    match Schedule.missed_policy t with
    | Catch_up -> assert (List.is_empty missed)
    | Latest_only ->
      assert (List.length run <= 1);
      List.iter run ~f:(fun r ->
        List.iter missed ~f:(fun m -> assert (m.index < r.index))))
;;

let%test_unit "the schedule pauses exactly when the failure streak reaches the limit" =
  Quickcheck.test
    (Quickcheck.Generator.list_with_length 12 Bool.quickcheck_generator)
    ~sexp_of:[%sexp_of: bool list]
    ~f:(fun outcomes ->
      let r =
        Recurrence.create_exn Weekly ~start:(Date.of_string "2027-01-04") ()
      in
      let final, streak, paused_at =
        List.foldi
          outcomes
          ~init:(Schedule.create ~id ~job:() r, 0, None)
          ~f:(fun index (t, streak, paused_at) ok ->
            match Schedule.status t with
            | Active ->
              let outcome : Outcome.t =
                if ok then Succeeded { reference = "x" } else Failed { reason = "r" }
              in
              let t = Schedule.settle t ~index outcome in
              let streak = if ok then 0 else streak + 1 in
              let paused_at =
                match Schedule.status t, paused_at with
                | Paused _, None -> Some (index, streak)
                | _ -> paused_at
              in
              t, streak, paused_at
            | _ -> t, streak, paused_at)
      in
      (match paused_at with
       | Some (_, s) -> [%test_result: int] s ~expect:3
       | None -> assert (streak < 3));
      [%test_result: bool]
        (Schedule.Status.equal (Schedule.status final) Active)
        ~expect:(Option.is_none paused_at))
;;

let%test_unit "stable round trip" =
  Quickcheck.test gen ~sexp_of:sexp_of_case ~f:(fun (t, _) ->
    let m = (module struct
      type t = unit Schedule.Stable.V1.t [@@deriving bin_io]
    end : Binable.S with type t = unit Schedule.t)
    in
    [%test_result: unit Schedule.t]
      (Binable.of_string m (Binable.to_string m t))
      ~expect:t;
    [%test_result: unit Schedule.t]
      ([%of_sexp: unit Schedule.Stable.V1.t] ([%sexp_of: unit Schedule.Stable.V1.t] t))
      ~expect:t)
;;
