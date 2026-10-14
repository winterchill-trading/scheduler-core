open! Core
module Claim = Store.Claim

module Make
    (M : Monad.S)
    (Store : Store.S with type 'a io := 'a M.t and type job := string) =
struct
  open M.Let_syntax

  let t0 = Time_ns.of_string_with_utc_offset "2027-01-01 09:00:00Z"
  let at minutes = Time_ns.add t0 (Time_ns.Span.of_int_sec (minutes * 60))
  let stale_after = Time_ns.Span.of_int_sec 120
  let id name = Schedule_id.create_exn ("conformance-" ^ name)
  let date = Date.of_string

  let expect name ~got ~want sexp_of =
    if Sexp.equal (sexp_of got) (sexp_of want)
    then Ok ()
    else
      Or_error.error_s
        [%message
          "store conformance"
            name
            ~got:(sexp_of got : Sexp.t)
            ~want:(sexp_of want : Sexp.t)]
  ;;

  let claim_shape = function
    | Claim.Claimed { attempt } -> sprintf "claimed %d" attempt
    | Running _ -> "running"
    | Finished _ -> "finished"
  ;;

  let settle_shape = function
    | `Settled _ -> "settled"
    | `Claimed _ -> "claimed"
    | `Finished _ -> "finished"
  ;;

  let schedules store =
    let r = Recurrence.create_exn Weekly ~start:(date "2027-01-04") () in
    let a = Schedule.create ~id:(id "a") ~job:"job a" r in
    let b = Schedule.create ~id:(id "b") ~job:"job b" r in
    let%bind () = Store.save store a in
    let%bind () = Store.save store b in
    let paused_b =
      match Schedule.pause b with
      | Ok p -> p
      | Error r -> raise_s [%sexp (r : Schedule.Refusal.t)]
    in
    (* saving again replaces: [b] is paused and drops out of [active] *)
    let%bind () = Store.save store paused_b in
    let%bind found = Store.find store (id "a") in
    let%bind missing = Store.find store (id "zz") in
    let%map active = Store.active store in
    let active =
      List.filter_map active ~f:(fun s ->
        let s = Schedule_id.to_string (Schedule.id s) in
        Option.some_if (String.is_prefix s ~prefix:"conformance-") s)
      |> List.sort ~compare:String.compare
    in
    Or_error.combine_errors_unit
      [ expect "find" ~got:found ~want:(Some a) [%sexp_of: string Schedule.t option]
      ; expect "find missing" ~got:missing ~want:None [%sexp_of: string Schedule.t option]
      ; expect "active" ~got:active ~want:[ "conformance-a" ] [%sexp_of: string list]
      ]
  ;;

  let claims store =
    let key = Run_key.create (id "a") (date "2027-01-04") in
    let claim now = Store.claim store key ~index:0 ~now ~stale_after >>| claim_shape in
    let%bind first = claim (at 0) in
    let%bind fresh = claim (at 1) in
    let%bind taken_over = claim (at 5) in
    let%bind open_claims = Store.claimed store in
    let%bind finished =
      Store.finish store key ~index:0 (Succeeded { reference = "r1" }) ~now:(at 6)
    in
    let%bind again =
      Store.finish store key ~index:0 (Failed { reason = "late" }) ~now:(at 7)
    in
    let%bind after = claim (at 30) in
    let%map open_after = Store.claimed store in
    Or_error.combine_errors_unit
      [ expect "first claim" ~got:first ~want:"claimed 1" [%sexp_of: string]
      ; expect "fresh claim" ~got:fresh ~want:"running" [%sexp_of: string]
      ; expect
          "stale claim taken over"
          ~got:taken_over
          ~want:"claimed 2"
          [%sexp_of: string]
      ; expect
          "open claims"
          ~got:(List.map open_claims ~f:(fun r -> r.key))
          ~want:[ key ]
          [%sexp_of: Run_key.t list]
      ; expect
          "finish"
          ~got:finished.state
          ~want:
            (Finished
               { outcome = Succeeded { reference = "r1" }
               ; attempts = 2
               ; finished_at = at 6
               })
          [%sexp_of: Run_record.State.t]
      ; expect "first finish wins" ~got:again ~want:finished [%sexp_of: Run_record.t]
      ; expect "claim after finish" ~got:after ~want:"finished" [%sexp_of: string]
      ; expect "no open claims" ~got:open_after ~want:[] [%sexp_of: Run_record.t list]
      ]
  ;;

  let unclaimed store =
    let key d = Run_key.create (id "a") (date d) in
    let settle d outcome =
      Store.settle_unclaimed store (key d) ~index:1 outcome ~now:(at 40) >>| settle_shape
    in
    let%bind fresh = settle "2027-01-11" Missed in
    let%bind twice = settle "2027-01-11" Skipped in
    let%bind (_ : Claim.t) =
      Store.claim store (key "2027-01-18") ~index:2 ~now:(at 41) ~stale_after
    in
    let%bind on_claim = settle "2027-01-18" Skipped in
    let%bind kept = Store.record store (key "2027-01-11") in
    let%map records = Store.records store (id "a") in
    Or_error.combine_errors_unit
      [ expect "settle unclaimed" ~got:fresh ~want:"settled" [%sexp_of: string]
      ; expect "settle twice" ~got:twice ~want:"finished" [%sexp_of: string]
      ; expect "settle a claimed date" ~got:on_claim ~want:"claimed" [%sexp_of: string]
      ; expect
          "first settlement kept"
          ~got:(Option.bind kept ~f:Run_record.outcome)
          ~want:(Some Missed)
          [%sexp_of: Outcome.t option]
      ; expect
          "records in date order"
          ~got:(List.map records ~f:(fun r -> Date.to_string r.key.due_date))
          ~want:[ "2027-01-04"; "2027-01-11"; "2027-01-18" ]
          [%sexp_of: string list]
      ]
  ;;

  let check store =
    let%bind a = schedules store in
    let%bind b = claims store in
    let%map c = unclaimed store in
    Or_error.combine_errors_unit [ a; b; c ]
  ;;
end
