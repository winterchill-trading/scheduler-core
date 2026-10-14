(* The README's quick start, built in CI so it stays correct. *)

open! Core
open Scheduler_core

module Pay = struct
  type t = { cents : int } [@@deriving sexp_of]

  let validate _ _ = Ok ()

  (* Pass the run key on as the idempotency key of the payment request. *)
  let run _t (c : Job.Context.t) = Ok ("payment-" ^ Run_key.to_string c.key)
end

module Store = Store_memory.Make (Monad.Ident) (Pay)
module Runner = Runner.Make (Monad.Ident) (Pay) (Store)

let () =
  let clock = Clock.wall ~zone:Time_float.Zone.utc in
  let runner = Runner.create ~store:(Store.create ()) ~clock () in
  let recurrence =
    Recurrence.create_exn
      (Monthly { day_of_month = None })
      ~start:(Date.of_string "2027-01-31")
      ()
  in
  ignore
    (Runner.add runner ~id:(Schedule_id.create_exn "rent") { cents = 900_00 } recurrence);
  (* call from a timer, e.g. every minute *)
  List.iter (Runner.tick runner) ~f:(fun e -> print_s [%sexp (e : Runner.Execution.t)])
;;
