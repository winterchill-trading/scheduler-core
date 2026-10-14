open! Core

type t =
  { now : unit -> Time_ns.t
  ; zone : Time_float.Zone.t
  }

let create ~now ~zone = { now; zone }
let wall ~zone = { now = Time_ns.now; zone }
let now t = t.now ()
let zone t = t.zone
let today t = Time_ns.to_date (now t) ~zone:t.zone
let with_zone t zone = { t with zone }

module Manual = struct
  type nonrec t =
    { mutable time : Time_ns.t
    ; zone : Time_float.Zone.t
    }

  let create ~start ~zone = { time = start; zone }
  let clock t = { now = (fun () -> t.time); zone = t.zone }
  let set t time = t.time <- time
  let advance t span = t.time <- Time_ns.add t.time span

  let set_date t date =
    t.time <- Time_ns.of_date_ofday ~zone:t.zone date Time_ns.Ofday.start_of_day
  ;;
end
