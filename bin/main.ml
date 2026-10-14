open! Core
open Scheduler_core

let frequency_param =
  let open Command.Let_syntax in
  let%map_open name =
    flag
      "frequency"
      (required
         (Arg_type.of_alist_exn
            [ "once", `Once
            ; "weekly", `Weekly
            ; "fortnightly", `Fortnightly
            ; "monthly", `Monthly
            ; "every_n_days", `Every_n_days
            ]))
      ~doc:"F once | weekly | fortnightly | monthly | every_n_days"
  and day = flag "day" (optional int) ~doc:"D day of month for monthly (1-31)"
  and interval = flag "interval" (optional int) ~doc:"N days for every_n_days (1-366)" in
  match name, interval with
  | `Once, _ -> Frequency.Once
  | `Weekly, _ -> Weekly
  | `Fortnightly, _ -> Fortnightly
  | `Monthly, _ -> Monthly { day_of_month = day }
  | `Every_n_days, Some n -> Every_n_days n
  | `Every_n_days, None -> failwith "every_n_days needs -interval"
;;

let command =
  Command.basic
    ~summary:"Print the dates of a recurring schedule"
    ~readme:(fun () ->
      "Dates are computed by index from the start, so a monthly schedule on the 31st\n\
       runs on the last day of shorter months and returns to the 31st after them.\n\
       With -due-on, prints what a schedule that has settled nothing yet owes that day\n\
       when only the latest passed date runs.")
    (let%map_open.Command frequency = frequency_param
     and start = flag "start" (required date) ~doc:"YYYY-MM-DD first possible date"
     and end_date = flag "end" (optional date) ~doc:"YYYY-MM-DD last possible date"
     and max_occurrences = flag "max" (optional int) ~doc:"N at most N dates"
     and count =
       flag "count" (optional_with_default 12 int) ~doc:"N dates to print (default 12)"
     and due_on =
       flag "due-on" (optional date) ~doc:"YYYY-MM-DD show what is due that day"
     in
     fun () ->
       match Recurrence.create frequency ~start ?end_date ?max_occurrences () with
       | Error e ->
         eprint_s [%sexp (e : Recurrence.Invalid.t)];
         exit 2
       | Ok r ->
         Recurrence.dates r
         |> Fn.flip Sequence.take count
         |> Sequence.iter ~f:(fun (i, d) ->
           printf
             "%d\t%s\t%s\n"
             i
             (Date.to_string d)
             (Day_of_week.to_string (Date.day_of_week d)));
         Option.iter due_on ~f:(fun today ->
           let s = Schedule.create ~id:(Schedule_id.create_exn "cli") ~job:() r in
           print_s [%sexp (Schedule.due s ~today : Schedule.Due.t)]))
;;

let () = Command_unix.run command
