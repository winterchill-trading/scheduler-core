open! Core

module Make (M : Monad.S) (Job : T) = struct
  type 'a io = 'a M.t
  type job = Job.t

  type t =
    { schedules : Job.t Schedule.t Schedule_id.Table.t
    ; runs : Run_record.t Run_key.Table.t
    }

  let create () =
    { schedules = Schedule_id.Table.create (); runs = Run_key.Table.create () }
  ;;

  let all t =
    Hashtbl.data t.schedules
    |> List.sort ~compare:(fun a b -> Schedule_id.compare (Schedule.id a) (Schedule.id b))
  ;;

  let find t id = M.return (Hashtbl.find t.schedules id)

  let save t s =
    Hashtbl.set t.schedules ~key:(Schedule.id s) ~data:s;
    M.return ()
  ;;

  let active t =
    M.return
      (List.filter (all t) ~f:(fun s -> Schedule.Status.equal (Schedule.status s) Active))
  ;;

  let claim t key ~index ~now ~stale_after : Store.Claim.t io =
    let take attempt =
      Hashtbl.set
        t.runs
        ~key
        ~data:{ key; index; state = Claimed { attempt; claimed_at = now } };
      Store.Claim.Claimed { attempt }
    in
    M.return
      (match Hashtbl.find t.runs key with
       | None -> take 1
       | Some ({ state = Finished _; _ } as r) -> Finished r
       | Some ({ state = Claimed { attempt; _ }; _ } as r) ->
         if Run_record.is_running r ~now ~stale_after
         then Running r
         else take (attempt + 1))
  ;;

  let finish t key ~index outcome ~now =
    let record : Run_record.t =
      match Hashtbl.find t.runs key with
      | Some ({ state = Finished _; _ } as r) -> r
      | Some { state = Claimed { attempt; _ }; _ } ->
        { key
        ; index
        ; state = Finished { outcome; attempts = attempt; finished_at = now }
        }
      | None ->
        { key; index; state = Finished { outcome; attempts = 0; finished_at = now } }
    in
    Hashtbl.set t.runs ~key ~data:record;
    M.return record
  ;;

  let settle_unclaimed t key ~index outcome ~now =
    M.return
      (match Hashtbl.find t.runs key with
       | Some ({ state = Finished _; _ } as r) -> `Finished r
       | Some ({ state = Claimed _; _ } as r) -> `Claimed r
       | None ->
         let r : Run_record.t =
           { key; index; state = Finished { outcome; attempts = 0; finished_at = now } }
         in
         Hashtbl.set t.runs ~key ~data:r;
         `Settled r)
  ;;

  let claimed t =
    Hashtbl.data t.runs
    |> List.filter ~f:(fun (r : Run_record.t) ->
      match r.state with
      | Claimed _ -> true
      | Finished _ -> false)
    |> List.sort ~compare:(fun (a : Run_record.t) b -> Run_key.compare a.key b.key)
    |> M.return
  ;;

  let record t key = M.return (Hashtbl.find t.runs key)

  let records t id =
    Hashtbl.data t.runs
    |> List.filter ~f:(fun (r : Run_record.t) -> Schedule_id.equal r.key.schedule id)
    |> List.sort ~compare:(fun (a : Run_record.t) b -> Run_key.compare a.key b.key)
    |> M.return
  ;;
end
