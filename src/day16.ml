open Core

type valve = {
  tunnels : string list;
  rate : int;
}

let () =
  let valves =
    In_channel.input_lines In_channel.stdin
    |> List.fold
         ~init:(Map.empty (module String))
         ~f:(fun acc line ->
           Scanf.sscanf
             line
             "Valve %s has flow rate=%d; tunnel%_[s] lead%_[s] to valve%_[s] \
              %[A-Z, ]%!"
             (fun key rate tunnels ->
               let tunnels =
                 List.init
                   ((String.length tunnels / 4) + 1)
                   ~f:(fun i ->
                     String.of_char_list
                       [tunnels.[4 * i]; tunnels.[(4 * i) + 1]]
                   )
               in
               Map.add_exn acc ~key ~data:{ tunnels; rate }
           )
         )
  in
  let rec travel valve time opened_at last_seen alpha =
    if
      time >= 30
      || Option.equal
           Set.equal
           (Map.find last_seen valve)
           (Some (Map.key_set opened_at))
      || alpha
         >= Map.fold valves ~init:0 ~f:(fun ~key ~data acc ->
                acc
                + (30
                  - (Map.find opened_at key |> Option.value ~default:(time + 1))
                  )
                  * data.rate
            )
    then
      Map.fold opened_at ~init:0 ~f:(fun ~key ~data acc ->
          acc + ((Map.find_exn valves key).rate * (30 - data))
      )
    else
      let alpha =
        if Map.mem opened_at valve || (Map.find_exn valves valve).rate = 0 then
          alpha
        else
          max
            (travel
               valve
               (time + 1)
               (Map.add_exn opened_at ~key:valve ~data:(time + 1))
               last_seen
               alpha
            )
            alpha
      in
      (Map.find_exn valves valve).tunnels
      |> List.permute
      |> List.fold ~init:alpha ~f:(fun alpha next_valve ->
             max
               alpha
               (travel
                  next_valve
                  (time + 1)
                  opened_at
                  (Map.set last_seen ~key:valve ~data:(Map.key_set opened_at))
                  alpha
               )
         )
  in
  printf
    "%d\n"
    (travel "AA" 0 (Map.empty (module String)) (Map.empty (module String)) 0)
