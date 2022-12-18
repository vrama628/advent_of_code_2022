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
  let rec travel
      elven_valve
      phant_valve
      time
      opened_at
      elven_last_seen
      phant_last_seen
      alpha =
    let num_opened = Map.length opened_at in
    if
      time >= 26
      || List.equal String.equal (Map.keys opened_at) (Map.keys valves)
      || Option.equal
           ( = )
           (Map.find elven_last_seen elven_valve)
           (Some num_opened)
      || Option.equal
           ( = )
           (Map.find phant_last_seen phant_valve)
           (Some num_opened)
      || Option.equal
           ( = )
           (Map.find phant_last_seen elven_valve)
           (Some num_opened)
         && Option.equal
              ( = )
              (Map.find elven_last_seen phant_valve)
              (Some num_opened)
      || alpha
         >= Map.fold valves ~init:0 ~f:(fun ~key ~data acc ->
                acc
                + (26
                  - (Map.find opened_at key |> Option.value ~default:(time + 1))
                  )
                  * data.rate
            )
    then
      Map.fold opened_at ~init:0 ~f:(fun ~key ~data acc ->
          acc + ((Map.find_exn valves key).rate * (26 - data))
      )
    else
      let () =
        if time < 10 then
          printf
            "%selven=%s, phant=%s\n%!"
            (String.init time ~f:(Fn.const ' '))
            elven_valve
            phant_valve
      in
      let phant_last_seen =
        Map.set phant_last_seen ~key:phant_valve ~data:(Map.length opened_at)
      in
      let elven_last_seen =
        Map.set elven_last_seen ~key:elven_valve ~data:(Map.length opened_at)
      in
      let elven_moves =
        (Map.find_exn valves elven_valve).tunnels
        |> List.map ~f:(fun tunnel -> tunnel, [])
      in
      let phant_moves =
        (Map.find_exn valves phant_valve).tunnels
        |> List.map ~f:(fun tunnel -> tunnel, [])
      in
      let elven_choices =
        if
          Map.mem opened_at elven_valve
          || (Map.find_exn valves elven_valve).rate = 0
        then
          elven_moves
        else
          (elven_valve, [elven_valve]) :: elven_moves
      in
      let phant_choices =
        if
          Map.mem opened_at phant_valve
          || (Map.find_exn valves phant_valve).rate = 0
        then
          phant_moves
        else
          (phant_valve, [phant_valve]) :: phant_moves
      in
      List.cartesian_product elven_choices phant_choices
      |> List.permute
      |> ( if String.(elven_valve = phant_valve) then
           List.filter
             ~f:(fun ((elven_valve, elven_opens), (phant_valve, phant_opens)) ->
               String.(elven_valve <= phant_valve)
               && (List.is_empty elven_opens || List.is_empty phant_opens)
           )
         else
           Fn.id
         )
      |> List.fold
           ~init:alpha
           ~f:(fun alpha ((elven_valve, elven_opens), (phant_valve, phant_opens))
              ->
             let opened_at =
               List.fold
                 (elven_opens @ phant_opens)
                 ~init:opened_at
                 ~f:(fun opened_at key ->
                   Map.set opened_at ~key ~data:(time + 1)
               )
             in
             max
               alpha
               (travel
                  elven_valve
                  phant_valve
                  (time + 1)
                  opened_at
                  elven_last_seen
                  phant_last_seen
                  alpha
               )
         )
  in
  let empty = Map.empty (module String) in
  printf "%d\n" (travel "AA" "AA" 0 empty empty empty 2265)
