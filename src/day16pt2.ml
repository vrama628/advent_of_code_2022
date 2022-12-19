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
  let travel travel (elven_valve, phant_valve, time, opened) =
    if time >= 26 || Set.length opened = Map.length valves then
      0
    else
      let () =
        if time < 10 then
          printf
            "%selven=%s, phant=%s\n%!"
            (String.init time ~f:(Fn.const ' '))
            elven_valve
            phant_valve
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
          Set.mem opened elven_valve
          || (Map.find_exn valves elven_valve).rate = 0
        then
          elven_moves
        else
          (elven_valve, [elven_valve]) :: elven_moves
      in
      let phant_choices =
        if
          Set.mem opened phant_valve
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
           ~init:0
           ~f:(fun acc ((elven_valve, elven_opens), (phant_valve, phant_opens))
              ->
             let opens = elven_opens @ phant_opens in
             let opened = List.fold opens ~init:opened ~f:Set.add in
             let added_now =
               List.sum
                 (module Int)
                 opens
                 ~f:(fun valve ->
                   (26 - (time + 1)) * (Map.find_exn valves valve).rate
                 )
             in
             max
               acc
               (travel (elven_valve, phant_valve, time + 1, opened) + added_now)
         )
  in
  let travel = Memo.recursive ~hashable:Base.Hashable.poly travel in
  printf "%d\n" (travel ("AA", "AA", 0, Set.empty (module String)))
