open Core

type valve = {
  tunnels : string list;
  rate : int;
}

module StringSet = Set.Make (String)

module HashableSet = struct
  include StringSet
  include Hashable.Make (struct
    include StringSet
    include StringSet.Provide_hash (String)
  end)
end

module Hashable =
  Tuple.Hashable_t (Tuple.Hashable_t (String) (Int)) (HashableSet)

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
  let travel travel valve time open_valves last_seen =
    if
      time >= 30
      || Option.equal
           StringSet.equal
           (Map.find last_seen valve)
           (Some open_valves)
    then
      0
    else
      let log =
        if time <= 10 then
          printf "%s%s\n%!" (String.init time ~f:(Fn.const '\t'))
        else
          ignore
      in
      let max_if_dont_open =
        List.map (Map.find_exn valves valve).tunnels ~f:(fun next_valve ->
            log next_valve;
            travel
              next_valve
              (time + 1)
              open_valves
              (Map.set last_seen ~key:valve ~data:open_valves)
        )
        |> List.max_elt ~compare
        |> Option.value_exn
      in
      if StringSet.mem open_valves valve then
        max_if_dont_open
      else
        max
          (((Map.find_exn valves valve).rate * (30 - (time + 1)))
          + travel valve (time + 1) (StringSet.add open_valves valve) last_seen
          )
          max_if_dont_open
  in
  let memo_table = Hashable.Table.create () in
  let rec memo_travel valve time open_valves last_seen =
    let key = (valve, time), open_valves in
    match Hashable.Table.find memo_table key with
    | Some res -> res
    | None ->
      let data = travel memo_travel valve time open_valves last_seen in
      Hashable.Table.add_exn memo_table ~key ~data;
      data
  in
  printf "%d\n" (memo_travel "AA" 0 StringSet.empty (Map.empty (module String)))
