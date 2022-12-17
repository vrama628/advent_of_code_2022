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
  let rec travel valve time opened_at last_seen alpha =
    if
      time >= 30
      || Option.equal
           Set.equal
           (Map.find last_seen valve)
           (Some (Map.key_set opened_at))
      || alpha
         > Map.fold valves ~init:0 ~f:(fun ~key ~data acc ->
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
      let log =
        if time <= 10 then
          printf "%s%s\n%!" (String.init time ~f:(Fn.const '\t'))
        else
          ignore
      in
      let max_if_dont_open =
        (Map.find_exn valves valve).tunnels
        |> List.fold ~init:alpha ~f:(fun alpha next_valve ->
               log next_valve;
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
      if Map.mem opened_at valve then
        max_if_dont_open
      else
        max
          (travel
             valve
             (time + 1)
             (Map.add_exn opened_at ~key:valve ~data:(time + 1))
             last_seen
             max_if_dont_open
          )
          max_if_dont_open
  in
  printf
    "%d\n"
    (travel "AA" 0 (Map.empty (module String)) (Map.empty (module String)) 0)
