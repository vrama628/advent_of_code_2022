open Core

type point = int * int
type sensor = {
  sensor : point;
  beacon : point;
}

let positions_of_sensor { sensor = sx, sy; beacon = bx, by } =
  let distance_to_beacon = abs (sx - bx) + abs (sy - by) in
  let distance_to_2000000 = abs (sy - 2000000) in
  let radius = distance_to_beacon - distance_to_2000000 in
  List.range ~stop:`inclusive (sx - radius) (sx + radius)
  |> Set.of_list (module Int)

let () =
  let sensors =
    In_channel.input_lines In_channel.stdin
    |> List.map ~f:(fun line ->
           Scanf.sscanf
             line
             "Sensor at x=%d, y=%d: closest beacon is at x=%d, y=%d"
             (fun sx sy bx by -> { sensor = sx, sy; beacon = bx, by }
           )
       )
  in
  let positions_with_no_beacons =
    Set.diff
      (sensors |> List.map ~f:positions_of_sensor |> Set.union_list (module Int))
      (sensors
      |> List.filter_map ~f:(function
             | { beacon = x, 2000000; _ } -> Some x
             | _ -> None
             )
      |> Set.of_list (module Int)
      )
  in
  printf "%d\n" (Set.length positions_with_no_beacons)
