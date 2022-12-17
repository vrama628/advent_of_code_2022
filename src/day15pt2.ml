open Core

type point = int * int
type sensor = {
  sensor : point;
  beacon : point;
}

let interval_of_sensor row { sensor = sx, sy; beacon = bx, by } =
  let distance_to_beacon = abs (sx - bx) + abs (sy - by) in
  let distance_to_row = abs (sy - row) in
  let radius = distance_to_beacon - distance_to_row in
  if radius >= 0 then
    Some (sx - radius, sx + radius)
  else
    None

let rec covered_until n = function
  | (a, b) :: rest ->
    if n + 1 >= a then
      covered_until (max n b) rest
    else
      Some (n + 1)
  | [] -> Option.some_if (n < 4000000) (n + 1)

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
  let y, x =
    List.find_map (List.range ~stop:`inclusive 0 4000000) ~f:(fun row ->
        sensors
        |> List.filter_map ~f:(interval_of_sensor row)
        |> List.sort ~compare:(fun (a, _) (b, _) -> Int.compare a b)
        |> covered_until 0
        |> Option.map ~f:(fun col -> row, col)
    )
    |> Option.value_exn
  in
  printf "%d\n" ((x * 4000000) + y)
