open Core

let rec loop cycle x acc : int =
  match In_channel.input_line In_channel.stdin with
  | None -> acc
  | Some "noop" ->
    let acc =
      if cycle % 40 = 20 then
        acc + (x * cycle)
      else
        acc
    in
    loop (cycle + 1) x acc
  | Some line ->
    let acc =
      if cycle % 40 = 20 then
        acc + (x * cycle)
      else if (cycle + 1) % 40 = 20 then
        acc + (x * (cycle + 1))
      else
        acc
    in
    let dx = String.chop_prefix_exn line ~prefix:"addx " |> int_of_string in
    loop (cycle + 2) (x + dx) acc

let () = Printf.printf "%d\n" (loop 1 1 0)
