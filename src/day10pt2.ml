open Core

let draw_cycle cycle x =
  if abs (((cycle - 1) % 40) - x) <= 1 then
    printf "#"
  else
    printf ".";
  if cycle % 40 = 0 then printf "\n"

let rec loop cycle x : unit =
  draw_cycle cycle x;
  match In_channel.input_line In_channel.stdin with
  | None -> ()
  | Some "noop" -> loop (cycle + 1) x
  | Some line ->
    draw_cycle (cycle + 1) x;
    let dx = String.chop_prefix_exn line ~prefix:"addx " |> int_of_string in
    loop (cycle + 2) (x + dx)

let () = loop 1 1
