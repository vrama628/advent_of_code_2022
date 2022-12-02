open Core

let insert ~sum (a, b, c) =
  if sum > a then
    sum, b, c
  else if sum > b then
    a, sum, c
  else if sum > c then
    a, b, sum
  else
    a, b, c

let rec loop ~res:(a, b, c) ~sum =
  match In_channel.input_line In_channel.stdin with
  | None -> a + b + c
  | Some line ->
    if String.is_empty line then
      loop ~res:(insert ~sum (a, b, c)) ~sum:0
    else
      loop ~res:(a, b, c) ~sum:(sum + int_of_string line)

let () = Printf.printf "%d\n" (loop ~res:(0, 0, 0) ~sum:0)
