open Core

let rec loop ~res ~sum =
  match In_channel.input_line In_channel.stdin with
  | None -> res
  | Some line ->
    if String.is_empty line then
      loop ~res:(max res sum) ~sum:0
    else
      loop ~res ~sum:(sum + int_of_string line)

let () = Printf.printf "%d\n" (loop ~res:0 ~sum:0)