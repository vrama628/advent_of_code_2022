open Core

let priority char =
  let ascii = Char.to_int char in
  if ascii > 90 then
    ascii - 96
  else
    ascii - 38

let rec loop acc =
  match In_channel.input_line In_channel.stdin with
  | None -> acc
  | Some line ->
    let half_len = String.length line / 2 in
    let left, right =
      String.prefix line half_len, String.suffix line half_len
    in
    let item =
      String.find left ~f:(String.contains right) |> Option.value_exn
    in
    loop (acc + priority item)

let () = Printf.printf "%d\n" (loop 0)