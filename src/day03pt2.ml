open Core
open In_channel

let priority char =
  let ascii = Char.to_int char in
  if ascii > 90 then
    ascii - 96
  else
    ascii - 38

let set_of_string string = string |> String.to_list |> Set.of_list (module Char)

let rec loop acc =
  match input_line stdin, input_line stdin, input_line stdin with
  | Some line1, Some line2, Some line3 ->
    let set1, set2, set3 =
      set_of_string line1, set_of_string line2, set_of_string line3
    in
    let item = Set.inter set1 (Set.inter set2 set3) |> Set.choose_exn in
    loop (acc + priority item)
  | _ -> acc

let () = Printf.printf "%d\n" (loop 0)