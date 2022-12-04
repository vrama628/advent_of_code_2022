open Core

let pair_of_list = function [a; b] -> a, b | _ -> failwith "unreachable"

let parse_range range =
  range |> String.split ~on:'-' |> List.map ~f:int_of_string |> pair_of_list

let contains (a, b) (c, d) = a <= c && b >= d

let rec loop acc =
  match In_channel.input_line In_channel.stdin with
  | None -> acc
  | Some line ->
    let elf1, elf2 =
      String.split line ~on:',' |> List.map ~f:parse_range |> pair_of_list
    in
    let incr = (contains elf1 elf2 || contains elf2 elf1) |> Bool.to_int in
    loop (acc + incr)

let () = Printf.printf "%d\n" (loop 0)