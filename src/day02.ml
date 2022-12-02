open Core

type choice =
  | Rock
  | Paper
  | Scissors

let choice_of_char = function
  | 'A' | 'X' -> Rock
  | 'B' | 'Y' -> Paper
  | 'C' | 'Z' -> Scissors
  | _ -> failwith "unreachable"

let shape_score = function Rock -> 1 | Paper -> 2 | Scissors -> 3

let outcome_score ~opp ~you =
  match you, opp with
  | Paper, Rock | Rock, Scissors | Scissors, Paper -> 6
  | Paper, Paper | Rock, Rock | Scissors, Scissors -> 3
  | Paper, Scissors | Rock, Paper | Scissors, Rock -> 0

let rec loop acc =
  match In_channel.input_line In_channel.stdin with
  | None -> acc
  | Some line ->
    let opp, you = choice_of_char line.[0], choice_of_char line.[2] in
    loop (acc + shape_score you + outcome_score ~opp ~you)

let () = Printf.printf "%d\n" (loop 0)